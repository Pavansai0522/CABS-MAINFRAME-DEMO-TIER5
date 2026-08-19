#!/usr/bin/env python3
"""CABS Tier 5 -- generator orchestrator.

Builds a complete, internally consistent input set for the wholesale access
billing estate: reference masters, daily usage generations, the inbound
settlement feed, generation-level control records, and a run manifest that
records exactly what was produced so the mainframe run can be reconciled
against it.

    python3 generate.py --profile SMOKE --days 2 --seed 20260815 --outdir ./out

Profiles
--------
=========  ===================  ================================
Profile    CDRs per day         Intended use
=========  ===================  ================================
SMOKE          50,000           end-to-end proof, laptop, minutes
DAILY         500,000           the standard cycle
STRESS      2,000,000           volume behaviour, sort spill
TARGET    100,000,000           cloud target side only
=========  ===================  ================================

Determinism
-----------
Output is byte-identical for a given ``--seed``, ``--profile``, ``--days``
and ``--cycle-end``. It does **not** depend on ``--workers``: work is split
into fixed-size shards derived from the record count, and workers only
decide who processes which shard, never how the shards are cut.
"""

from __future__ import annotations

import argparse
import concurrent.futures as futures
import hashlib
import json
import os
import platform
import shutil
import sys
import tempfile
import time
from datetime import date, datetime, timedelta, timezone
from decimal import Decimal
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple

import gen_common as gc
import gen_cdr
import gen_reference
import gen_settlement
from gen_cdr import CycleWindow, UsageMix
from gen_common import DeterministicRandom, FixedRecordWriter, Layout, RecordBuilder
from gen_reference import ReferenceData

TOOL_VERSION = "1.0.0"

#: CDRs per day, per profile.
PROFILES: Dict[str, int] = {
    "SMOKE": 50_000,
    "DAILY": 500_000,
    "STRESS": 2_000_000,
    "TARGET": 100_000_000,
}

#: Records per shard. Fixed, so the split does not depend on worker count.
SHARD_SIZE = 250_000

DEFAULT_COPYBOOKS = Path(__file__).resolve().parent.parent / "COPYBOOKS"

# ---------------------------------------------------------------------------
# Worker-side state (one process, initialised once)
# ---------------------------------------------------------------------------

_W: Dict[str, Any] = {}


def _worker_init(copybook_dir: str, reference_json: str) -> None:
    """Rebuild the layouts and the reference model inside a worker process."""
    _W["layouts"] = gc.load_layouts(copybook_dir, quiet=True)
    _W["reference"] = ReferenceData.from_json(
        json.loads(Path(reference_json).read_text(encoding="utf-8"))
    )


def _shard_task(spec: Dict[str, Any]) -> Dict[str, Any]:
    """Generate one shard of one day's usage into a part file."""
    layouts: Dict[str, Layout] = _W["layouts"]
    reference: ReferenceData = _W["reference"]
    day = date.fromisoformat(spec["day"])
    cycle = CycleWindow(date.fromisoformat(spec["cycle_start"]), date.fromisoformat(spec["cycle_end"]))
    rng = DeterministicRandom(spec["seed"]).substream(spec["stream"])
    mix = UsageMix(**spec["mix"])
    entry, probes = gen_cdr.generate_usage_day(
        path=Path(spec["path"]),
        layout=layouts["CABSCDR"],
        rng=rng,
        reference=reference,
        day=day,
        cycle=cycle,
        record_count=spec["records"],
        mix=mix,
        seq_base=spec["seq_base"],
    )
    entry["shard"] = spec["shard"]
    entry["probes"] = probes
    return entry


# ---------------------------------------------------------------------------
# Shard planning and assembly
# ---------------------------------------------------------------------------


def _plan_shards(records: int, shard_size: int = SHARD_SIZE) -> List[int]:
    """Split a day's record count into fixed-size shards.

    The last shard absorbs the remainder. This split is a pure function of
    ``records``, which is what keeps output independent of ``--workers``.
    """
    if records <= shard_size:
        return [records]
    whole, remainder = divmod(records, shard_size)
    shards = [shard_size] * whole
    if remainder:
        shards.append(remainder)
    return shards


def _assemble(final_path: Path, parts: Sequence[Path], lrecl: int) -> Dict[str, Any]:
    """Concatenate shard part files into the day's dataset and hash it."""
    digest = hashlib.sha256()
    records = 0
    written = 0
    with final_path.open("wb") as out:
        for part in parts:
            with part.open("rb") as fh:
                while True:
                    chunk = fh.read(1 << 20)
                    if not chunk:
                        break
                    out.write(chunk)
                    digest.update(chunk)
                    written += len(chunk)
    for part in parts:
        try:
            part.unlink()
        except OSError:
            # Some mounted filesystems (network shares, synced folders) refuse
            # unlink. The dataset is already assembled and correct; a leftover
            # part file is untidy, not wrong.
            pass
    records, remainder = divmod(written, lrecl)
    if remainder:
        raise RuntimeError(
            "%s is %d bytes, not a whole number of %d-byte records" % (final_path, written, lrecl)
        )
    return {
        "file": final_path.name,
        "path": str(final_path),
        "lrecl": lrecl,
        "recfm": "FB",
        "records": records,
        "bytes": written,
        "sha256": digest.hexdigest(),
    }


def _merge_shard_entries(entries: Sequence[Dict[str, Any]]) -> Dict[str, Any]:
    """Sum the counts and hash totals of a day's shards."""
    merged: Dict[str, Any] = {}
    conditions: Dict[str, int] = {}
    minutes = Decimal("0.00")
    amount = Decimal("0.00000")
    seq = Decimal(0)
    ocn = Decimal(0)
    hash_records = 0
    for e in entries:
        for k, v in e.get("conditions", {}).items():
            conditions[k] = conditions.get(k, 0) + v
        minutes = gc.CABS_CONTEXT.add(minutes, Decimal(e["hash_minutes"]))
        amount = gc.CABS_CONTEXT.add(amount, Decimal(e["hash_amount"]))
        seq = gc.CABS_CONTEXT.add(seq, Decimal(e["hash_seq"]))
        ocn = gc.CABS_CONTEXT.add(ocn, Decimal(e["hash_ocn"]))
        hash_records += e["hash_records"]
    merged["conditions"] = conditions
    merged["hash_records"] = hash_records
    merged["hash_minutes"] = str(minutes)
    merged["hash_amount"] = str(amount)
    merged["hash_seq"] = str(seq)
    merged["hash_ocn"] = str(ocn)
    return merged


# ---------------------------------------------------------------------------
# Generation control records
# ---------------------------------------------------------------------------


def _write_generation_control(
    path: Path,
    layout: Layout,
    run_id: str,
    usage_entries: Sequence[Dict[str, Any]],
    bill_period: int,
) -> Dict[str, Any]:
    """Write one CABSCTL record per generated usage file.

    The generator is a process like any other, so it declares its counts and
    hash totals in the estate's own control format. This gives the first
    process in the chain (CABING01) something to reconcile CT-READ against,
    and gives the harness's L3 an anchor: the balancing equation
    ``CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED + CT-CARRIED-FWD``
    must hold here before it can mean anything downstream.
    """
    lrecl = layout.lrecl
    builder = RecordBuilder(layout, lrecl)
    with FixedRecordWriter(path, lrecl) as writer:
        for step, entry in enumerate(usage_entries, 1):
            b = builder.reset()
            b.set("CT-RUN-ID", run_id)
            b.set("CT-PROCESS-ID", "GENUSAGE")
            b.set("CT-STEP-SEQ", step)
            b.set("CT-CYCLE-YYDDD", entry["cycle_yyddd"])
            b.set("CT-BILL-PERIOD", bill_period)
            b.set("CT-RERUN-NBR", 0)
            b.set("CT-JOBNAME", "CABGEN01")
            b.set("CT-STEPNAME", "STEP%03d" % step)
            b.set("CT-READ", entry["records"])
            b.set("CT-WRITTEN", entry["records"])
            b.set("CT-REJECTED", 0)
            b.set("CT-SUMMARISED", 0)
            b.set("CT-CARRIED-FWD", 0)
            b.set("CT-HASH-MINUTES", Decimal(entry["hash_minutes"]))
            b.set("CT-HASH-AMOUNT", Decimal(entry["hash_amount"]))
            b.set("CT-HASH-SEQ", Decimal(entry["hash_seq"]))
            b.set("CT-HASH-OCN", Decimal(entry["hash_ocn"]))
            b.set("CT-BAL-IND", "B")
            b.set("CT-RC", 0)
            b.set("CT-RESTART-KEY", entry["file"][:26])
            writer.write(b.build())
    result = writer.close()
    result.update(
        {
            "dsn": "TELCABS.CABS.CONTROL",
            "dd": "CTLOUT",
            "copybook": "CABSCTL",
            "process_id": "GENUSAGE",
            "balancing_equation": "CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED + CT-CARRIED-FWD",
        }
    )
    return result


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        prog="generate.py",
        description="Generate the CABS Tier 5 wholesale access billing input estate.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    p.add_argument(
        "--profile",
        choices=sorted(PROFILES),
        default="DAILY",
        help="volume profile: SMOKE 50k, DAILY 500k, STRESS 2M, TARGET 100M CDRs per day",
    )
    p.add_argument("--days", type=int, default=1, help="number of daily generations to produce")
    p.add_argument("--seed", type=int, default=20260815, help="master RNG seed; same seed, same bytes")
    p.add_argument("--outdir", default="./cabs_data", help="output directory")
    p.add_argument(
        "--cycle-end",
        default=None,
        help="last day of the billing cycle, YYYY-MM-DD (default: the last generated day)",
    )
    p.add_argument(
        "--cycle-days", type=int, default=30, help="length of the billing cycle window in days"
    )
    p.add_argument(
        "--records-per-day",
        type=int,
        default=None,
        help="override the profile's record count (for targeted tests)",
    )
    p.add_argument(
        "--workers",
        type=int,
        default=1,
        help="parallel shard workers; does not change the output, only the wall clock",
    )
    p.add_argument("--copybooks", default=str(DEFAULT_COPYBOOKS), help="path to COPYBOOKS/")
    p.add_argument("--no-reference", action="store_true", help="skip the master files")
    p.add_argument("--no-usage", action="store_true", help="skip the usage generations")
    p.add_argument("--no-settlement", action="store_true", help="skip the inbound settlement feed")
    p.add_argument(
        "--settlement-details",
        type=int,
        default=None,
        help="CMDS inbound detail records (default scales with the profile)",
    )
    p.add_argument(
        "--boundary-probes",
        type=int,
        default=None,
        help="exact-band-boundary probe records per day (default scales with the profile)",
    )
    p.add_argument(
        "--divergence",
        default="none",
        help=(
            "apply target-side behaviour differences to the generated usage, producing a "
            "candidate estate for the comparison harness. Comma-separated names, or 'all'. "
            "See gen_divergence.py for the catalogue."
        ),
    )
    p.add_argument("--manifest", default="run_manifest.json", help="manifest filename")
    p.add_argument("--quiet", action="store_true")
    return p


def _log(quiet: bool, message: str) -> None:
    if not quiet:
        print(message, flush=True)


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = build_parser().parse_args(argv)
    started = time.time()

    outdir = Path(args.outdir).resolve()
    outdir.mkdir(parents=True, exist_ok=True)
    reference_dir = outdir / "REFERENCE"
    usage_dir = outdir / "USAGE"
    settle_dir = outdir / "SETTLEMENT"
    control_dir = outdir / "CONTROL"

    records_per_day = args.records_per_day or PROFILES[args.profile]
    if args.days < 1:
        raise SystemExit("--days must be at least 1")

    cycle_end = date.fromisoformat(args.cycle_end) if args.cycle_end else date(2024, 9, 30)
    days = [cycle_end - timedelta(days=args.days - 1 - i) for i in range(args.days)]
    cycle = CycleWindow(cycle_end - timedelta(days=args.cycle_days - 1), cycle_end)
    bill_period = cycle_end.year % 100 * 10000 + cycle_end.month * 100 + 1
    run_id = "G%s%02d" % (gc.date_to_yyddd(cycle_end), args.seed % 100)

    _log(args.quiet, "CABS Tier 5 generator %s" % TOOL_VERSION)
    _log(args.quiet, "  profile          : %s (%s CDRs/day)" % (args.profile, "{:,}".format(records_per_day)))
    _log(args.quiet, "  days             : %d  (%s .. %s)" % (args.days, days[0], days[-1]))
    _log(args.quiet, "  billing cycle    : %s .. %s  period %06d" % (cycle.start, cycle.end, bill_period))
    _log(args.quiet, "  seed             : %d" % args.seed)
    _log(args.quiet, "  outdir           : %s" % outdir)

    layouts = gc.load_layouts(args.copybooks, quiet=args.quiet)
    if "CABSCDR" not in layouts:
        raise SystemExit("could not parse COPYBOOKS/CABSCDR.cpy from %s" % args.copybooks)

    layout_diagnostics = {
        name: lay.diagnostics
        for name, lay in layouts.items()
        if lay.diagnostics and not name.startswith("CABS-")
    }

    rng = DeterministicRandom(args.seed)
    manifest_files: List[Dict[str, Any]] = []

    # ---------------- reference ------------------------------------------
    reference_json = reference_dir / "reference_index.json"
    if args.no_reference and reference_json.exists():
        _log(args.quiet, "\nreference     : reusing %s" % reference_json)
        reference = ReferenceData.from_json(json.loads(reference_json.read_text(encoding="utf-8")))
    else:
        _log(args.quiet, "\nreference     : generating master files")
        t0 = time.time()
        reference, ref_entries = gen_reference.generate_reference(
            outdir=reference_dir,
            layouts=layouts,
            rng=rng.substream("reference"),
            cycle=cycle_end,
            profile=args.profile,
        )
        manifest_files.extend(ref_entries)
        for e in ref_entries:
            if "records" in e:
                _log(args.quiet, "                %-34s %10s records" % (e["file"], "{:,}".format(e["records"])))
        _log(args.quiet, "                %.1fs" % (time.time() - t0))

    # ---------------- usage ----------------------------------------------
    usage_entries: List[Dict[str, Any]] = []
    all_probes: List[Dict[str, Any]] = []
    if not args.no_usage:
        probes_per_day = args.boundary_probes
        if probes_per_day is None:
            probes_per_day = {"SMOKE": 40, "DAILY": 200, "STRESS": 600, "TARGET": 5000}[args.profile]
        mix = UsageMix(boundary_probes=probes_per_day)

        usage_dir.mkdir(parents=True, exist_ok=True)
        # Shard part files are staged outside the output tree, so nothing has
        # to be deleted from a filesystem that may not permit it and no debris
        # is left in the deliverable directory.
        stage_dir = Path(tempfile.mkdtemp(prefix="cabsgen-"))
        tasks: List[Dict[str, Any]] = []
        day_parts: Dict[int, List[Path]] = {}
        for generation, day in enumerate(days, 1):
            final_name = gc.gdg_name("TELCABS.CABS.USAGE.RAW", generation)
            shards = _plan_shards(records_per_day)
            day_parts[generation] = []
            # Probes are spread evenly across a day's shards.
            probes_left = probes_per_day
            for shard_index, shard_records in enumerate(shards):
                part = stage_dir / ("%s.part%04d" % (final_name, shard_index))
                day_parts[generation].append(part)
                share = probes_left // (len(shards) - shard_index)
                probes_left -= share
                tasks.append(
                    {
                        "day": day.isoformat(),
                        "cycle_start": cycle.start.isoformat(),
                        "cycle_end": cycle.end.isoformat(),
                        "seed": args.seed,
                        "stream": "usage/%s/shard%04d" % (day.isoformat(), shard_index),
                        "path": str(part),
                        "records": shard_records,
                        "seq_base": 1 + shard_index * 1_000_000,
                        "shard": shard_index,
                        "generation": generation,
                        "mix": {**mix.__dict__, "boundary_probes": share},
                    }
                )

        _log(
            args.quiet,
            "\nusage         : %d generation(s), %s records each, %d shard(s) total, %d worker(s)"
            % (args.days, "{:,}".format(records_per_day), len(tasks), args.workers),
        )
        t0 = time.time()
        results: List[Dict[str, Any]] = []
        if args.workers > 1 and len(tasks) > 1:
            with futures.ProcessPoolExecutor(
                max_workers=args.workers,
                initializer=_worker_init,
                initargs=(str(args.copybooks), str(reference_json)),
            ) as pool:
                for entry in pool.map(_shard_task, tasks):
                    results.append(entry)
        else:
            _worker_init(str(args.copybooks), str(reference_json))
            for spec in tasks:
                results.append(_shard_task(spec))
                if not args.quiet and len(tasks) > 1:
                    print("                shard %d/%d" % (len(results), len(tasks)), flush=True)

        by_generation: Dict[int, List[Dict[str, Any]]] = {}
        for spec, entry in zip(tasks, results):
            by_generation.setdefault(spec["generation"], []).append(entry)
            all_probes.extend(entry.pop("probes", []))

        for generation, day in enumerate(days, 1):
            shards_for_day = by_generation[generation]
            final_path = usage_dir / gc.gdg_name("TELCABS.CABS.USAGE.RAW", generation)
            entry = _assemble(final_path, day_parts[generation], layouts["CABSCDR"].lrecl)
            entry.update(
                {
                    "dsn": "TELCABS.CABS.USAGE.RAW",
                    "gdg": "TELCABS.CABS.USAGE.RAW(+%d)" % generation,
                    "dd": "RAWIN",
                    "copybook": "CABSCDR",
                    "generation": generation,
                    "cycle_date": day.isoformat(),
                    "cycle_yyddd": gc.date_to_yyddd(day),
                    "shards": len(shards_for_day),
                }
            )
            entry.update(_merge_shard_entries(shards_for_day))
            usage_entries.append(entry)
            manifest_files.append(entry)
            _log(
                args.quiet,
                "                %-38s %12s records  %s" % (entry["file"], "{:,}".format(entry["records"]), entry["sha256"][:16]),
            )

        shutil.rmtree(stage_dir, ignore_errors=True)
        (usage_dir / "boundary_probes.json").write_text(json.dumps(all_probes, indent=1), encoding="utf-8")
        _log(args.quiet, "                %d band-boundary probes  %.1fs" % (len(all_probes), time.time() - t0))

        # ------------ generation control records --------------------------
        control_dir.mkdir(parents=True, exist_ok=True)
        ctl_entry = _write_generation_control(
            control_dir / gc.gdg_name("TELCABS.CABS.CONTROL", 0),
            layouts["CABSCTL"],
            run_id,
            usage_entries,
            bill_period,
        )
        manifest_files.append(ctl_entry)
        _log(args.quiet, "control       : %s (%d records)" % (ctl_entry["file"], ctl_entry["records"]))

    # ---------------- settlement -----------------------------------------
    if not args.no_settlement:
        details = args.settlement_details
        if details is None:
            details = {"SMOKE": 2_000, "DAILY": 20_000, "STRESS": 60_000, "TARGET": 250_000}[args.profile]
        _log(args.quiet, "\nsettlement    : inbound CMDS/RAO exchange and counterparty meet-point")
        t0 = time.time()
        settle_entries = gen_settlement.generate_settlement(
            outdir=settle_dir,
            layouts=layouts,
            rng=rng.substream("settlement"),
            reference=reference,
            settle_period=bill_period,
            exchange_date=cycle_end,
            cmds_details=details,
        )
        manifest_files.extend(settle_entries)
        for e in settle_entries:
            _log(args.quiet, "                %-38s %12s records" % (e["file"], "{:,}".format(e["records"])))
        _log(args.quiet, "                %.1fs" % (time.time() - t0))

    # ---------------- divergence (candidate estate) -----------------------
    divergence_report: Optional[Dict[str, Any]] = None
    if args.divergence and args.divergence.lower() not in ("none", ""):
        import gen_divergence

        _log(args.quiet, "\ndivergence    : applying '%s' to the usage files" % args.divergence)
        divergence_report = gen_divergence.apply_divergence(
            usage_files=[Path(e["path"]) for e in usage_entries],
            layout=layouts["CABSCDR"],
            spec=args.divergence,
            rng=rng.substream("divergence"),
            probes=all_probes,
            cycle_start_yyddd=gc.date_to_yyddd(cycle.start),
        )
        for name, count in divergence_report["applied"].items():
            _log(args.quiet, "                %-28s %8d records changed" % (name, count))
        # Re-hash the changed files so the manifest still describes the bytes.
        for entry in usage_entries:
            path = Path(entry["path"])
            digest = hashlib.sha256(path.read_bytes()).hexdigest()
            entry["sha256_after_divergence"] = digest

    # ---------------- manifest -------------------------------------------
    manifest = {
        "tool": "CABS Tier 5 generator",
        "tool_version": TOOL_VERSION,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "python": platform.python_version(),
        "run_id": run_id,
        "parameters": {
            "profile": args.profile,
            "records_per_day": records_per_day,
            "days": args.days,
            "seed": args.seed,
            "cycle_start": cycle.start.isoformat(),
            "cycle_end": cycle.end.isoformat(),
            "bill_period": bill_period,
            "shard_size": SHARD_SIZE,
            "workers": args.workers,
            "divergence": args.divergence,
            "copybooks": str(args.copybooks),
        },
        "reproducibility": (
            "Byte-identical for the same seed, profile, days and cycle-end. Independent of "
            "--workers: shards are cut by record count, not by worker count."
        ),
        "totals": {
            "files": len(manifest_files),
            "records": sum(e.get("records", 0) for e in manifest_files if isinstance(e.get("records"), int)),
            "bytes": sum(e.get("bytes", 0) for e in manifest_files),
            "usage_records": sum(e["records"] for e in usage_entries),
            "band_boundary_probes": len(all_probes),
        },
        "copybook_diagnostics": layout_diagnostics,
        "files": manifest_files,
    }
    if divergence_report is not None:
        manifest["divergence"] = divergence_report

    manifest_path = outdir / args.manifest
    manifest_path.write_text(json.dumps(manifest, indent=1), encoding="utf-8")

    elapsed = time.time() - started
    _log(args.quiet, "\nmanifest      : %s" % manifest_path)
    _log(
        args.quiet,
        "total         : %s records in %s files, %.1f MB, %.1fs"
        % (
            "{:,}".format(manifest["totals"]["records"]),
            manifest["totals"]["files"],
            manifest["totals"]["bytes"] / (1024 * 1024),
            elapsed,
        ),
    )
    if layout_diagnostics:
        _log(args.quiet, "\ncopybook diagnostics (reported, not corrected):")
        for member, diags in sorted(layout_diagnostics.items()):
            for d in diags:
                _log(args.quiet, "  %-9s %s" % (member, d))
    return 0


if __name__ == "__main__":
    sys.exit(main())
