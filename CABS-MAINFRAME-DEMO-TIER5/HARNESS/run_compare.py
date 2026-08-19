#!/usr/bin/env python3
"""CABS Tier 5 -- bill-to-bill comparison harness, CLI orchestrator.

    python3 run_compare.py --legacy  ../DATA/legacy \
                           --candidate ../DATA/candidate \
                           --contracts contracts/compare_contract.json \
                           --out ../DATA/attributed_run

    # blind: the answer key and the defect signatures are refused
    python3 run_compare.py --legacy L --candidate C --contracts ... --blind

    # a single level
    python3 run_compare.py --legacy L --candidate C --contracts ... --level L3

Pipeline
--------
1. Match the datasets on each side against the contract's patterns.
2. Canonicalise both sides into ``<out>/canonical/{legacy,candidate}/*.ndjson``.
   Both sides go through the *same* normalisation into a form neither of them
   owns. Nothing is compared before this point.
3. Run the requested levels over the canonical files.
4. Classify the variances -- unless ``--blind``, in which case they are all
   DIVERGENT and no attribution is attempted.
5. Write ``comparison_report.json`` and ``comparison_summary.txt``.

Exit codes
----------
``0``  MATCH, or DIVERGENT-BY-DESIGN only
``1``  at least one DIVERGENT variance
``2``  the harness could not run (no matching datasets, unreadable contract)
"""

from __future__ import annotations

import argparse
import fnmatch
import json
import shutil
import sys
import tempfile
import time
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple

_HERE = Path(__file__).resolve().parent
if str(_HERE) not in sys.path:
    sys.path.insert(0, str(_HERE))

import compare as cmp_mod
import report as report_mod
from canonical import CanonicalRecord, DatasetSpec, canonicalise_file, load_layouts, read_ndjson, write_ndjson
from compare import LEVELS, NdjsonIndex, run_levels
from verdict import BlindRunError, VerdictEngine

DEFAULT_CONTRACT = _HERE / "contracts" / "compare_contract.json"
DEFAULT_SIGNATURES = _HERE / "defect_signatures.json"
DEFAULT_SEALED = _HERE.parent / "SEALED"
DEFAULT_COPYBOOKS = _HERE.parent / "COPYBOOKS"


# ---------------------------------------------------------------------------
# Dataset discovery
# ---------------------------------------------------------------------------


def _match_datasets(
    directory: Path, specs: Sequence[DatasetSpec]
) -> Dict[str, List[Path]]:
    """Map each contract dataset to the files that satisfy its pattern."""
    found: Dict[str, List[Path]] = {}
    if not directory.is_dir():
        return found
    all_files = sorted(p for p in directory.rglob("*") if p.is_file())
    for spec in specs:
        matches = [p for p in all_files if fnmatch.fnmatch(p.name, spec.pattern)]
        if matches:
            found[spec.name] = matches
    return found


def _canonicalise_side(
    side: str,
    files: Dict[str, List[Path]],
    specs: Dict[str, DatasetSpec],
    layouts: Dict[str, Any],
    outdir: Path,
    quiet: bool,
) -> Dict[str, Path]:
    """Canonicalise every matched dataset on one side into NDJSON."""
    outdir.mkdir(parents=True, exist_ok=True)
    produced: Dict[str, Path] = {}
    for name, paths in sorted(files.items()):
        spec = specs[name]
        layout = layouts.get(spec.layout)
        if layout is None:
            print("  WARNING: no layout %s for dataset %s" % (spec.layout, name), file=sys.stderr)
            continue
        target = outdir / ("%s.ndjson" % name)
        count = 0
        try:
            with target.open("w", encoding="utf-8") as fh:
                for path in paths:
                    for record in canonicalise_file(path, layout, spec, side):
                        fh.write(json.dumps(record.to_json(), separators=(",", ":"), ensure_ascii=False))
                        fh.write("\n")
                        count += 1
        except ValueError as exc:
            # A dataset that cannot be read against its declared layout is a
            # finding, not a crash. Report it and carry on with the rest.
            print(
                "  ERROR    %-9s %-14s cannot be canonicalised against %s: %s"
                % (side, name, spec.layout, exc),
                file=sys.stderr,
            )
            try:
                target.unlink()
            except OSError:
                pass
            continue
        produced[name] = target
        if not quiet:
            print(
                "  %-9s %-14s %s -> %s (%s records)"
                % (side, name, ",".join(p.name for p in paths[:2]) + ("…" if len(paths) > 2 else ""),
                   target.name, "{:,}".format(count))
            )
    return produced


def _load_records(path: Optional[Path]) -> List[CanonicalRecord]:
    return list(read_ndjson(path)) if path and path.exists() else []


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        prog="run_compare.py",
        description="Staged L1-L5 bill-to-bill comparison with a three-way verdict.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    p.add_argument("--legacy", required=True, help="directory holding the legacy output")
    p.add_argument("--candidate", required=True, help="directory holding the candidate output")
    p.add_argument("--contracts", default=str(DEFAULT_CONTRACT), help="comparison contract JSON")
    p.add_argument(
        "--level",
        action="append",
        choices=list(LEVELS),
        default=None,
        help="run only this level; repeatable. Default: all five",
    )
    p.add_argument(
        "--blind",
        action="store_true",
        help="refuse to load the answer key and the defect signatures; every variance is DIVERGENT",
    )
    p.add_argument("--sealed", default=str(DEFAULT_SEALED), help="directory holding answer_key_*.json")
    p.add_argument("--signatures", default=str(DEFAULT_SIGNATURES), help="defect signature rules")
    p.add_argument("--copybooks", default=str(DEFAULT_COPYBOOKS))
    p.add_argument("--out", default="./compare_out", help="output directory")
    p.add_argument("--cycle-start", default=None, help="override the contract's cycle start (YYDDD)")
    p.add_argument("--cycle-end", default=None, help="override the contract's cycle end (YYDDD)")
    p.add_argument(
        "--probes",
        default=None,
        help="path to boundary_probes.json, used to key attribution on the probe records",
    )
    p.add_argument("--max-reported", type=int, default=None, help="cap on listed variances per level")
    p.add_argument(
        "--work-dir",
        default=None,
        help="where the canonical NDJSON is staged. Defaults to a system temporary "
             "directory, which matters when --out is on a network or synced volume: "
             "the canonical form is several times the size of the EBCDIC input and is "
             "seeked into heavily",
    )
    p.add_argument("--keep-canonical", action="store_true", help="keep the canonical NDJSON files under --out")
    p.add_argument("--quiet", action="store_true")
    return p


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = build_parser().parse_args(argv)
    started = time.time()

    contract_path = Path(args.contracts)
    if not contract_path.is_file():
        print("cannot read contract %s" % contract_path, file=sys.stderr)
        return 2
    contract = json.loads(contract_path.read_text(encoding="utf-8"))

    specs = {d["name"]: DatasetSpec.from_json(d) for d in contract["datasets"]}
    levels = args.level or list(LEVELS)
    max_reported = args.max_reported or contract.get("reporting", {}).get("max_reported_per_level", 2000)

    legacy_dir = Path(args.legacy)
    candidate_dir = Path(args.candidate)
    outdir = Path(args.out)
    outdir.mkdir(parents=True, exist_ok=True)

    if not args.quiet:
        print("CABS Tier 5 bill-to-bill comparison")
        print("  legacy      : %s" % legacy_dir)
        print("  candidate   : %s" % candidate_dir)
        print("  contract    : %s" % contract_path)
        print("  levels      : %s" % ", ".join(levels))
        print("  mode        : %s" % ("BLIND" if args.blind else "attributed"))
        print()

    layouts = load_layouts(args.copybooks)
    legacy_files = _match_datasets(legacy_dir, list(specs.values()))
    candidate_files = _match_datasets(candidate_dir, list(specs.values()))
    common = sorted(set(legacy_files) & set(candidate_files))

    if not common:
        print(
            "no dataset in the contract matched files on BOTH sides.\n"
            "  legacy matched   : %s\n"
            "  candidate matched: %s\n"
            "Check --legacy/--candidate and the contract's file patterns."
            % (sorted(legacy_files) or "nothing", sorted(candidate_files) or "nothing"),
            file=sys.stderr,
        )
        return 2

    if not args.quiet:
        print("canonicalising (both sides into the same form, which neither side owns)")
    canonical_dir = (
        Path(args.work_dir) / "canonical"
        if args.work_dir
        else (outdir / "canonical" if args.keep_canonical else Path(tempfile.mkdtemp(prefix="cabscmp-")))
    )
    legacy_nd = _canonicalise_side("legacy", {k: legacy_files[k] for k in common}, specs, layouts, canonical_dir / "legacy", args.quiet)
    candidate_nd = _canonicalise_side("candidate", {k: candidate_files[k] for k in common}, specs, layouts, canonical_dir / "candidate", args.quiet)
    if not args.quiet:
        print()

    # ---- assemble the level inputs --------------------------------------
    pairs: List[Tuple[DatasetSpec, Path, Path]] = []
    for name in common:
        spec = specs[name]
        if {"L1", "L2"} & set(levels) & set(spec.levels):
            pairs.append((spec, legacy_nd[name], candidate_nd[name]))

    control_input = None
    control_name = next((n for n in common if specs[n].layout == "CABSCTL"), None)
    if control_name:
        control_input = (
            _load_records(legacy_nd.get(control_name)),
            _load_records(candidate_nd.get(control_name)),
            contract.get("l3", {}).get("process_chain", []),
        )

    bill_input: Dict[str, Any] = {}
    header_name = next((n for n in common if specs[n].layout == "CABSBHDR"), None)
    detail_name = next((n for n in common if specs[n].layout == "CABSBILL"), None)
    if header_name or detail_name:
        bill_input = {
            "legacy_headers": _load_records(legacy_nd.get(header_name)) if header_name else [],
            "candidate_headers": _load_records(candidate_nd.get(header_name)) if header_name else [],
            "legacy_details": _load_records(legacy_nd.get(detail_name)) if detail_name else [],
            "candidate_details": _load_records(candidate_nd.get(detail_name)) if detail_name else [],
            "config": contract.get("l4", {}),
        }

    settlement_input: Dict[str, Any] = {}
    settle_name = next((n for n in common if specs[n].layout == "CABSSETL"), None)
    if settle_name:
        settlement_input = {
            "legacy": _load_records(legacy_nd.get(settle_name)),
            "candidate": _load_records(candidate_nd.get(settle_name)),
            "config": contract.get("l5", {}),
        }

    if not args.quiet:
        print("comparing")
    comparison = run_levels(
        levels=levels,
        pairs=pairs,
        control=control_input,
        bill=bill_input or None,
        settlement=settlement_input or None,
        max_reported=max_reported,
    )
    variances = comparison.all_variances()
    if not args.quiet:
        print("  %s variance(s) across %d level(s)" % ("{:,}".format(len(variances)), len(levels)))
        print()

    # ---- verdict ---------------------------------------------------------
    engine = VerdictEngine(blind=args.blind)
    parameters = dict(contract.get("parameters", {}))
    if args.cycle_start:
        parameters["cycle_start"] = args.cycle_start
    if args.cycle_end:
        parameters["cycle_end"] = args.cycle_end
    probe_keys = set()
    if args.probes and Path(args.probes).is_file():
        for probe in json.loads(Path(args.probes).read_text(encoding="utf-8")):
            probe_keys.add("%s|%s|%s" % (probe["ocn"], probe["ban"], probe["seq"]))
    engine.set_substitutions(
        cycle_start=parameters.get("cycle_start"),
        cycle_end=parameters.get("cycle_end"),
        band_boundaries=parameters.get("band_boundaries", []),
        probe_keys=probe_keys,
    )

    if args.blind:
        try:
            engine.load_answer_keys(args.sealed)
        except BlindRunError as exc:
            if not args.quiet:
                print("verdict: %s" % exc)
    else:
        defects = engine.load_answer_keys(args.sealed)
        rules = engine.load_signatures(args.signatures)
        if not args.quiet:
            print("verdict: %d seeded defect(s) and %d signature rule(s) loaded" % (defects, rules))

    attributions = engine.classify(variances)
    score = engine.score(attributions)

    run_meta = {
        "legacy": str(legacy_dir),
        "candidate": str(candidate_dir),
        "contract": str(contract_path),
        "contract_version": contract.get("contract_version"),
        "levels": levels,
        "blind": args.blind,
        "datasets_compared": common,
        "datasets_legacy_only": sorted(set(legacy_files) - set(candidate_files)),
        "datasets_candidate_only": sorted(set(candidate_files) - set(legacy_files)),
        "parameters": parameters,
        "elapsed_seconds": round(time.time() - started, 2),
    }

    report = report_mod.build_report(comparison, attributions, score, run_meta)
    json_path = report_mod.write_json(report, outdir / "comparison_report.json")
    summary = report_mod.render_summary(report)
    summary_path = outdir / "comparison_summary.txt"
    summary_path.write_text(summary + "\n", encoding="utf-8")

    if not args.quiet:
        print()
        print(summary)
        print()
        print("report  : %s" % json_path)
        print("summary : %s" % summary_path)

    if not args.keep_canonical:
        # The canonical files are large. They are kept only on request,
        # because a report that cannot be reproduced from its inputs is not
        # evidence -- and the inputs are the EBCDIC datasets, not these.
        shutil.rmtree(canonical_dir, ignore_errors=True)

    return 1 if score["counts"].get("DIVERGENT", 0) else 0


if __name__ == "__main__":
    sys.exit(main())
