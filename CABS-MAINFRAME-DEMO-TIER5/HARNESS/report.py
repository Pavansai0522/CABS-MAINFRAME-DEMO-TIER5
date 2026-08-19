"""CABS Tier 5 -- comparison reporting.

Two outputs from the same data:

* a **JSON report**, which is the machine-readable record of the run and is
  what a pipeline gates on;
* a **readable summary**, which is what a human reads before deciding
  whether a cutover can proceed.

The summary is deliberately blunt about three things, because all three are
routinely lost in parity reporting:

1. **Penny-level variance totals**, signed and net, not just counts. Ten
   thousand variances that net to zero and ten thousand that net to minus
   four hundred dollars are entirely different situations.
2. **Which seeded defects were detected and which were missed.** A missed
   defect is a statement about the harness, not about the candidate.
3. **What did not run.** A level that was skipped because its data did not
   exist is not a level that passed.
"""

from __future__ import annotations

import json
from collections import defaultdict
from datetime import datetime, timezone
from decimal import Decimal
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence

from compare import ComparisonResult, LEVELS, LEVEL_NAMES, Variance
from verdict import DIVERGENT, DIVERGENT_BY_DESIGN, MATCH, Attribution

__all__ = ["build_report", "write_json", "render_summary"]

_RULE = "=" * 78
_THIN = "-" * 78


def _is_scaled(variance) -> bool:
    """True for fields that carry decimal places -- money and quantities.

    A difference of 97,270 on CD-VC-ORIG-NPANXX is a different NPA-NXX, not
    ninety-seven thousand dollars. Summing unscaled identifiers into a money
    total is how a parity report ends up quoting a number nobody can defend,
    so identifiers are counted separately and never added to the money.
    """
    scale = variance.context.get("scale")
    if scale is None:
        # L3/L4/L5 aggregates carry no per-field scale; they are money by
        # construction because the contract lists them as money fields.
        return variance.level in ("L3", "L4", "L5")
    try:
        return int(scale) > 0
    except (TypeError, ValueError):
        return False


def _money_totals(attributions: Sequence[Attribution]) -> Dict[str, Any]:
    """Signed and absolute variance totals, by verdict and by field."""
    net_by_verdict: Dict[str, Decimal] = defaultdict(lambda: Decimal(0))
    abs_by_verdict: Dict[str, Decimal] = defaultdict(lambda: Decimal(0))
    net_by_field: Dict[str, Decimal] = defaultdict(lambda: Decimal(0))
    identifier_movements: Dict[str, int] = defaultdict(int)
    counted = 0
    for a in attributions:
        raw = a.variance.delta
        if raw is None:
            continue
        try:
            delta = Decimal(raw)
        except Exception:
            continue
        if not _is_scaled(a.variance):
            identifier_movements[a.variance.field or "-"] += 1
            continue
        counted += 1
        net_by_verdict[a.verdict] += delta
        abs_by_verdict[a.verdict] += abs(delta)
        if a.variance.field:
            net_by_field[a.variance.field] += delta
    return {
        "variances_carrying_a_delta": counted,
        "unscaled_identifier_differences": dict(
            sorted(identifier_movements.items(), key=lambda kv: -kv[1])
        ),
        "note": (
            "Money totals cover fields declaring decimal places only. Differences on "
            "unscaled identifiers (NPA-NXX, LATA, CIC, HHMMSS) are counted separately "
            "because their arithmetic difference is not an amount."
        ),
        "net_by_verdict": {k: str(v) for k, v in sorted(net_by_verdict.items())},
        "absolute_by_verdict": {k: str(v) for k, v in sorted(abs_by_verdict.items())},
        "net_by_field": {
            k: str(v) for k, v in sorted(net_by_field.items(), key=lambda kv: -abs(kv[1]))
        },
        "net_overall": str(sum(net_by_verdict.values(), Decimal(0))),
        "absolute_overall": str(sum(abs_by_verdict.values(), Decimal(0))),
    }


def _group_variances(attributions: Sequence[Attribution]) -> List[Dict[str, Any]]:
    """Collapse variances into (level, kind, layout, field) groups."""
    groups: Dict[str, Dict[str, Any]] = {}
    for a in attributions:
        sig = a.variance.signature()
        entry = groups.setdefault(
            sig,
            {
                "signature": sig,
                "level": a.variance.level,
                "kind": a.variance.kind,
                "layout": a.variance.layout,
                "field": a.variance.field,
                "count": 0,
                "verdicts": defaultdict(int),
                "defect_ids": set(),
                "net_delta": Decimal(0),
                "examples": [],
            },
        )
        entry["count"] += 1
        entry["verdicts"][a.verdict] += 1
        if a.defect_id:
            entry["defect_ids"].add(a.defect_id)
        if a.variance.delta is not None:
            try:
                entry["net_delta"] += Decimal(a.variance.delta)
            except Exception:
                pass
        if len(entry["examples"]) < 5:
            entry["examples"].append(
                {
                    "key": a.variance.key,
                    "legacy": a.variance.legacy,
                    "candidate": a.variance.candidate,
                    "delta": a.variance.delta,
                    "verdict": a.verdict,
                    "defect_id": a.defect_id,
                }
            )
    out = []
    for entry in groups.values():
        entry["verdicts"] = dict(entry["verdicts"])
        entry["defect_ids"] = sorted(entry["defect_ids"])
        entry["net_delta"] = str(entry["net_delta"])
        out.append(entry)
    return sorted(out, key=lambda e: (-e["count"], e["signature"]))


def build_report(
    comparison: ComparisonResult,
    attributions: Sequence[Attribution],
    score: Dict[str, Any],
    run_meta: Dict[str, Any],
) -> Dict[str, Any]:
    """Assemble the JSON report."""
    per_level: Dict[str, Any] = {}
    by_level_attr: Dict[str, List[Attribution]] = defaultdict(list)
    for a in attributions:
        by_level_attr[a.variance.level].append(a)

    for level in LEVELS:
        result = comparison.levels.get(level)
        if result is None:
            continue
        level_attrs = by_level_attr.get(level, [])
        counts = {MATCH: 0, DIVERGENT: 0, DIVERGENT_BY_DESIGN: 0}
        for a in level_attrs:
            counts[a.verdict] = counts.get(a.verdict, 0) + 1
        if result.ran and not level_attrs:
            verdict = MATCH
        elif counts[DIVERGENT]:
            verdict = DIVERGENT
        elif counts[DIVERGENT_BY_DESIGN]:
            verdict = DIVERGENT_BY_DESIGN
        else:
            verdict = MATCH
        per_level[level] = {
            "level": level,
            "name": LEVEL_NAMES[level],
            "ran": result.ran,
            "skipped_reason": result.skipped_reason,
            "verdict": verdict if result.ran else "NOT RUN",
            "records_compared": result.records_compared,
            "fields_compared": result.fields_compared,
            "variance_count": len(result.variances),
            "verdict_counts": counts,
            "stats": result.stats,
        }

    return {
        "report_schema": "cabs.comparison.v1",
        "generated_at_utc": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "run": run_meta,
        "overall_verdict": score["overall_verdict"],
        "score": score,
        "money": _money_totals(attributions),
        "levels": per_level,
        "datasets": comparison.datasets,
        "variance_groups": _group_variances(attributions),
        "variances": [a.to_json() for a in attributions],
    }


def write_json(report: Dict[str, Any], path: Path | str) -> Path:
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(report, indent=1), encoding="utf-8")
    return path


def _fmt(value: Any, width: int) -> str:
    text = "" if value is None else str(value)
    if len(text) > width:
        return text[: width - 1] + "…"
    return text.ljust(width)


def render_summary(report: Dict[str, Any]) -> str:
    """The human-readable summary."""
    out: List[str] = []
    run = report["run"]
    score = report["score"]

    out.append(_RULE)
    out.append("CABS TIER 5 -- BILL-TO-BILL COMPARISON REPORT")
    out.append(_RULE)
    out.append("generated      : %s" % report["generated_at_utc"])
    out.append("legacy         : %s" % run.get("legacy"))
    out.append("candidate      : %s" % run.get("candidate"))
    out.append("contract       : %s" % run.get("contract"))
    out.append("levels run     : %s" % ", ".join(run.get("levels", [])))
    out.append("mode           : %s" % ("BLIND (answer key withheld)" if score["blind"] else "attributed"))
    out.append("")
    out.append("OVERALL VERDICT: %s" % report["overall_verdict"])
    out.append("")

    out.append(_THIN)
    out.append("PER-LEVEL")
    out.append(_THIN)
    out.append(
        "%-4s %-11s %-22s %10s %10s %8s"
        % ("LVL", "NAME", "VERDICT", "RECORDS", "FIELDS", "VARIANCE")
    )
    for level in LEVELS:
        entry = report["levels"].get(level)
        if entry is None:
            continue
        out.append(
            "%-4s %-11s %-22s %10s %10s %8d"
            % (
                entry["level"],
                entry["name"],
                entry["verdict"],
                "{:,}".format(entry["records_compared"]),
                "{:,}".format(entry["fields_compared"]),
                entry["variance_count"],
            )
        )
        if not entry["ran"] and entry["skipped_reason"]:
            out.append("     not run: %s" % entry["skipped_reason"])
    out.append("")

    out.append(_THIN)
    out.append("VERDICT COUNTS (over variances; everything not listed here matched)")
    out.append(_THIN)
    compared_fields = sum(e["fields_compared"] for e in report["levels"].values())
    total_variances = sum(score["counts"].get(v, 0) for v in (DIVERGENT, DIVERGENT_BY_DESIGN))
    out.append("  %-22s %8s" % ("MATCH (fields)", "{:,}".format(max(compared_fields - total_variances, 0))))
    for verdict in (DIVERGENT_BY_DESIGN, DIVERGENT):
        out.append("  %-22s %8d" % (verdict, score["counts"].get(verdict, 0)))
    out.append("")

    money = report["money"]
    out.append(_THIN)
    out.append("VARIANCE TOTALS (penny level, candidate minus legacy)")
    out.append(_THIN)
    out.append("  money/quantity variances   : %s" % "{:,}".format(money["variances_carrying_a_delta"]))
    out.append("  net overall                : %s" % money["net_overall"])
    out.append("  absolute overall           : %s" % money["absolute_overall"])
    for verdict, value in money["net_by_verdict"].items():
        out.append("  net, %-22s: %s" % (verdict, value))
    if money["unscaled_identifier_differences"]:
        out.append("  unscaled identifier differences (not money, counted separately):")
        for field, count in list(money["unscaled_identifier_differences"].items())[:8]:
            out.append("    %-28s %d record(s)" % (field, count))
    if money["net_by_field"]:
        out.append("  largest net movements by field:")
        for field, value in list(money["net_by_field"].items())[:12]:
            out.append("    %-28s %s" % (field, value))
    out.append("")

    out.append(_THIN)
    out.append("WHAT DIVERGED")
    out.append(_THIN)
    groups = report["variance_groups"]
    if not groups:
        out.append("  nothing -- every compared record and field agreed")
    else:
        out.append(
            "%-4s %-34s %-22s %8s %16s"
            % ("LVL", "KIND / FIELD", "VERDICT", "COUNT", "NET DELTA")
        )
        for g in groups[:40]:
            verdicts = "+".join(
                "%s:%d" % (k.replace("DIVERGENT-BY-DESIGN", "BY-DESIGN"), v)
                for k, v in sorted(g["verdicts"].items())
            )
            label = "%s / %s" % (g["kind"], g["field"] or "-")
            out.append(
                "%-4s %-34s %-22s %8d %16s"
                % (g["level"], _fmt(label, 34), _fmt(verdicts, 22), g["count"], g["net_delta"])
            )
            if g["defect_ids"]:
                out.append("     traced to: %s" % ", ".join(g["defect_ids"]))
            example = g["examples"][0]
            out.append(
                "     e.g. key=%s  legacy=%s  candidate=%s"
                % (example["key"], example["legacy"], example["candidate"])
            )
        if len(groups) > 40:
            out.append("  ... and %d further groups (see the JSON report)" % (len(groups) - 40))
    out.append("")

    out.append(_THIN)
    out.append("SEEDED DEFECTS")
    out.append(_THIN)
    if score["blind"]:
        out.append("  withheld -- this was a blind run.")
        out.append("  Re-run verdict attribution against this same comparison output without")
        out.append("  --blind to apply the answer key and score the detection.")
    else:
        out.append("  detection rate : %s" % score["detection_rate"])
        detected = score["seeded_defects_detected"]
        out.append("  detected       : %s" % (", ".join(detected) if detected else "none"))
        for defect_id in detected:
            out.append(
                "    %-4s %d variance(s) traced" % (defect_id, score["variances_by_defect"][defect_id])
            )
        out.append("  missed         : %s" % (", ".join(score["seeded_defects_missed"]) or "none"))
        for entry in score["missed_detail"]:
            out.append(
                "    %-4s %-9s %-34s (%s)"
                % (entry["id"], entry["family"], _fmt(entry["construct"], 34), entry["detectable_by"])
            )
        out.append("")
        out.append("  signed inputs:")
        for signed in score["signed_inputs"]:
            out.append("    %s" % signed["file"])
            out.append("      sha256 %s" % signed["sha256"])
    out.append("")

    out.append(_THIN)
    out.append("HOW TO READ THIS")
    out.append(_THIN)
    out.append("  MATCH               the two sides agree.")
    out.append("  DIVERGENT-BY-DESIGN the variance traces to a known seeded defect in the")
    out.append("                      legacy. Scored positively -- the transform found")
    out.append("                      something real. It needs a business decision about the")
    out.append("                      defect, not a code change to silence the harness.")
    out.append("  DIVERGENT           a variance with nothing behind it. This one blocks.")
    out.append("")
    out.append("  A level that did not run did not pass. A missed seeded defect is a")
    out.append("  statement about the harness, not about the candidate.")
    out.append(_RULE)
    return "\n".join(out)
