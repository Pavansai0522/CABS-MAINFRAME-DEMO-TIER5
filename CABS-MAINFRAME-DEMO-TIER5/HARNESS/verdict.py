"""CABS Tier 5 -- three-way verdict classification.

A two-way pass/fail comparison is the wrong instrument for a modernization
parity test. It has only one way to describe a difference, so it forces
every real finding into the same bucket as every bug, and the usual response
is to widen a tolerance until the test goes quiet. That is how an estate
acquires a defect it can no longer see: a tolerance introduced to stop a
balancing proof failing, wide enough to absorb the precision mismatch that
was making it fail, left in place for decades.

So there are three verdicts:

``MATCH``
    No variance. The two sides agree.

``DIVERGENT-BY-DESIGN``
    A variance that traces to a known seeded defect in the legacy estate.
    The transform found something real. **Scored positively** -- the
    candidate has surfaced a defect the legacy has been carrying, and the
    right response is a business decision about the defect, not a code
    change to make the harness quiet.

``DIVERGENT``
    Everything else. A variance with nothing behind it. This is the one that
    blocks.

The answer key is a separate signed input
----------------------------------------
``SEALED/answer_key_*.json`` and the signature rules in
``defect_signatures.json`` are loaded by *this* module and by nothing else.
``--blind`` refuses to load either.

That separation is the point. A blind run produces a comparison report with
every variance classified DIVERGENT and no attribution at all. The answer
key is applied afterwards, to the same variance list, and the difference
between the two runs is the score. If classification lived inside the
comparison engine there would be no way to demonstrate that the engine had
not been tuned to the answer.
"""

from __future__ import annotations

import glob
import hashlib
import json
import re
from dataclasses import asdict, dataclass, field as dc_field
from decimal import Decimal, InvalidOperation
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple

from compare import Variance

__all__ = [
    "MATCH",
    "DIVERGENT",
    "DIVERGENT_BY_DESIGN",
    "SeededDefect",
    "SignatureRule",
    "Attribution",
    "VerdictEngine",
    "BlindRunError",
]

MATCH = "MATCH"
DIVERGENT = "DIVERGENT"
DIVERGENT_BY_DESIGN = "DIVERGENT-BY-DESIGN"


class BlindRunError(RuntimeError):
    """Raised when a blind run is asked for the answer key."""


# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------


@dataclass
class SeededDefect:
    """One entry from a sealed answer key."""

    id: str
    family: str
    file: str
    paragraph: str
    construct: str
    description: str
    business_impact: str
    correct_modernization_response: str
    detectable_by: str
    source_file: str = ""

    @classmethod
    def from_json(cls, payload: Dict[str, Any], source_file: str) -> "SeededDefect":
        return cls(
            id=payload["id"],
            family=payload.get("family", ""),
            file=payload.get("file", ""),
            paragraph=payload.get("paragraph", ""),
            construct=payload.get("construct", ""),
            description=payload.get("description", ""),
            business_impact=payload.get("business_impact", ""),
            correct_modernization_response=payload.get("correct_modernization_response", ""),
            detectable_by=payload.get("detectable_by", ""),
            source_file=source_file,
        )

    def summary(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "family": self.family,
            "file": self.file,
            "paragraph": self.paragraph,
            "construct": self.construct,
            "detectable_by": self.detectable_by,
        }


@dataclass
class SignatureRule:
    """A machine-readable predicate that attributes a variance to a defect."""

    defect: str
    name: str
    match: Dict[str, Any]
    evidence: str = ""
    confidence: str = "high"
    note: str = ""

    @classmethod
    def from_json(cls, payload: Dict[str, Any]) -> "SignatureRule":
        return cls(
            defect=payload["defect"],
            name=payload.get("name", ""),
            match=payload.get("match", {}),
            evidence=payload.get("evidence", ""),
            confidence=payload.get("confidence", "high"),
            note=payload.get("note", ""),
        )


@dataclass
class Attribution:
    """The verdict on one variance."""

    verdict: str
    variance: Variance
    defect_id: Optional[str] = None
    rule: Optional[str] = None
    confidence: Optional[str] = None
    evidence: Optional[str] = None

    def to_json(self) -> Dict[str, Any]:
        return {
            "verdict": self.verdict,
            "defect_id": self.defect_id,
            "rule": self.rule,
            "confidence": self.confidence,
            "evidence": self.evidence,
            "variance": self.variance.to_json(),
        }


# ---------------------------------------------------------------------------
# Predicate evaluation
# ---------------------------------------------------------------------------


def _as_decimal(value: Any) -> Optional[Decimal]:
    if value is None:
        return None
    try:
        return Decimal(str(value))
    except (InvalidOperation, ValueError):
        return None


def _test_scalar(condition: Any, value: Any, substitutions: Dict[str, Any]) -> bool:
    """Evaluate one condition against one value.

    Conditions are JSON, so they are auditable and can be reviewed by
    someone who does not read Python::

        "CD-EDIT-STATUS": {"in": ["6", "7", "8", "9"]}
        "delta":          {"ne": "0"}
        "legacy":         {"in_set": "$band_boundaries"}
        "CD-CONN-YYDDD":  {"lt": "$cycle_start"}
    """
    if condition is None:
        return True
    if not isinstance(condition, dict):
        return str(value).strip() == str(condition).strip()

    text = None if value is None else str(value).strip()

    for op, expected in condition.items():
        expected = substitutions.get(expected[1:], expected) if (
            isinstance(expected, str) and expected.startswith("$")
        ) else expected

        if op == "eq":
            if text != str(expected).strip():
                return False
        elif op == "ne":
            if text == str(expected).strip():
                return False
        elif op == "in":
            if text not in [str(e).strip() for e in expected]:
                return False
        elif op == "not_in":
            if text in [str(e).strip() for e in expected]:
                return False
        elif op == "in_set":
            # Compared as Decimal VALUES, not as strings: the canonical form
            # preserves the declared scale, so a band boundary of 250000
            # arrives as "250000.00" and must still match.
            candidates = {Decimal(str(e)) for e in expected}
            dec = _as_decimal(text)
            if dec is None or dec not in candidates:
                return False
        elif op == "present":
            if bool(expected) != (value is not None):
                return False
        elif op == "regex":
            if text is None or not re.search(str(expected), text):
                return False
        elif op in ("lt", "le", "gt", "ge"):
            left, right = _as_decimal(text), _as_decimal(expected)
            if left is None or right is None:
                # Fall back to lexical comparison, which is correct for
                # YYDDD within a century and for fixed-width codes.
                left_s, right_s = text, str(expected).strip()
                if left_s is None:
                    return False
                cmp_ok = {
                    "lt": left_s < right_s, "le": left_s <= right_s,
                    "gt": left_s > right_s, "ge": left_s >= right_s,
                }[op]
            else:
                cmp_ok = {
                    "lt": left < right, "le": left <= right,
                    "gt": left > right, "ge": left >= right,
                }[op]
            if not cmp_ok:
                return False
        elif op == "abs_ge":
            dec = _as_decimal(text)
            if dec is None or abs(dec) < Decimal(str(expected)):
                return False
        elif op == "abs_le":
            dec = _as_decimal(text)
            if dec is None or abs(dec) > Decimal(str(expected)):
                return False
        else:
            raise ValueError("unknown signature operator %r" % op)
    return True


def _matches(rule: SignatureRule, variance: Variance, substitutions: Dict[str, Any]) -> bool:
    m = rule.match
    if "levels" in m and variance.level not in m["levels"]:
        return False
    if "kinds" in m and variance.kind not in m["kinds"]:
        return False
    if "layouts" in m and variance.layout not in m["layouts"]:
        return False
    if "datasets" in m and variance.dataset not in m["datasets"]:
        return False
    if "fields" in m and (variance.field or "") not in m["fields"]:
        return False
    if "field_regex" in m and not re.search(m["field_regex"], variance.field or ""):
        return False
    for attr in ("legacy", "candidate", "delta"):
        if attr in m and not _test_scalar(m[attr], getattr(variance, attr), substitutions):
            return False
    for name, condition in (m.get("witness") or {}).items():
        witness = variance.context.get("witness") or {}
        if not _test_scalar(condition, witness.get(name), substitutions):
            return False
    for name, condition in (m.get("context") or {}).items():
        if not _test_scalar(condition, variance.context.get(name), substitutions):
            return False
    if "key_in" in m:
        key_set = substitutions.get(m["key_in"][1:]) if str(m["key_in"]).startswith("$") else m["key_in"]
        if variance.key not in (key_set or set()):
            return False
    return True


# ---------------------------------------------------------------------------
# The engine
# ---------------------------------------------------------------------------


class VerdictEngine:
    """Classifies variances. Owns the only path to the answer key."""

    def __init__(self, blind: bool = False) -> None:
        self.blind = blind
        self.defects: Dict[str, SeededDefect] = {}
        self.rules: List[SignatureRule] = []
        self.substitutions: Dict[str, Any] = {}
        self.inputs: List[Dict[str, str]] = []

    # -- loading ---------------------------------------------------------
    def load_answer_keys(self, sealed_dir: Path | str, pattern: str = "answer_key_*.json") -> int:
        """Load the sealed answer keys. Refused during a blind run."""
        if self.blind:
            raise BlindRunError(
                "this is a blind run: the answer key was not loaded. Re-run without "
                "--blind, against the same comparison output, to apply it."
            )
        sealed_dir = Path(sealed_dir)
        loaded = 0
        for path in sorted(sealed_dir.glob(pattern)):
            payload = json.loads(path.read_text(encoding="utf-8"))
            entries = payload if isinstance(payload, list) else payload.get("defects", [])
            for entry in entries:
                defect = SeededDefect.from_json(entry, path.name)
                self.defects[defect.id] = defect
                loaded += 1
            self.inputs.append(
                {
                    "file": str(path),
                    "sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
                    "defects": str(len(entries)),
                }
            )
        return loaded

    def load_signatures(self, path: Path | str) -> int:
        """Load the machine-readable attribution rules.

        These are withheld during a blind run too. A signature file names the
        defects and describes their fingerprints; handing it to a blind run
        would leak the answer as surely as the key itself.
        """
        if self.blind:
            raise BlindRunError(
                "this is a blind run: defect signatures were not loaded. The signature "
                "file names the seeded defects and would leak the answer."
            )
        path = Path(path)
        payload = json.loads(path.read_text(encoding="utf-8"))
        self.rules = [SignatureRule.from_json(r) for r in payload.get("rules", [])]
        self.inputs.append(
            {
                "file": str(path),
                "sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
                "rules": str(len(self.rules)),
            }
        )
        return len(self.rules)

    def set_substitutions(self, **values: Any) -> None:
        """Values that signature rules may reference as ``$name``."""
        self.substitutions.update(values)

    # -- classification --------------------------------------------------
    def classify(self, variances: Sequence[Variance]) -> List[Attribution]:
        """Classify every variance. Order of rules is order of precedence."""
        out: List[Attribution] = []
        for variance in variances:
            attribution = Attribution(verdict=DIVERGENT, variance=variance)
            if not self.blind:
                for rule in self.rules:
                    if _matches(rule, variance, self.substitutions):
                        if rule.defect not in self.defects:
                            # A signature naming a defect the answer key does
                            # not contain is a broken input, not a match.
                            continue
                        attribution = Attribution(
                            verdict=DIVERGENT_BY_DESIGN,
                            variance=variance,
                            defect_id=rule.defect,
                            rule=rule.name,
                            confidence=rule.confidence,
                            evidence=rule.evidence,
                        )
                        break
            out.append(attribution)
        return out

    def score(self, attributions: Sequence[Attribution]) -> Dict[str, Any]:
        """Verdict counts, and which seeded defects were detected vs missed."""
        counts = {MATCH: 0, DIVERGENT: 0, DIVERGENT_BY_DESIGN: 0}
        by_defect: Dict[str, int] = {}
        for a in attributions:
            counts[a.verdict] = counts.get(a.verdict, 0) + 1
            if a.defect_id:
                by_defect[a.defect_id] = by_defect.get(a.defect_id, 0) + 1

        overall = MATCH
        if counts[DIVERGENT]:
            overall = DIVERGENT
        elif counts[DIVERGENT_BY_DESIGN]:
            overall = DIVERGENT_BY_DESIGN

        detected = sorted(by_defect)
        missed = sorted(set(self.defects) - set(by_defect))
        return {
            "blind": self.blind,
            "overall_verdict": overall,
            "counts": counts,
            "variances_by_defect": dict(sorted(by_defect.items())),
            "seeded_defects_total": len(self.defects),
            "seeded_defects_detected": detected,
            "seeded_defects_missed": missed,
            "detection_rate": (
                "%d/%d" % (len(detected), len(self.defects)) if self.defects else "n/a (blind)"
            ),
            "missed_detail": [
                self.defects[d].summary() for d in missed
            ],
            "signed_inputs": self.inputs,
            "note": (
                "DIVERGENT-BY-DESIGN is scored positively: the candidate surfaced a defect "
                "the legacy estate has been carrying. It is not a pass -- it is a finding "
                "that needs a business decision, not a code change."
            ),
        }
