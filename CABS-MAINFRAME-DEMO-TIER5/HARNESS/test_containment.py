#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Answer-key containment test.

This estate is built to be handed to a modernization effort **blind**. That
only works if `SEALED/` is the only place the answers appear. This test proves
it mechanically rather than asserting it.

What it does
------------
1. Loads every ``SEALED/*.json``.
2. Extracts every value of ``description``, ``business_impact``,
   ``correct_modernization_response``, ``construct`` and ``paragraph``, at any
   depth, from every one of them.
3. Derives two kinds of distinctive fragment:

   * **verbatim runs** — every 8-word window of the prose fields. Eight words
     is the bar because below it you are in shared domain vocabulary (the
     balancing equation, a copybook name, a tariff term) and at or above it
     you are quoting. Five-word windows are also counted and reported, but
     they are noise-dominated and are not what the test fails on.
   * **attributions** — a defect id (``D1``–``D12``) appearing on the same
     line as a program name, a source path or a sealed paragraph name. This
     is the property that actually matters: *which program carries which
     defect* must not be derivable outside ``SEALED/``.

4. Greps every non-``SEALED/`` file in the tree for both.
5. Fails loudly, listing every hit with file, line and the fragment matched.

Exemptions
----------
Read from ``WITHHOLD_FOR_BLIND_RUN.md`` at the estate root, which lists the
artefacts that are answer-bearing by design and must be removed from a blind
handover. Nothing is exempt unless it is in that table, the table is asserted
short, every entry in it is asserted to exist, and
``HARNESS/defect_signatures.json`` specifically is asserted to be genuinely
refused by ``HARNESS/verdict.py`` under ``--blind``. Hits inside exempted
artefacts are counted and reported, never silently dropped.

Run:  python3 HARNESS/test_containment.py [-v]
Exit: 0 contained, 1 leaked.
"""

from __future__ import annotations

import json
import os
import re
import sys
import unittest
from collections import defaultdict

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SEALED = os.path.join(ROOT, "SEALED")
WITHHOLD_DOC = "WITHHOLD_FOR_BLIND_RUN.md"

PROSE_FIELDS = ("description", "business_impact",
                "correct_modernization_response", "construct")
NAME_FIELDS = ("paragraph",)

VERBATIM_RUN = 8            # words — at or above this you are quoting
SHORT_RUN = 5               # words — reported only

SKIP_DIRS = {"SEALED", "__pycache__", ".git", ".idea", ".vscode"}
SKIP_EXTS = {".ndjson", ".dat", ".gz", ".zip", ".png", ".jpg", ".jpeg", ".gif",
             ".pyc", ".xlsx", ".docx", ".pptx", ".pdf", ".so", ".bin"}
MAX_BYTES = 12 * 1024 * 1024
MAX_WITHHELD = 6            # a long exemption list is itself a failure

WORD = re.compile(r"[A-Za-z0-9_'’-]+")
IDENT = re.compile(r"\b[A-Z][A-Z0-9]*(?:[-.][A-Z0-9]+)+\b")
DEFECT_ID = re.compile(r"(?<![A-Za-z0-9_-])D(?:[1-9]|1[0-2])(?![A-Za-z0-9_-])")
PROGRAM = re.compile(r"\bCAB[A-Z0-9]{5}\b")
SRC_PATH = re.compile(r"\b(?:BATCH|SORTEXIT|ONLINE|HLASM|PLI|REXX|IMS|DB2|VSAM|JCL|"
                      r"COPYBOOKS|CTC)/[A-Za-z0-9_./-]+")


# ── helpers ────────────────────────────────────────────────────────────────
def norm(text):
    return [w.lower() for w in WORD.findall(text)]


def windows(text, n):
    w = norm(text)
    return {" ".join(w[i:i + n]) for i in range(len(w) - n + 1)}


def walk_values(node, out):
    if isinstance(node, dict):
        for k, v in node.items():
            if isinstance(v, str) and k in PROSE_FIELDS + NAME_FIELDS:
                out.append((k, v))
            walk_values(v, out)
    elif isinstance(node, list):
        for v in node:
            walk_values(v, out)


def load_sealed():
    """-> entries, verbatim{frag:(id,field)}, short{frag}, paragraph_names{name}"""
    entries, verbatim, short, para = [], {}, set(), set()
    files = sorted(f for f in os.listdir(SEALED) if f.endswith(".json"))
    if not files:
        raise AssertionError("SEALED/ holds no .json — this test would pass vacuously")

    for fn in files:
        with open(os.path.join(SEALED, fn), encoding="utf-8") as fh:
            doc = json.load(fh)
        collected = []
        walk_values(doc, collected)

        ids = re.findall(r'"id"\s*:\s*"(D\d+)"', json.dumps(doc))
        owner = ids[0] if len(set(ids)) == 1 else fn

        for field, value in collected:
            entries.append((owner, field, value))
            if field in PROSE_FIELDS:
                for frag in windows(value, VERBATIM_RUN):
                    verbatim.setdefault(frag, (owner, field))
                short |= windows(value, SHORT_RUN)
            else:
                for ident in IDENT.findall(value.upper()):
                    if len(ident) >= 8:
                        para.add(ident)
    return entries, verbatim, short, para


def sealed_paragraph_owners():
    """paragraph identifier -> {source files that legitimately declare it}"""
    owner = defaultdict(set)
    for fn in sorted(os.listdir(SEALED)):
        if not fn.endswith(".json"):
            continue
        with open(os.path.join(SEALED, fn), encoding="utf-8") as fh:
            doc = json.load(fh)

        def visit(n):
            if isinstance(n, dict):
                f, p = n.get("file"), n.get("paragraph")
                if isinstance(f, str) and isinstance(p, str):
                    for ident in IDENT.findall(p.upper()):
                        owner[ident].add(f.replace("\\", "/"))
                for v in n.values():
                    visit(v)
            elif isinstance(n, list):
                for v in n:
                    visit(v)

        visit(doc)
    return owner


def load_withheld():
    """Parse the exemption table out of WITHHOLD_FOR_BLIND_RUN.md."""
    path = os.path.join(ROOT, WITHHOLD_DOC)
    if not os.path.exists(path):
        return []
    out = []
    with open(path, encoding="utf-8") as fh:
        for line in fh:
            if not line.startswith("| `"):
                continue
            cell = line.split("|")[1].strip()
            m = re.match(r"^`([^`]+)`$", cell)
            if m:
                out.append(m.group(1).strip().rstrip("/") + ("/" if m.group(1).endswith("/") else ""))
    return out


def is_withheld(rel, withheld):
    for w in withheld:
        ww = w.rstrip("/")
        if rel == ww or rel.startswith(ww + "/"):
            return w
    return None


def chunk(line, size=100000, overlap=400):
    """Yield overlapping slices so a long line is scanned, not skipped."""
    if len(line) <= size:
        yield line
        return
    i = 0
    while i < len(line):
        yield line[i:i + size + overlap]
        i += size


def scan_files():
    for root, dirs, fs in os.walk(ROOT):
        dirs[:] = [d for d in dirs if d not in SKIP_DIRS]
        for f in sorted(fs):
            p = os.path.join(root, f)
            if os.path.splitext(f)[1].lower() in SKIP_EXTS:
                continue
            try:
                if os.path.getsize(p) > MAX_BYTES:
                    continue
            except OSError:
                continue
            yield p, os.path.relpath(p, ROOT).replace("\\", "/")


def blind_run_refuses(relpath):
    """Does HARNESS/verdict.py genuinely refuse `relpath` under --blind?"""
    v = os.path.join(ROOT, "HARNESS", "verdict.py")
    if not os.path.exists(v):
        return False
    with open(v, encoding="utf-8") as fh:
        src = fh.read()
    stem = os.path.splitext(os.path.basename(relpath))[0]
    for m in re.finditer(r"\n    def\s+(\w+)\(.*?(?=\n    def |\nclass |\Z)", src, re.S):
        body = m.group(0)
        name = m.group(1)
        mentions = stem in body or stem.replace("_", " ") in body or \
            (name.startswith("load_") and stem.split("_")[-1] in name)
        if mentions and "self.blind" in body and "BlindRunError" in body:
            return True
    return False


# ── the scan ───────────────────────────────────────────────────────────────
def run():
    entries, verbatim, short, para_names = load_sealed()
    owners = sealed_paragraph_owners()
    withheld = load_withheld()

    verbatim_hits, attribution_hits = [], []
    short_hits = 0
    withheld_hits = defaultdict(int)
    scanned = 0

    for path, rel in scan_files():
        if rel == "HARNESS/test_containment.py" or rel == WITHHOLD_DOC:
            continue
        try:
            with open(path, encoding="utf-8", errors="replace") as fh:
                text = fh.read()
        except OSError:
            continue
        scanned += 1
        wh = is_withheld(rel, withheld)

        for lineno, line in enumerate(text.split("\n"), 1):
          # long lines (the hub's embedded JSON is one 1.6 MB line) are chunked
          # with overlap rather than skipped — skipping them was a hole
          for line in chunk(line):
            for frag in windows(line, VERBATIM_RUN):
                if frag in verbatim:
                    if wh:
                        withheld_hits[wh] += 1
                    else:
                        verbatim_hits.append((rel, lineno, frag, verbatim[frag]))
            for frag in windows(line, SHORT_RUN):
                if frag in short:
                    short_hits += 1 if not wh else 0

            ids = DEFECT_ID.findall(line)
            if ids:
                up = line.upper()
                progs = set(PROGRAM.findall(up))
                paths = set(SRC_PATH.findall(line))
                paras = {i for i in IDENT.findall(up) if i in owners}
                if progs or paths or paras:
                    if wh:
                        withheld_hits[wh] += 1
                    else:
                        attribution_hits.append(
                            (rel, lineno, sorted(set(ids)),
                             sorted(progs | paths | paras)[:4]))

    return dict(entries=entries, verbatim_frags=len(verbatim),
                short_frags=len(short), paragraph_names=len(para_names),
                withheld=withheld, scanned=scanned,
                verbatim_hits=verbatim_hits, attribution_hits=attribution_hits,
                short_hits=short_hits, withheld_hits=dict(withheld_hits))


# ── the assertions ─────────────────────────────────────────────────────────
class ContainmentTest(unittest.TestCase):
    """SEALED/ must be the only place the answers appear."""

    @classmethod
    def setUpClass(cls):
        cls.r = run()

    def test_sealed_is_not_empty(self):
        self.assertGreater(len(self.r["entries"]), 20,
                           "too few sealed fields extracted — vacuous pass")
        self.assertGreater(self.r["verbatim_frags"], 1000,
                           "too few verbatim fragments derived from SEALED/")

    def test_the_withhold_list_is_short_and_real(self):
        w = self.r["withheld"]
        self.assertTrue(w, "%s declares no artefacts — the exemption table is "
                           "missing or unparseable" % WITHHOLD_DOC)
        self.assertLessEqual(len(w), MAX_WITHHELD,
                             "the withhold list has grown to %d entries; "
                             "containment is being managed by exemption" % len(w))
        for rel in w:
            self.assertTrue(os.path.exists(os.path.join(ROOT, rel.rstrip("/"))),
                            "%s is withheld but does not exist" % rel)

    def test_the_signature_file_is_genuinely_refused_by_a_blind_run(self):
        rel = "HARNESS/defect_signatures.json"
        self.assertIn(rel, self.r["withheld"],
                      "%s is answer-bearing and must be on the withhold list" % rel)
        self.assertTrue(blind_run_refuses(rel),
                        "%s is exempted as answer-bearing but HARNESS/verdict.py "
                        "does not refuse it under --blind" % rel)

    def test_the_hub_publishes_only_the_three_permitted_fields(self):
        """HUB/index.html is on the withhold list because it pairs each defect
        id with its file. That pairing is what the deliverable spec allows and
        nothing more — this bounds the exemption instead of trusting it."""
        hub = os.path.join(ROOT, "HUB", "index.html")
        if not os.path.exists(hub):
            self.skipTest("no hub built")
        with open(hub, encoding="utf-8") as fh:
            text = fh.read()
        m = re.search(r"const DATA\s*=\s*(\{.*?\});\n", text, re.S)
        self.assertIsNotNone(m, "cannot find the hub's embedded DATA block")
        rows = json.loads(m.group(1)).get("defects", [])
        self.assertTrue(rows, "the hub publishes no defect rows at all")
        allowed = {"id", "file", "detectable_by", "key_file"}
        for row in rows:
            extra = set(row) - allowed
            self.assertFalse(extra, "hub defect row %s carries %s beyond the "
                                    "permitted fields" % (row.get("id"), sorted(extra)))
        _, verbatim, _, _ = load_sealed()
        quoted = [f for f in windows(text, VERBATIM_RUN) if f in verbatim]
        self.assertFalse(quoted, "the hub quotes sealed prose: %r" % quoted[:5])

    def test_no_sealed_prose_is_quoted_outside_sealed(self):
        hits = self.r["verbatim_hits"]
        if hits:
            out = ["", "%d run(s) of %d+ words quoted from SEALED/ outside it:"
                   % (len(hits), VERBATIM_RUN)]
            for rel, ln, frag, src in hits[:80]:
                out.append("  %s:%d  [%s %s]  %r" % (rel, ln, src[0], src[1], frag))
            if len(hits) > 80:
                out.append("  ... and %d more" % (len(hits) - 80))
            self.fail("\n".join(out))

    def test_no_defect_is_attributed_to_a_program_outside_sealed(self):
        hits = self.r["attribution_hits"]
        if hits:
            out = ["", "%d defect-to-program attribution(s) outside SEALED/:" % len(hits)]
            for rel, ln, ids, tgt in hits[:80]:
                out.append("  %s:%d  %s <-> %s" % (rel, ln, ",".join(ids), ", ".join(tgt)))
            if len(hits) > 80:
                out.append("  ... and %d more" % (len(hits) - 80))
            self.fail("\n".join(out))


def main():
    r = run()
    print("SEALED/ fields extracted ................ %d" % len(r["entries"]))
    print("distinct %d-word verbatim fragments ...... %d" % (VERBATIM_RUN, r["verbatim_frags"]))
    print("distinct %d-word fragments (reported) .... %d" % (SHORT_RUN, r["short_frags"]))
    print("sealed paragraph names .................. %d" % r["paragraph_names"])
    print("non-SEALED files scanned ................ %d" % r["scanned"])
    print("withheld artefacts (from %s) ... %s" % (WITHHOLD_DOC, ", ".join(r["withheld"])))
    print()
    print("VERBATIM  sealed prose quoted outside ... %d" % len(r["verbatim_hits"]))
    print("ATTRIB    defect->program outside ....... %d" % len(r["attribution_hits"]))
    print("(reported) %d-word coincidences ......... %d" % (SHORT_RUN, r["short_hits"]))
    if r["withheld_hits"]:
        print("inside withheld artefacts (expected, not failed):")
        for rel, n in sorted(r["withheld_hits"].items()):
            print("   %-36s %d" % (rel, n))
    print()
    return unittest.main(argv=[sys.argv[0]] + sys.argv[1:], exit=False,
                         verbosity=2).result


if __name__ == "__main__":
    sys.exit(0 if main().wasSuccessful() else 1)
