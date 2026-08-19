#!/usr/bin/env python3
"""
build_hub.py — generate HUB/index.html for the CABS Tier 5 reference estate.

Everything in the hub is DERIVED from the estate, never invented:

  * estate inventory .............. walked from the file system (files + lines per folder)
  * runnable / reference split .... README.md "Runnable versus reference-only" table
  * knowledge graph ............... parsed from .cbl / .jcl / .prc / .ctl / .cpy source
  * process contracts ............. CONTRACTS/contracts.json (247 processes)
  * 27 complexities ............... CONTRACTS/complexity_placement.json
  * CAST findings / blind spots ... DOCS/CAST_IMAGING_GUIDE.md sections 2 and 3
  * seeded defects ................ SEALED/answer_key_*.json — id / file / detectable_by
                                    ONLY. The construct statement, the description, the
                                    business impact and the correct modernization response
                                    are NEVER read into the output.

Run:  python3 build_hub.py            (from HUB/, or anywhere — paths are resolved from __file__)
"""

from __future__ import annotations

import html
import json
import os
import re
import sys
from collections import Counter, defaultdict, OrderedDict
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
HUB = ROOT / "HUB"
OUT = HUB / "index.html"

# fields we are allowed to lift out of SEALED/ — anything else leaks the answer key.
# "construct" is NOT allowed: in these answer keys that field is the defect
# statement written out in plain English, so publishing it publishes the answer.
ANSWER_KEY_ALLOWED = ("id", "file", "detectable_by")

SKIP_DIRS = {"__pycache__", ".git", "_scratch_delete_me", "HUB"}

TECH_BY_EXT = {
    ".cbl": "COBOL", ".cpy": "Copybook", ".jcl": "JCL", ".prc": "JCL PROC",
    ".ctl": "Sort control card", ".asm": "HLASM", ".pli": "PL/I", ".exec": "REXX",
    ".bms": "BMS map", ".dbd": "IMS DBD", ".psb": "IMS PSB", ".ddl": "DB2 DDL",
    ".sql": "SQL", ".py": "Python (harness/generators)", ".md": "Documentation",
    ".json": "JSON metadata", ".yaml": "YAML metadata", ".dat": "Test data",
    ".ndjson": "Test data", ".txt": "Documentation", ".xlsx": "Workbook",
    ".docx": "Document", ".part0000": "Test data",
}

# ─────────────────────────────────────────────────────────────────────────────
#  helpers
# ─────────────────────────────────────────────────────────────────────────────

def read(p: Path) -> str:
    try:
        return p.read_text(encoding="utf-8", errors="replace")
    except Exception:
        return ""


def count_lines(p: Path) -> int:
    try:
        with p.open("rb") as fh:
            return sum(1 for _ in fh)
    except Exception:
        return 0


def walk_files():
    for dirpath, dirnames, filenames in os.walk(ROOT):
        dirnames[:] = sorted(d for d in dirnames if d not in SKIP_DIRS and not d.startswith("."))
        rel_dir = Path(dirpath).relative_to(ROOT)
        for fn in sorted(filenames):
            if fn.startswith("."):
                continue
            p = Path(dirpath) / fn
            if p.suffix in (".pyc",):
                continue
            yield p, (rel_dir.as_posix() if rel_dir.as_posix() != "." else "(root)")


# ─────────────────────────────────────────────────────────────────────────────
#  1. estate inventory
# ─────────────────────────────────────────────────────────────────────────────

def scan_estate():
    folders = defaultdict(lambda: {"files": 0, "lines": 0, "tech": Counter()})
    tech = defaultdict(lambda: {"files": 0, "lines": 0})
    total_files = total_lines = 0
    for p, rel_dir in walk_files():
        ln = count_lines(p)
        t = TECH_BY_EXT.get(p.suffix.lower(), "Other")
        top = rel_dir.split("/")[0] if rel_dir != "(root)" else "(root)"
        f = folders[rel_dir]
        f["files"] += 1
        f["lines"] += ln
        f["tech"][t] += 1
        tech[t]["files"] += 1
        tech[t]["lines"] += ln
        total_files += 1
        total_lines += ln
    # roll up to top-level folder
    tops = defaultdict(lambda: {"files": 0, "lines": 0})
    for rel_dir, v in folders.items():
        top = rel_dir.split("/")[0] if rel_dir != "(root)" else "(root)"
        tops[top]["files"] += v["files"]
        tops[top]["lines"] += v["lines"]
    return {
        "total_files": total_files,
        "total_lines": total_lines,
        "by_folder": [
            {"folder": k, "files": v["files"], "lines": v["lines"]}
            for k, v in sorted(folders.items(), key=lambda kv: -kv[1]["lines"])
        ],
        "by_top": [
            {"folder": k, "files": v["files"], "lines": v["lines"]}
            for k, v in sorted(tops.items(), key=lambda kv: -kv[1]["lines"])
        ],
        "by_tech": [
            {"tech": k, "files": v["files"], "lines": v["lines"]}
            for k, v in sorted(tech.items(), key=lambda kv: -kv[1]["lines"])
        ],
    }


# ─────────────────────────────────────────────────────────────────────────────
#  2. README — runnable vs reference-only table, and the coupling table
# ─────────────────────────────────────────────────────────────────────────────

def parse_readme():
    txt = read(ROOT / "README.md")
    runnable = []
    # the table under "## Runnable versus reference-only"
    sec = _section(txt, "## Runnable versus reference-only", "### Three things")
    for row in _table_rows(sec):
        if len(row) < 5:
            continue
        folder, files, lines, status, why = row[0], row[1], row[2], row[3], row[4]
        if folder.lower().startswith("folder"):
            continue
        runnable.append({
            "folder": _clean_md(folder),
            "files": _clean_md(files),
            "lines": _clean_md(lines),
            "status": _clean_md(status),
            "why": _clean_md(why),
            "runnable": "RUNNABLE" in status.upper(),
        })
    # cross-application coupling table
    sec2 = _section(txt, "### Why this is *the* modernization blocker", "## The 27 complexities")
    sec2b = _section(txt, "## The two applications, and the coupling that blocks everything",
                     "### Why this is *the* modernization blocker")
    coupling = []
    for row in _table_rows(sec2b):
        if len(row) >= 4 and row[0].strip().startswith("`"):
            coupling.append({
                "program": _clean_md(row[0]), "app": _clean_md(row[1]),
                "reads": _clean_md(row[2]), "owned_by": _clean_md(row[3]),
            })
    blocker = _paras(sec2)[:4]
    # 27-complexity plain-english table
    plain = {}
    sec3 = _section(txt, "## The 27 complexities", "**Traceability.**")
    for row in _table_rows(sec3):
        if len(row) >= 3 and row[0].strip().isdigit():
            plain[int(row[0].strip())] = _clean_md(row[2])
    # "what this is" opening statement
    what = _paras(_section(txt, "## What this is, and why it exists", "## The domain"))
    domain = _paras(_section(txt, "## The domain — wholesale carrier access billing", "## Repository layout"))
    return {
        "runnable_table": runnable,
        "coupling": coupling,
        "blocker": blocker,
        "plain_english": plain,
        "what_it_is": what[:3],
        "domain": domain[:2],
    }


def _section(txt: str, start: str, end: str) -> str:
    i = txt.find(start)
    if i < 0:
        return ""
    j = txt.find(end, i + len(start))
    return txt[i:j if j > 0 else len(txt)]


def _table_rows(sec: str):
    for line in sec.splitlines():
        s = line.strip()
        if not s.startswith("|") or set(s) <= set("|-: "):
            continue
        cells = [c.strip() for c in s.strip("|").split("|")]
        yield cells


def _clean_md(s: str) -> str:
    s = re.sub(r"\*\*(.+?)\*\*", r"\1", s)
    s = re.sub(r"\*(.+?)\*", r"\1", s)
    s = s.replace("`", "")
    return s.strip()


def _paras(sec: str):
    body = sec.split("\n", 1)[1] if "\n" in sec else ""
    out, cur = [], []
    for line in body.splitlines():
        if line.strip().startswith(("|", "#", ">", "```", "-", "*")) and not cur:
            continue
        if not line.strip():
            if cur:
                out.append(_clean_md(" ".join(cur)))
                cur = []
            continue
        if line.strip().startswith(("|", "#", "```")):
            if cur:
                out.append(_clean_md(" ".join(cur)))
                cur = []
            continue
        cur.append(line.strip())
    if cur:
        out.append(_clean_md(" ".join(cur)))
    return [p for p in out if len(p) > 40]


# ─────────────────────────────────────────────────────────────────────────────
#  3. knowledge graph — parsed from source
# ─────────────────────────────────────────────────────────────────────────────

RE_PROGID   = re.compile(r"^\s*PROGRAM-ID\.\s+([A-Z0-9#$@-]+)", re.M)
RE_COPY     = re.compile(r"(?<![A-Z0-9$#@-])COPY\s+([A-Z0-9#$@-]+)")
RE_CALL_LIT = re.compile(r"(?<![A-Z0-9$#@-])CALL\s+'([A-Z0-9#$@-]+)'")
RE_CALL_DYN = re.compile(r"(?<![A-Z0-9$#@-])CALL\s+([A-Z][A-Z0-9-]{2,})\s+(?:USING|RETURNING|$)", re.M)
RE_DSN      = re.compile(r"DSN=([A-Z0-9$#@&]+(?:\.[A-Z0-9$#@&]+)*(?:\([^)]*\))?)")
RE_GDG      = re.compile(r"\((\+?-?\d+)\)$")
RE_MEMBER   = re.compile(r"\(([A-Z0-9#$@]+)\)$")
RE_MODS     = re.compile(r"E(15|35)\s*=\s*\(\s*([A-Z0-9#$@]+)")

COBOL_DIRS = ("BATCH", "SORTEXIT", "CTC", "ONLINE")


def is_comment_cobol(line: str) -> bool:
    return len(line) > 6 and line[6] == "*"


def strip_cobol_comments(txt: str) -> str:
    return "\n".join(l for l in txt.splitlines() if not is_comment_cobol(l))


RE_SYMGEN = re.compile(r"\((&[A-Z0-9#$@]+)\)$")


def norm_dsn(raw: str):
    """Return (canonical_dsn, kind, member_or_gen). kind in dataset|member."""
    m = RE_GDG.search(raw)
    if m:
        return raw[: m.start()], "dataset", m.group(1)
    m = RE_SYMGEN.search(raw)
    if m:
        # (&INGDG) — a generation selected by a symbolic supplied at submission
        return raw[: m.start()], "dataset", m.group(1)
    m = RE_MEMBER.search(raw)
    if m:
        return raw[: m.start()], "member", m.group(1)
    return raw, "dataset", None


def app_of_dsn(dsn: str) -> str:
    parts = dsn.split(".")
    if len(parts) > 1 and parts[0] == "TELCABS":
        if parts[1] in ("CABS", "SETL"):
            return parts[1]
    return "SHARED"


def app_of_path(rel: str) -> str:
    """Application ownership of a source member, by the estate's own convention."""
    stem = Path(rel).stem.upper()
    if stem.startswith(("CABSET", "CABS2", "CABCTL05")):
        return "SETL"
    if rel.startswith("BATCH/SETTLE"):
        return "SETL"
    return "CABS"


class Graph:
    def __init__(self):
        self.nodes = OrderedDict()
        self.edges = []
        self._seen = set()

    def node(self, nid, **kw):
        if nid not in self.nodes:
            self.nodes[nid] = {"id": nid, "label": kw.pop("label", nid), **kw}
        else:
            for k, v in kw.items():
                if v not in (None, "", []) and not self.nodes[nid].get(k):
                    self.nodes[nid][k] = v
        return self.nodes[nid]

    def edge(self, s, t, rel, **kw):
        if s == t:
            return
        key = (s, t, rel)
        if key in self._seen:
            return
        self._seen.add(key)
        self.edges.append({"s": s, "t": t, "r": rel, **kw})


def build_graph(contracts):
    g = Graph()
    g.node("APP:CABS", label="CABS", type="application", app="CABS",
           note="Wholesale carrier access billing. Owns TELCABS.CABS.*")
    g.node("APP:SETL", label="SETL", type="application", app="SETL",
           note="Settlement. Owns TELCABS.SETL.*")

    prog_files = {}          # PROGRAM-ID -> rel path
    copybooks = set()

    # ---- copybooks -----------------------------------------------------
    for p in sorted((ROOT / "COPYBOOKS").glob("*.cpy")):
        nm = p.stem.upper()
        copybooks.add(nm)
        txt = read(p)
        g.node(f"CPY:{nm}", label=nm, type="copybook", app="SHARED",
               file=f"COPYBOOKS/{p.name}", lines=len(txt.splitlines()))
    for sub in ("ONLINE", "IMS", "DB2"):
        d = ROOT / sub
        if d.is_dir():
            for p in sorted(d.rglob("*.cpy")):
                nm = p.stem.upper()
                copybooks.add(nm)
                g.node(f"CPY:{nm}", label=nm, type="copybook", app="CABS",
                       file=p.relative_to(ROOT).as_posix(), lines=count_lines(p))

    # ---- programs (COBOL / PL/I / HLASM / REXX) -------------------------
    def add_program(p: Path, ntype: str, lang: str):
        rel = p.relative_to(ROOT).as_posix()
        txt = read(p)
        pid = None
        m = RE_PROGID.search(txt)
        if m:
            pid = m.group(1).upper()
        pid = pid or p.stem.upper()
        nid = f"PGM:{pid}"
        prog_files[pid] = rel
        g.node(nid, label=pid)
        # a member we have the source for wins over a stub created by an earlier CALL literal
        g.nodes[nid].update({"type": ntype, "lang": lang, "file": rel,
                             "lines": len(txt.splitlines()), "app": app_of_path(rel),
                             "folder": str(Path(rel).parent), "resolved": True})
        if lang == "COBOL":
            body = strip_cobol_comments(txt)
            for cm in RE_COPY.finditer(body):
                mem = cm.group(1).upper()
                g.node(f"CPY:{mem}", label=mem, type="copybook",
                       app="SHARED", file=("COPYBOOKS/%s.cpy" % mem) if mem in copybooks else "not in estate",
                       resolved=mem in copybooks)
                g.edge(nid, f"CPY:{mem}", "COPY", kind="copy")
            for cm in RE_CALL_LIT.finditer(body):
                tgt = cm.group(1).upper()
                g.node(f"PGM:{tgt}", label=tgt, type="program", lang="COBOL", app=app_of_path(rel))
                g.edge(nid, f"PGM:{tgt}", "CALL", kind="call_static")
            for cm in RE_CALL_DYN.finditer(body):
                ident = cm.group(1).upper()
                if ident in ("USING", "RETURNING", "END-CALL"):
                    continue
                dn = f"DYN:{ident}"
                g.node(dn, label=ident, type="dynamic_target", app=app_of_path(rel),
                       note="Dynamic CALL target — the module name is built in WORKING-STORAGE at run "
                            "time. Static analysis cannot resolve it.")
                g.edge(nid, dn, "CALL (dynamic)", kind="call_dynamic", unresolved=True)
        return pid

    for sub in COBOL_DIRS:
        d = ROOT / sub
        if d.is_dir():
            for p in sorted(d.rglob("*.cbl")):
                rel = p.relative_to(ROOT).as_posix()
                ntype = "sort_exit" if rel.startswith("SORTEXIT/") else (
                        "cics_program" if rel.startswith("ONLINE/") else "program")
                add_program(p, ntype, "COBOL")
    for p in sorted((ROOT / "PLI").glob("*.pli")):
        add_program(p, "pli_program", "PL/I")
    for p in sorted((ROOT / "HLASM").rglob("*.asm")):
        add_program(p, "hlasm_program", "HLASM")
    for p in sorted((ROOT / "REXX").glob("*.exec")):
        add_program(p, "rexx_exec", "REXX")

    # ---- sort control cards --------------------------------------------
    for p in sorted((ROOT / "JCL" / "CTLCARDS").glob("*.ctl")):
        nm = p.stem.upper()
        txt = read(p)
        rules = [l.strip() for l in txt.splitlines()
                 if l.strip() and not l.lstrip().startswith("*")]
        nid = f"CTL:{nm}"
        g.node(nid, label=nm, type="control_card", app="CABS" if nm.startswith("CABS") else "SHARED",
               file=p.relative_to(ROOT).as_posix(), lines=len(txt.splitlines()),
               rules=rules[:12])
        for mm in RE_MODS.finditer(txt.upper()):
            exit_name = mm.group(2)
            enid = f"PGM:{exit_name}"
            exists = exit_name in prog_files
            g.node(enid, label=exit_name, type="sort_exit", app="CABS",
                   file=prog_files.get(exit_name, "no source in the estate"),
                   resolved=exists)
            g.edge(nid, enid, "MODS=E%s" % mm.group(1), kind="sort_exit", unresolved=not exists)

    # ---- JCL jobs and PROCs --------------------------------------------
    def parse_jcl(p: Path, is_proc: bool):
        rel = p.relative_to(ROOT).as_posix()
        nm = p.stem.upper()
        nid = ("PRC:" if is_proc else "JOB:") + nm
        txt = read(p)
        g.node(nid, label=nm, type="proc" if is_proc else "job",
               app=app_of_path(rel), file=rel, lines=len(txt.splitlines()))
        # join continuations
        stmts, cur = [], None
        for line in txt.splitlines():
            if line.startswith("//*") or not line.startswith("//"):
                continue
            m = re.match(r"^//(\S*)\s+(EXEC|DD|PROC|JOB|PEND|SET|INCLUDE)\s*(.*)$", line)
            if m:
                if cur:
                    stmts.append(cur)
                cur = {"name": m.group(1), "op": m.group(2), "body": m.group(3)}
            elif cur is not None and re.match(r"^//\s+\S", line):
                cur["body"] += "," + line[2:].strip().lstrip(",")
        if cur:
            stmts.append(cur)

        owner = nid          # what the DDs attach to
        step = None
        for st in stmts:
            body = st["body"].upper()
            if st["op"] == "EXEC":
                step = st["name"] or step
                bstr = body.strip()
                mfirst = re.match(r"([A-Z0-9#$@&]+)", bstr)
                first = mfirst.group(1) if mfirst else None
                pgm_raw = re.match(r"PGM=([A-Z0-9#$@&]+)", bstr)
                if pgm_raw:
                    tgt = pgm_raw.group(1)
                    if tgt.startswith("&"):
                        # the program that runs is supplied at submission — complexity 9 / 13
                        owner = f"SYM:{nm}.{tgt}"
                        g.node(owner, label=nm + " " + tgt, type="symbolic_program",
                               app=app_of_path(rel), file=rel, resolved=False,
                               note="EXEC PGM=" + tgt + " in " + rel + " — the program name is a "
                                    "symbolic parameter resolved by the scheduler at submission. "
                                    "No JCL library member records the value it actually ran with.")
                        g.edge(nid, owner, "EXEC PGM=" + tgt, kind="job_program",
                               step=step, unresolved=True)
                    elif tgt in ("SORT", "ICEMAN", "SYNCSORT", "IDCAMS", "IEBGENER",
                                 "IEFBR14", "IEBCOPY", "IKJEFT01", "DFSRRC00", "IEBUPDTE",
                                 "IKJEFT1B", "IEBPTPCH", "ADRDSSU"):
                        owner = f"UTL:{tgt}"
                        g.node(owner, label=tgt, type="utility", app="SHARED",
                               note="z/OS utility program.")
                        g.edge(nid, owner, "EXEC PGM", kind="job_program", step=step)
                    else:
                        owner = f"PGM:{tgt}"
                        g.node(owner, label=tgt, type="program", lang="COBOL",
                               app=app_of_path(rel), file=prog_files.get(tgt, "no source in the estate"),
                               resolved=tgt in prog_files)
                        g.edge(nid, owner, "EXEC PGM", kind="job_program", step=step)
                elif first:
                    # PROC invocation; the program may be supplied as a symbolic value
                    pnid = f"PRC:{first}"
                    g.node(pnid, label=first, type="proc", app=app_of_path(rel))
                    g.edge(nid, pnid, "EXEC PROC", kind="proc_include", step=step)
                    owner = pnid
                    ov = re.search(r"\b(?:PGMNAME|PGM|PROGRAM)=([A-Z0-9#$@]+)", body)
                    if ov:
                        tgt = ov.group(1)
                        g.node(f"PGM:{tgt}", label=tgt, type="program", lang="COBOL",
                               app=app_of_path(rel), file=prog_files.get(tgt, "no source in the estate"),
                               resolved=tgt in prog_files)
                        g.edge(pnid, f"PGM:{tgt}", "symbolic override", kind="proc_override",
                               step=step, unresolved=True)
                        owner = f"PGM:{tgt}"
            elif st["op"] == "DD":
                ddname = st["name"].split(".")[-1] or "DD"
                if ddname in ("STEPLIB", "JOBLIB", "SYSLIB", "SYSPROC", "SYSEXEC"):
                    # library search order is itself a construct (complexity 12) — model it,
                    # but as its own node type so it can be filtered out of the main view
                    for seq, dm in enumerate(RE_DSN.finditer(body), 1):
                        lib = re.sub(r"\([^)]*\)$", "", dm.group(1))
                        g.node(f"LIB:{lib}", label=lib, type="library", app="SHARED",
                               note="Load/copy library. Concatenation order decides which copy of a "
                                    "module is picked up at load time — complexity 12.")
                        g.edge(nid, f"LIB:{lib}", "%s #%d" % (ddname, seq), kind="steplib",
                               step=step, seq=seq)
                    continue
                for dm in RE_DSN.finditer(body):
                    raw = dm.group(1)
                    dsn, kind, extra = norm_dsn(raw)
                    if kind == "member" and "CTLCARD" in dsn:
                        g.edge(owner if owner.startswith(("PGM:", "UTL:")) else nid,
                               f"CTL:{extra}", "SYSIN control card", kind="control_card", step=step)
                        continue
                    if kind == "member" and ("LOADLIB" in dsn or "PROCLIB" in dsn or "COPYLIB" in dsn):
                        continue
                    if re.search(r"LOADLIB|PROCLIB|COPYLIB|SYSOUT|COB2LIB|LINKLIB|SORTLIB|SDSNLOAD",
                                 dsn) or dsn.startswith(("SYS1.", "DSN810.", "CEE.", "ISP.")):
                        continue
                    dnid = f"DS:{dsn}"
                    g.node(dnid, label=dsn.replace("TELCABS.", ""), type="dataset",
                           app=app_of_dsn(dsn), dsn=dsn, gdg=bool(extra and re.match(r"^[+-]?\d+$", extra or "")))
                    disp = re.search(r"DISP=\(?\s*([A-Z]+)", body)
                    dv = disp.group(1) if disp else "SHR"
                    write = dv in ("NEW", "MOD")
                    src = owner
                    if write:
                        g.edge(src, dnid, "writes " + ddname, kind="writes", dd=ddname, step=step)
                    else:
                        g.edge(dnid, src, "read by " + ddname, kind="reads", dd=ddname, step=step)
        return nid

    for p in sorted((ROOT / "JCL").glob("*.jcl")):
        parse_jcl(p, False)
    for p in sorted((ROOT / "JCL" / "PROCS").iterdir()):
        if p.suffix in (".prc", ".jcl"):
            parse_jcl(p, True)
    for p in sorted((ROOT / "VSAM").glob("*.jcl")):
        parse_jcl(p, False)

    # ---- unresolved static CALL targets --------------------------------
    try:
        appx = json.loads(read(ROOT / "CONTRACTS" / "complexity_placement.json")).get("appendices", {})
        dyn = (appx.get("interfaces") or {}).get("dynamic_call_targets_rating") or {}
        cand = "; ".join(dyn.get("targets", []))
    except Exception:
        cand = ""
    for nid, n in g.nodes.items():
        if n["type"] in ("program", "sort_exit", "cics_program", "pli_program",
                         "hlasm_program", "rexx_exec") and not n.get("file"):
            n["file"] = "no source anywhere in the estate"
            n["resolved"] = False
            n["note"] = ("Called by name but never defined here — a stub has to be written before "
                         "anything runs.")
        if n["type"] == "dynamic_target" and cand:
            n["note"] = (n.get("note", "") + " Candidate targets recorded in "
                         "CONTRACTS/complexity_placement.json appendices: " + cand)

    # ---- application membership ---------------------------------------
    for n in g.nodes.values():
        a = n.get("app")
        if a in ("CABS", "SETL") and n["type"] in (
                "program", "sort_exit", "cics_program", "pli_program", "hlasm_program",
                "rexx_exec", "job", "proc", "dataset", "control_card", "symbolic_program"):
            g.edge(f"APP:{a}", n["id"], "contains", kind="membership")

    # ---- cross-application edges ---------------------------------------
    for e in g.edges:
        sa = g.nodes[e["s"]].get("app")
        ta = g.nodes[e["t"]].get("app")
        if e["kind"] == "membership":
            continue
        if sa in ("CABS", "SETL") and ta in ("CABS", "SETL") and sa != ta:
            e["cross_app"] = True

    # degree
    deg = Counter()
    for e in g.edges:
        if e["kind"] == "membership":
            continue
        deg[e["s"]] += 1
        deg[e["t"]] += 1
    for nid, n in g.nodes.items():
        n["deg"] = deg.get(nid, 0)

    return g


# ─────────────────────────────────────────────────────────────────────────────
#  4. process contracts
# ─────────────────────────────────────────────────────────────────────────────

LIST_CAP = 20          # produced_by / read_by lists are truncated for page weight


def _cap(v):
    if isinstance(v, list) and len(v) > LIST_CAP:
        return v[:LIST_CAP] + ["… and %d more (see CONTRACTS/contracts.json)" % (len(v) - LIST_CAP)]
    return v


def load_contracts():
    d = json.loads(read(ROOT / "CONTRACTS" / "contracts.json"))
    procs = []
    und_by_proc = Counter()
    for it in d.get("undetermined_items", []):
        und_by_proc[it.get("process_id")] += 1
    for p in d["processes"]:
        q = dict(p)
        for side in ("inputs", "outputs"):
            rows = []
            for r in q.get(side) or []:
                rr = dict(r)
                for k in ("produced_by", "read_by"):
                    if k in rr:
                        rr[k] = _cap(rr[k])
                rows.append(rr)
            q[side] = rows
        q["undetermined_count"] = und_by_proc.get(p["process_id"], 0)
        # count UNDETERMINED literals actually on the record
        q["undetermined_fields"] = _count_undetermined(p)
        procs.append(q)
    return {
        "meta": d["meta"],
        "undetermined_summary": d["undetermined_summary"],
        "global_gaps": d["global_gaps"],
        "processes": procs,
        "undetermined_total": len(d.get("undetermined_items", [])),
        "undetermined_items_sample": d.get("undetermined_items", [])[:400],
    }


def _count_undetermined(obj) -> int:
    n = 0
    if isinstance(obj, dict):
        for v in obj.values():
            n += _count_undetermined(v)
    elif isinstance(obj, list):
        for v in obj:
            n += _count_undetermined(v)
    elif isinstance(obj, str) and obj.strip() == "UNDETERMINED":
        n += 1
    return n


# ─────────────────────────────────────────────────────────────────────────────
#  5. 27 complexities
# ─────────────────────────────────────────────────────────────────────────────

def load_complexities(plain_english):
    d = json.loads(read(ROOT / "CONTRACTS" / "complexity_placement.json"))
    out = []
    for c in d["constructs"]:
        out.append({
            "id": c["id"],
            "name": c["name"],
            "plain_english": c.get("plain_english") or plain_english.get(c["id"], ""),
            "required_count": c.get("required_count"),
            "required_basis": c.get("required_basis", ""),
            "as_built_count": c.get("as_built_count"),
            "as_built_unit": c.get("as_built_unit", ""),
            "status": c.get("status", ""),
            "placements": [
                {
                    "file": pl.get("file", ""),
                    "program": pl.get("program", ""),
                    "paragraph": pl.get("paragraph", ""),
                    "note": pl.get("note", ""),
                    "verification": pl.get("verification", ""),
                }
                for pl in (c.get("placements") or [])
            ],
        })
    return {
        "constructs": out,
        "below_requirement": d.get("constructs_below_requirement", []),
        "not_a_construct": d.get("not_a_construct_but_a_finding", []),
        "verification_method": d.get("verification_method", {}),
        # The per-defect placement block was moved into SEALED/defect_placements.json.
        # complexity_placement.json now carries a count and a pointer only, so there is
        # nothing here for the hub to publish and this list is expected to be empty.
        "seeded_defect_placements": [
            {"id": p.get("id"), "file": p.get("file")}
            for p in (d.get("seeded_defects") or [])
            if isinstance(p, dict) and p.get("id")
        ],
        "seeded_defect_note": d.get("seeded_defect_note", ""),
    }


# ─────────────────────────────────────────────────────────────────────────────
#  6. seeded defects — SEALED/ read under strict field restriction
# ─────────────────────────────────────────────────────────────────────────────

def load_defects(placements):
    rows = []
    leaked = []
    for f in sorted((ROOT / "SEALED").glob("answer_key_*.json")):
        for e in json.loads(read(f)):
            row = {k: e.get(k) for k in ANSWER_KEY_ALLOWED}
            row["key_file"] = "SEALED/" + f.name
            rows.append(row)
    # self-check: no forbidden field may appear anywhere in the emitted rows
    forbidden = ("description", "business_impact", "correct_modernization_response",
                 "paragraph", "construct", "construct_no")
    for r in rows:
        for k in r:
            if k in forbidden:
                leaked.append((r["id"], k))
    rows.sort(key=lambda r: int(re.sub(r"\D", "", r["id"]) or 0))
    return rows, leaked


# ─────────────────────────────────────────────────────────────────────────────
#  7. CAST guide — expected findings (§2) and blind spots (§3)
# ─────────────────────────────────────────────────────────────────────────────

def parse_cast_guide():
    txt = read(ROOT / "DOCS" / "CAST_IMAGING_GUIDE.md")
    lines = txt.splitlines()
    # index of ### headings
    heads = [(i, l) for i, l in enumerate(lines) if l.startswith("### ") or l.startswith("## ")]

    def block(idx):
        start = heads[idx][0] + 1
        end = heads[idx + 1][0] if idx + 1 < len(heads) else len(lines)
        return "\n".join(lines[start:end])

    expected_table = []
    expected_sections = []
    misses = []
    for i, (ln, h) in enumerate(heads):
        title = h.lstrip("#").strip()
        body = block(i)
        if h.startswith("### 2.8"):
            for row in _table_rows(body):
                if len(row) >= 2 and not row[0].lower().startswith("finding"):
                    expected_table.append({"finding": _clean_md(row[0]), "count": _clean_md(row[1])})
        elif re.match(r"### 2\.\d", h):
            expected_sections.append({
                "title": title,
                "lead": (_paras(("#\n" + body)) or [""])[0][:520],
                "bullets": _bold_leads(body)[:4],
            })
        elif re.match(r"### 3\.\d", h):
            misses.append({
                "title": title,
                "lead": (_paras(("#\n" + body)) or [""])[0][:600],
                "bullets": _bold_leads(body)[:4],
                "quote": _first_quote(body),
            })
    return {"expected_table": expected_table, "expected_sections": expected_sections,
            "misses": misses}


def _bold_leads(body: str):
    out = []
    for m in re.finditer(r"\*\*(.{25,220}?)\*\*", body, re.S):
        s = _clean_md(" ".join(m.group(1).split()))
        if s and s not in out:
            out.append(s)
    return out


def _first_quote(body: str):
    q = [l.strip("> ").strip() for l in body.splitlines() if l.strip().startswith(">")]
    if not q:
        return ""
    return _clean_md(" ".join(q))[:500]


# ─────────────────────────────────────────────────────────────────────────────
#  8. dataflow lineage — derived from the contracts' family in/out datasets
# ─────────────────────────────────────────────────────────────────────────────

STAGES = [
    ("INGEST",   "Usage ingest",       "Read raw CDRs, edit, validate, split, suspend and recycle."),
    ("RATING",   "Rating",             "Load the rate table, dispatch the element raters, band, override, build bill detail."),
    ("JURIS",    "Jurisdiction / PIU-PLU", "Determine interstate/intrastate, apply PIU and PLU factors, restate."),
    ("SETTLE",   "Settlement",         "Meet-point split, reciprocal compensation, CMDS/RAO exchange, net per counterparty."),
    ("BILLCALC", "Bill calculation",   "Assemble the account, apply adjustments, tax, prove the invoice balances."),
    ("FORMAT",   "Formatting",         "Number, paginate and render the invoice print and EDI streams."),
    ("REPORT",   "Reporting",          "Control-total reporting, ageing, revenue assurance, close."),
]


def build_lineage(contracts):
    procs = contracts["processes"]
    by_fam = defaultdict(list)
    for p in procs:
        by_fam[p["family"]].append(p)

    def dsns(p, side):
        out = []
        for r in p.get(side) or []:
            d = r.get("dsn")
            if isinstance(d, str) and d.startswith("TELCABS."):
                out.append(re.sub(r"\([^)]*\)$", "", d))
        return out

    fam_out, fam_in = defaultdict(Counter), defaultdict(Counter)
    for fam, ps in by_fam.items():
        for p in ps:
            for d in dsns(p, "outputs"):
                fam_out[fam][d] += 1
            for d in dsns(p, "inputs"):
                fam_in[fam][d] += 1

    stages = []
    for code, name, desc in STAGES:
        ps = by_fam.get(code, [])
        stages.append({
            "code": code, "name": name, "desc": desc,
            "processes": len(ps),
            "programs": len({p["process_id"].split("@")[0] for p in ps}),
            "app": (Counter(p["application"] for p in ps).most_common(1) or [("CABS", 0)])[0][0],
            "outputs": [d for d, _ in fam_out[code].most_common(8)],
            "inputs": [d for d, _ in fam_in[code].most_common(8)],
        })

    flows = []
    codes = [s[0] for s in STAGES]
    for i, a in enumerate(codes):
        for b in codes:
            if a == b:
                continue
            shared = sorted(set(fam_out[a]) & set(fam_in[b]))
            if shared:
                flows.append({"from": a, "to": b, "datasets": shared[:6],
                              "count": len(shared),
                              "cross_app": (a == "SETTLE") != (b == "SETTLE")})
    return {"stages": stages, "flows": flows}


# ─────────────────────────────────────────────────────────────────────────────
#  9. render
# ─────────────────────────────────────────────────────────────────────────────

def js_json(obj) -> str:
    s = json.dumps(obj, ensure_ascii=False, separators=(",", ":"))
    return s.replace("<", "\\u003c").replace(">", "\\u003e").replace("&", "\\u0026")


def main():
    print("→ scanning estate …")
    estate = scan_estate()
    print("   %d files, %d lines" % (estate["total_files"], estate["total_lines"]))

    print("→ reading README.md …")
    readme = parse_readme()

    print("→ loading process contracts …")
    contracts = load_contracts()
    print("   %d processes, %d UNDETERMINED items"
          % (len(contracts["processes"]), contracts["undetermined_total"]))

    print("→ loading complexity placement …")
    cx = load_complexities(readme["plain_english"])
    n_place = sum(len(c["placements"]) for c in cx["constructs"])
    print("   %d constructs, %d placements" % (len(cx["constructs"]), n_place))

    print("→ reading SEALED/ under field restriction %s …" % (ANSWER_KEY_ALLOWED,))
    defects, leaked = load_defects(cx["seeded_defect_placements"])
    if leaked:
        sys.exit("ABORT — answer-key leak: %s" % leaked)
    print("   %d defects, 0 leaked fields" % len(defects))

    print("→ parsing DOCS/CAST_IMAGING_GUIDE.md …")
    cast = parse_cast_guide()
    print("   %d expected-finding rows, %d blind-spot sections"
          % (len(cast["expected_table"]), len(cast["misses"])))

    print("→ building knowledge graph from source …")
    g = build_graph(contracts)
    nodes = list(g.nodes.values())
    edges = [e for e in g.edges]
    real_edges = [e for e in edges if e["kind"] != "membership"]
    print("   %d nodes, %d edges (%d excluding application membership)"
          % (len(nodes), len(edges), len(real_edges)))

    print("→ building lineage …")
    lineage = build_lineage(contracts)

    manifests = []
    for p in sorted(ROOT.rglob("_MANIFEST.md")):
        txt = read(p)
        first = ""
        for line in txt.splitlines():
            s = line.strip()
            if s and not s.startswith("#"):
                first = _clean_md(s)
                break
        manifests.append({
            "path": p.relative_to(ROOT).as_posix(),
            "lines": len(txt.splitlines()),
            "title": next((l.lstrip("# ").strip() for l in txt.splitlines() if l.startswith("# ")),
                          p.parent.name),
            "lead": first[:300],
        })

    payload = {
        "meta": {
            "asset": contracts["meta"].get("asset", ""),
            "generated": contracts["meta"].get("generated", ""),
            "principle": contracts["meta"].get("principle", ""),
            "honesty_rule": contracts["meta"].get("honesty_rule", ""),
            "authored_from": contracts["meta"].get("authored_from", ""),
            "balancing_equation": contracts["meta"].get("balancing_equation", ""),
            "counts": contracts["meta"].get("counts", {}),
        },
        "estate": estate,
        "readme": readme,
        "contracts": contracts,
        "complexity": cx,
        "defects": defects,
        "cast": cast,
        "lineage": lineage,
        "manifests": manifests,
        "graph": {"nodes": nodes, "edges": edges},
        "graph_stats": {"nodes": len(nodes), "edges": len(edges),
                        "edges_excl_membership": len(real_edges)},
    }

    print("→ rendering %s …" % OUT)
    html_doc = TEMPLATE.replace("/*__PAYLOAD__*/null", js_json(payload))
    OUT.write_text(html_doc, encoding="utf-8")
    size = OUT.stat().st_size
    print("   %s — %.2f MB" % (OUT, size / 1048576))
    return payload


TEMPLATE = r"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8" />
<meta name="viewport" content="width=device-width, initial-scale=1.0" />
<title>CABS Tier 5 — Reference Estate Documentation Hub</title>
<script src="https://cdnjs.cloudflare.com/ajax/libs/d3/7.8.5/d3.min.js"></script>
<style>
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
:root{
  --bg:#f7f8fa; --surface:#ffffff; --surface2:#eef1f6; --border:#d4dae6;
  --text:#2a3242; --text-dim:#69748c; --text-hi:#0d1420; --accent:#2c5fd4;
  --ok:#10b981; --warn:#f59e0b; --bad:#ef4444; --info:#06b6d4;
  --c-program:#1d4ed8; --c-copybook:#047857; --c-dataset:#6d28d9; --c-job:#b45309;
  --c-proc:#c2410c; --c-control_card:#a16207; --c-sort_exit:#b91c1c;
  --c-application:#475569; --c-dynamic_target:#be185d; --c-cics_program:#0e7490;
  --c-pli_program:#4d7c0f; --c-hlasm_program:#7e22ce; --c-rexx_exec:#0f766e;
  --c-utility:#475569;
  --sidebar-w:238px; --header-h:54px;
}
html,body{height:100%}
body{font-family:'Segoe UI',system-ui,-apple-system,sans-serif;background:var(--bg);
  color:var(--text);overflow:hidden;font-size:12.5px;line-height:1.5}
b,strong{color:var(--text-hi);font-weight:600}
a{color:var(--accent)}
::-webkit-scrollbar{width:8px;height:8px}
::-webkit-scrollbar-thumb{background:#c2cad9;border-radius:4px}
::-webkit-scrollbar-track{background:transparent}
:focus-visible{outline:2px solid var(--accent);outline-offset:2px}

/* header */
#header{position:fixed;top:0;left:0;right:0;height:var(--header-h);
  background:rgba(255,255,255,.97);border-bottom:1px solid var(--border);
  display:flex;align-items:center;gap:16px;padding:0 18px;z-index:200}
#header .brand h1{font-size:13px;font-weight:700;color:var(--text-hi);letter-spacing:.02em}
#header .brand p{font-size:10.5px;color:var(--text-dim);margin-top:2px}
#header .divider{width:1px;height:26px;background:var(--border)}
.hchips{display:flex;gap:6px;flex-wrap:wrap}
.hchip{font-size:10px;color:var(--text-dim);background:var(--surface2);
  border:1px solid var(--border);border-radius:5px;padding:3px 8px;white-space:nowrap}
.hchip b{color:var(--text-hi);font-size:11px}
.search-wrap{margin-left:auto;position:relative;display:flex;align-items:center}
.search-wrap svg{position:absolute;left:9px;color:var(--text-dim);pointer-events:none}
#search{background:var(--surface2);border:1px solid var(--border);border-radius:7px;
  padding:6px 26px 6px 30px;color:var(--text);font-size:12px;width:230px;outline:none}
#search:focus{border-color:var(--accent)}
#search::placeholder{color:var(--text-dim)}
#clear-search{position:absolute;right:7px;background:none;border:none;color:var(--text-dim);
  cursor:pointer;font-size:13px;line-height:1;display:none}

/* sidebar */
#sidebar{position:fixed;top:var(--header-h);left:0;bottom:0;width:var(--sidebar-w);
  background:rgba(255,255,255,.97);border-right:1px solid var(--border);
  overflow-y:auto;z-index:100;padding:12px 10px 28px}
.sb-section{margin-bottom:16px}
.sb-label{font-size:9.5px;font-weight:700;text-transform:uppercase;letter-spacing:.1em;
  color:var(--text-dim);margin-bottom:6px;padding:0 4px}
.nav-btn{display:flex;align-items:center;gap:9px;width:100%;padding:6px 8px;border:none;
  border-radius:6px;background:transparent;cursor:pointer;color:var(--text);font-size:12px;
  text-align:left;margin-bottom:1px;font-family:inherit}
.nav-btn:hover{background:var(--surface2)}
.nav-btn[aria-current="true"]{background:var(--surface2);color:var(--text-hi);
  box-shadow:inset 2px 0 0 var(--accent)}
.nav-num{font-size:9.5px;color:var(--text-dim);width:12px;flex:none}
.nav-btn[aria-current="true"] .nav-num{color:var(--accent)}
.nav-cnt{margin-left:auto;font-size:9.5px;color:var(--text-dim)}
.filter-row{display:flex;align-items:center;gap:7px;padding:4px 6px;border-radius:5px;
  font-size:11px;cursor:pointer;user-select:none}
.filter-row:hover{background:var(--surface2)}
.filter-row input{accent-color:var(--accent);width:12px;height:12px;flex:none;cursor:pointer}
.f-dot{width:9px;height:9px;border-radius:50%;flex:none;border:1px solid rgba(15,20,32,.22)}
.f-cnt{margin-left:auto;font-size:9.5px;color:var(--text-dim)}
.sb-actions{display:flex;gap:5px;margin-bottom:7px}
.sb-action-btn{flex:1;padding:4px 0;background:var(--surface2);border:1px solid var(--border);
  border-radius:5px;color:var(--text-dim);font-size:10px;cursor:pointer;font-family:inherit}
.sb-action-btn:hover{border-color:var(--accent);color:var(--text-hi)}
.sb-divider{height:1px;background:var(--border);margin:6px 0 11px}
.sb-note{font-size:10.5px;color:var(--text-dim);line-height:1.6;padding:0 5px}

/* main */
#main{position:fixed;top:var(--header-h);left:var(--sidebar-w);right:0;bottom:0;overflow:hidden}
.section{display:none;height:100%;overflow-y:auto;padding:18px 22px 40px}
.section.active{display:block}
.section.nopad{padding:0}
.sec-head{margin-bottom:14px}
.sec-head h2{font-size:16px;color:var(--text-hi);font-weight:700;letter-spacing:.01em}
.sec-head p{font-size:11.5px;color:var(--text-dim);margin-top:4px;max-width:104ch;line-height:1.65}

/* cards + grids */
.kpis{display:grid;grid-template-columns:repeat(auto-fill,minmax(158px,1fr));gap:10px;margin-bottom:16px}
.kpi{background:var(--surface);border:1px solid var(--border);border-radius:9px;padding:11px 13px}
.kpi .v{font-size:21px;font-weight:700;color:var(--text-hi);line-height:1.15;letter-spacing:-.01em}
.kpi .l{font-size:10px;color:var(--text-dim);text-transform:uppercase;letter-spacing:.07em;margin-top:3px}
.kpi .s{font-size:10.5px;color:var(--text-dim);margin-top:5px;line-height:1.45}
.kpi.alert{border-color:#fbd5d5}.kpi.alert .v{color:#b91c1c}
.kpi.good .v{color:#059669}
.grid2{display:grid;grid-template-columns:repeat(auto-fit,minmax(420px,1fr));gap:12px}
.grid3{display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:11px}
.panel{background:var(--surface);border:1px solid var(--border);border-radius:9px;
  padding:13px 15px;margin-bottom:12px}
.panel h3{font-size:12px;color:var(--text-hi);font-weight:700;margin-bottom:9px;
  text-transform:uppercase;letter-spacing:.06em}
.panel h3 .sub{text-transform:none;letter-spacing:0;color:var(--text-dim);font-weight:400;
  font-size:10.5px;margin-left:7px}
.panel p{font-size:11.5px;color:#4a5568;line-height:1.65;margin-bottom:8px}
.panel p:last-child{margin-bottom:0}
.panel.danger{border-color:#fecaca;background:#fef2f2}
.panel.warn{border-color:#fde68a;background:#fffbeb}
.panel.accent{border-color:#b9c6e8}

/* tables */
table{width:100%;border-collapse:collapse;font-size:11.5px}
th{text-align:left;font-size:9.5px;text-transform:uppercase;letter-spacing:.07em;
  color:var(--text-dim);font-weight:700;padding:6px 9px;border-bottom:1px solid var(--border);
  position:sticky;top:0;background:var(--surface);z-index:2}
td{padding:5px 9px;border-bottom:1px solid #141a2c;vertical-align:middle;color:var(--text)}
tbody tr:hover{background:var(--surface2)}
.num{text-align:right;font-variant-numeric:tabular-nums}
.mono{font-family:'Cascadia Mono',Consolas,'SF Mono',monospace;font-size:11px}
.dim{color:var(--text-dim)}

/* badges */
.badge{display:inline-block;font-size:9.5px;padding:1.5px 6px;border-radius:4px;
  border:1px solid var(--border);background:var(--surface2);color:var(--text-dim);
  white-space:nowrap;letter-spacing:.02em}
.badge.ok{color:#065f46;border-color:#a7f3d0;background:#ecfdf5}
.badge.warn{color:#92400e;border-color:#fde68a;background:#fffbeb}
.badge.bad{color:#991b1b;border-color:#fecaca;background:#fef2f2}
.badge.info{color:#155e75;border-color:#a5f3fc;background:#ecfeff}

.ste-note{background:#eef2ff;border:1px solid #c7d2fe;border-left:3px solid #2c5fd4;
  border-radius:6px;padding:10px 14px;margin-bottom:18px;font-size:12px;color:#3730a3}
.ste-note b{color:#1e1b4b}
.ste h3{font-size:13.5px;color:var(--text-hi);margin:22px 0 8px;font-weight:650}
.ste h3:first-of-type{margin-top:4px}
.ste p{margin:0 0 9px;max-width:78ch}
.ste ul,.ste ol{margin:0 0 12px 20px;max-width:78ch}
.ste li{margin-bottom:5px}
.ste table{width:100%;border-collapse:collapse;margin:6px 0 16px;font-size:11.5px}
.ste th{background:var(--surface2);text-align:left;padding:7px 9px;border:1px solid var(--border);
  font-weight:650;color:var(--text-hi);vertical-align:middle}
.ste td{padding:7px 9px;border:1px solid var(--border);vertical-align:middle}
.ste td:first-child{font-weight:600;color:var(--text-hi);white-space:nowrap}
.ste code{background:var(--surface2);border:1px solid var(--border);border-radius:3px;
  padding:1px 4px;font-family:ui-monospace,Menlo,Consolas,monospace;font-size:11px}
.ste-cols{display:grid;grid-template-columns:1fr 1fr;gap:18px}
@media(max-width:1100px){.ste-cols{grid-template-columns:1fr}}
.badge.acc{color:#3730a3;border-color:#c7d2fe;background:#eef2ff}

/* graph */
#graph-wrap{position:relative;height:100%}
#graph{position:absolute;inset:0}
#graph svg{width:100%;height:100%;display:block;cursor:grab}
#graph svg:active{cursor:grabbing}
.glink{fill:none;stroke-width:1.1;opacity:.26}
.glink.hi{opacity:.95;stroke-width:2.2}
.glink.dim{opacity:.03}
.glink.dyn{stroke-dasharray:4 3;stroke-width:1.6;opacity:.7}
.glink.xapp{stroke-width:2.4;opacity:.9;stroke-dasharray:none}
.gnode circle,.gnode rect,.gnode polygon{stroke-width:1.6;cursor:pointer}
.gnode.hi circle,.gnode.hi rect,.gnode.hi polygon{stroke:#fff;stroke-width:2.4}
.gnode.dim{opacity:.09}
.gnode .lbl{fill:var(--text);font-size:8.5px;pointer-events:none;text-anchor:middle}
.gnode.sel circle,.gnode.sel rect,.gnode.sel polygon{stroke:#fff;stroke-width:3}
#gbar{position:absolute;left:12px;bottom:12px;display:flex;gap:6px;align-items:center;
  flex-wrap:wrap;max-width:calc(100% - 300px);z-index:20}
.stat-chip{background:rgba(255,255,255,.96);border:1px solid var(--border);border-radius:6px;
  padding:4px 9px;font-size:10.5px;color:var(--text-dim)}
.stat-chip b{color:var(--text-hi);font-size:12px}
#gzoom{position:absolute;right:14px;bottom:12px;display:flex;gap:5px;z-index:20}
.zoom-btn{width:29px;height:29px;background:var(--surface);border:1px solid var(--border);
  border-radius:6px;color:var(--text-dim);font-size:15px;cursor:pointer;display:flex;
  align-items:center;justify-content:center;font-family:inherit}
.zoom-btn:hover{border-color:var(--accent);color:var(--text-hi)}
#ginfo{position:absolute;top:12px;right:14px;width:296px;max-height:calc(100% - 24px);
  overflow-y:auto;background:var(--surface);border:1px solid var(--border);border-radius:10px;
  padding:13px 15px;z-index:30;display:none}
#ginfo.show{display:block}
.ipanel-close{position:absolute;top:9px;right:10px;background:none;border:none;cursor:pointer;
  color:var(--text-dim);font-size:14px;line-height:1;font-family:inherit}
.i-cat{font-size:9.5px;font-weight:700;text-transform:uppercase;letter-spacing:.08em;margin-bottom:3px}
.i-name{font-size:14px;font-weight:700;color:var(--text-hi);margin-bottom:7px;word-break:break-all}
.i-kv{display:grid;grid-template-columns:76px 1fr;gap:3px 8px;font-size:10.5px;margin-bottom:9px}
.i-kv dt{color:var(--text-dim)}
.i-kv dd{color:var(--text);word-break:break-all}
.nb-label{font-size:9.5px;color:var(--text-dim);margin:9px 0 4px;text-transform:uppercase;letter-spacing:.06em}
.nb-tags{display:flex;flex-wrap:wrap;gap:4px}
.nb-tag{font-size:9.5px;padding:2px 6px;border-radius:20px;background:var(--surface2);
  border:1px solid var(--border);color:var(--text-dim);cursor:pointer;font-family:inherit}
.nb-tag:hover{border-color:var(--accent);color:var(--text-hi)}
#tooltip{position:fixed;background:var(--surface2);border:1px solid var(--border);border-radius:6px;
  padding:5px 9px;font-size:11px;color:var(--text-hi);pointer-events:none;z-index:400;display:none;max-width:230px}

/* contract rows */
.ctrl-bar{display:flex;gap:8px;align-items:center;flex-wrap:wrap;margin-bottom:11px}
select,input[type=text],textarea{background:var(--surface2);border:1px solid var(--border);
  border-radius:6px;color:var(--text);font-size:11.5px;padding:5px 8px;outline:none;font-family:inherit}
select:focus,input:focus,textarea:focus{border-color:var(--accent)}
label.inline{display:flex;align-items:center;gap:6px;font-size:11px;color:var(--text-dim);cursor:pointer}
label.inline input{accent-color:var(--accent)}
.btn{background:var(--surface2);border:1px solid var(--border);border-radius:6px;color:var(--text);
  font-size:11px;padding:5px 11px;cursor:pointer;font-family:inherit}
.btn:hover{border-color:var(--accent);color:var(--text-hi)}
.btn.primary{background:#2c5fd4;border-color:#2c5fd4;color:#ffffff}
.row-x{cursor:pointer}
.row-x td:first-child{border-left:2px solid transparent}
.row-x.open td:first-child{border-left-color:var(--accent)}
.detail-cell{background:#f2f4f9;padding:0!important}
.detail-inner{padding:13px 16px}
.dgrid{display:grid;grid-template-columns:repeat(auto-fit,minmax(330px,1fr));gap:11px}
.dblock{background:var(--surface);border:1px solid var(--border);border-radius:7px;padding:10px 12px}
.dblock h4{font-size:10px;text-transform:uppercase;letter-spacing:.07em;color:var(--text-dim);
  margin-bottom:7px;font-weight:700}
.io-card{border:1px solid var(--border);border-radius:6px;padding:7px 9px;margin-bottom:6px;background:#f2f4f9}
.io-card .dd{font-size:11.5px;color:var(--text-hi);font-weight:600}
.io-kv{display:grid;grid-template-columns:88px 1fr;gap:1px 8px;font-size:10.5px;margin-top:5px}
.io-kv dt{color:var(--text-dim)}
.io-kv dd{color:var(--text);word-break:break-word}
.und{color:#a16207}
.pill-list{display:flex;flex-wrap:wrap;gap:4px;margin-top:4px}

/* cards for complexities / findings */
.cx-card{background:var(--surface);border:1px solid var(--border);border-radius:9px;padding:11px 13px;cursor:pointer}

.cx-card:hover{border-color:#2b3a66}
.cx-card.open{border-color:var(--accent)}
.cx-head{display:flex;align-items:baseline;gap:8px}
.cx-id{font-size:10px;color:var(--accent);font-weight:700;flex:none}
.cx-name{font-size:12.5px;color:var(--text-hi);font-weight:600}
.cx-plain{font-size:11px;color:#4a5568;margin-top:5px;line-height:1.55}
.cx-counts{display:flex;gap:6px;margin-top:8px;flex-wrap:wrap}
.cx-places{margin-top:10px;border-top:1px solid var(--border);padding-top:9px;display:none}
.cx-card.open .cx-places{display:block}
.place{border-left:2px solid #253053;padding:5px 0 5px 9px;margin-bottom:7px}
.place .pf{font-size:10.5px;color:var(--accent)}
.place .pp{font-size:11px;color:var(--text-hi)}
.place .pn{font-size:10.5px;color:#5a6478;line-height:1.5;margin-top:2px}

/* lineage */
#lineage-svg{width:100%;background:#ffffff;border:1px solid var(--border);border-radius:9px}
.ln-stage{cursor:pointer}
.ln-stage rect{stroke-width:1.4}
.ln-stage:hover rect{stroke:#fff}
.ln-title{fill:var(--text-hi);font-size:11.5px;font-weight:600}
.ln-sub{fill:var(--text-dim);font-size:9.5px}
.ln-ds{fill:#a5b0c8;font-size:9px;font-family:'Cascadia Mono',Consolas,monospace}
.ln-flow{fill:none;stroke-width:1.4;opacity:.5}
.ln-flow.hi{opacity:1;stroke-width:2.6}

/* empty state */
.empty{border:1px dashed var(--border);border-radius:9px;padding:22px;text-align:center;color:var(--text-dim)}
.empty h4{color:var(--text-hi);font-size:13px;margin-bottom:7px}
pre.code{background:#ffffff;border:1px solid var(--border);border-radius:7px;padding:10px 12px;
  font-family:'Cascadia Mono',Consolas,monospace;font-size:10.5px;color:#9fb0d0;overflow-x:auto;
  line-height:1.55;white-space:pre-wrap}
.hl{background:#3a3210;color:#fde68a;border-radius:2px;padding:0 1px}
.scroll-y{max-height:440px;overflow-y:auto;border:1px solid var(--border);border-radius:8px}
.scroll-y table th{background:#f2f4f9}
.bar{height:5px;border-radius:3px;background:var(--surface2);overflow:hidden;margin-top:5px}
.bar > i{display:block;height:100%;background:var(--accent)}
.legend{display:flex;flex-wrap:wrap;gap:9px;font-size:10px;color:var(--text-dim);margin-top:8px}
.legend span{display:flex;align-items:center;gap:5px}
.sr-only{position:absolute;width:1px;height:1px;padding:0;margin:-1px;overflow:hidden;clip:rect(0,0,0,0);border:0}
</style>
</head>
<body>
"""

TEMPLATE += r"""
<header id="header">
  <div class="brand">
    <h1>CABS Tier 5 — Wholesale Carrier Access Billing Reference Estate</h1>
    <p>Synthetic z/OS estate · process contracts · seeded defects · CAST ground truth</p>
  </div>
  <div class="divider"></div>
  <div class="hchips" id="head-chips"></div>
  <div class="search-wrap">
    <svg width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24" aria-hidden="true">
      <circle cx="11" cy="11" r="8"/><path d="M21 21l-4.35-4.35"/></svg>
    <label for="search" class="sr-only">Search the estate</label>
    <input type="text" id="search" placeholder="Search programs, datasets, contracts…" autocomplete="off" />
    <button id="clear-search" aria-label="Clear search">&#10005;</button>
  </div>
</header>

<nav id="sidebar" aria-label="Hub sections">
  <div class="sb-section">
    <div class="sb-label" id="nav-label">Sections</div>
    <div id="nav" role="tablist" aria-labelledby="nav-label"></div>
  </div>
  <div class="sb-divider"></div>
  <div class="sb-section" id="ctx-filters"></div>
  <div class="sb-divider"></div>
  <div class="sb-section">
    <div class="sb-label">About</div>
    <div class="sb-note" id="about-note"></div>
  </div>
</nav>

<main id="main">

  <!-- 1 ABOUT -->
  <section class="section" id="sec-about" role="tabpanel" aria-labelledby="tab-about" tabindex="0">
    <div class="sec-head">
      <h2>About the application</h2>
      <p>A description of CABS, its functions, its telecom features, and its inputs and outputs.</p>
    </div>
    <div class="ste">

      <div class="ste-note">
        <b>Language.</b> This page uses ASD-STE100 Simplified Technical English.
        Each sentence is short. Each word has one meaning. The text uses the active voice.
        Technical names keep the form that the telecom industry uses.
      </div>

      <h3>1. What the application does</h3>
      <p>The application has the name CABS. The letters mean Carrier Access Billing System.
      A telephone company owns a network. Other telephone companies send calls across this network.
      These other companies must pay for this use. CABS calculates the charge and makes an invoice.</p>

      <h3>2. Who receives an invoice</h3>
      <p>Three types of company receive an invoice from CABS.</p>
      <ul>
        <li>An <b>IXC</b> carries calls between cities. The letters mean Interexchange Carrier.</li>
        <li>A <b>CLEC</b> gives local telephone service in the same area as the network owner.
            The letters mean Competitive Local Exchange Carrier.</li>
        <li>A <b>reseller</b> sells the service of the network owner to its own customers.</li>
      </ul>
      <p>A fourth type of company does not receive an invoice. An <b>ILEC</b> is a local telephone
      company in a different area. CABS and the ILEC exchange money between them. Section 5 gives
      more data about this exchange.</p>

      <h3>3. The main functions</h3>
      <ol>
        <li>The application reads the records of network use.</li>
        <li>The application examines each record and finds the errors.</li>
        <li>The application applies a price to each record. The telecom industry calls this function <i>rating</i>.</li>
        <li>The application decides which government body controls each call.</li>
        <li>The application divides the money between the network owner and the other companies.</li>
        <li>The application adds the charges together for each account.</li>
        <li>The application makes an invoice.</li>
        <li>The application prints the invoice or writes it to a file.</li>
        <li>The application makes the reports and the control totals.</li>
      </ol>
      <p>The application does these functions one time each day. It does more functions one time
      each month. The monthly functions close the accounts and make the invoices.</p>

      <h3>4. The telecom functions</h3>
      <table>
        <thead><tr><th style="width:22%">Function</th><th>Description</th></tr></thead>
        <tbody>
          <tr><td>Switched access</td><td>A customer makes a long distance call. The call goes across
            the local network to the IXC. CABS charges the IXC for this use. The charge has five parts:
            access at the start of the call, access at the end of the call, local transport, tandem
            switch use, and the carrier common line charge.</td></tr>
          <tr><td>Special access</td><td>A company rents a circuit. Only that company uses the circuit.
            CABS charges a fixed amount each month. The amount does not change with the number of calls.</td></tr>
          <tr><td>Unbundled network elements</td><td>A company rents one part of the network.
            CABS charges for that part only. The short name is UNE.</td></tr>
          <tr><td>Interconnection</td><td>Two networks connect together. CABS charges for this connection.</td></tr>
          <tr><td>Jurisdiction</td><td>Each call is interstate, intrastate, or local. A different
            government body controls each type. A different price applies to each type.</td></tr>
          <tr><td>PIU and PLU factors</td><td>A carrier tells the network owner a percentage.
            The percentage shows how many of its calls cross a state border. CABS uses the percentage
            to divide the minutes between the jurisdictions.</td></tr>
          <tr><td>Restatement</td><td>A new percentage can arrive after CABS sends the invoice.
            CABS calculates the difference for the earlier period. CABS then makes an adjustment.</td></tr>
          <tr><td>Meet-point billing</td><td>Two local companies carry one call together.
            Each company bills its own percentage of the charge. The two percentages must give a
            total of 100. CABS finds the records where the total is not 100.</td></tr>
          <tr><td>Reciprocal compensation</td><td>Company A sends a call to company B.
            Company B completes the call. Company B charges company A for this work.</td></tr>
          <tr><td>ISP cap</td><td>A limit applies to calls that go to an internet provider.
            CABS pays no compensation for the minutes above this limit.</td></tr>
          <tr><td>CMDS and RAO settlement</td><td>Telephone companies exchange the records of use
            between them. CABS calculates the money that each company owes. A RAO code identifies
            each company in this exchange.</td></tr>
        </tbody>
      </table>

      <h3>5. The inputs</h3>
      <table>
        <thead><tr><th style="width:24%">Input</th><th style="width:20%">Format</th><th>Content</th></tr></thead>
        <tbody>
          <tr><td>Usage records</td><td>Fixed, 200 bytes, EBCDIC</td><td>One record for each use of the
            network. The record shows the carrier, the account, the time, and the quantity.
            The record has three types: voice, data, and special access.</td></tr>
          <tr><td>Carrier master</td><td>VSAM KSDS</td><td>One record for each carrier. The record shows
            the name, the type, the default percentages, and the settlement rules.</td></tr>
          <tr><td>Rate tables</td><td>VSAM KSDS</td><td>The price for each element. A price can have
            five decimal places. A price can change with the quantity.</td></tr>
          <tr><td>Factor records</td><td>VSAM KSDS</td><td>The PIU and PLU percentages for each carrier
            and each state. A new record can apply to an earlier period.</td></tr>
          <tr><td>Circuit inventory</td><td>VSAM KSDS</td><td>One record for each circuit or trunk group.
            The record shows the location, the account, and the meet-point percentages.</td></tr>
          <tr><td>Settlement records</td><td>Fixed, 180 bytes</td><td>The records that other companies
            send. These records show the use between the two companies.</td></tr>
          <tr><td>Control cards</td><td>Fixed, 80 bytes</td><td>The values that control one run.
            An operator supplies these values at the start of the run. The job does not supply a
            default value for them.</td></tr>
        </tbody>
      </table>

      <h3>6. The outputs</h3>
      <table>
        <thead><tr><th style="width:24%">Output</th><th style="width:20%">Format</th><th>Content</th></tr></thead>
        <tbody>
          <tr><td>Invoice header</td><td>Fixed, 400 bytes</td><td>One record for each account and each
            period. The record shows the total amount and the division between the jurisdictions.</td></tr>
          <tr><td>Invoice detail</td><td>Variable, 1651 bytes</td><td>One record for each line on the
            invoice. A line can have as many as 40 elements. The length changes with the number of elements.</td></tr>
          <tr><td>Settlement records</td><td>Fixed, 200 bytes</td><td>The money that each company owes.
            The record shows the type, the counterparty, and the direction of the payment.</td></tr>
          <tr><td>Suspense records</td><td>Fixed, 300 bytes</td><td>The records that failed a test.
            The record shows the error code, the program, and the original record.</td></tr>
          <tr><td>Control records</td><td>Fixed, 180 bytes</td><td>Each program writes one record.
            The record shows the counts and the hash totals. Section 7 gives the rule for these counts.</td></tr>
          <tr><td>Print file</td><td>Fixed, 133 bytes</td><td>The invoice for the printer. The first
            character controls the paper movement. The same character also shows a new section.</td></tr>
          <tr><td>Media extract</td><td>Fixed</td><td>The invoice data for a tape or an electronic
            transfer. A customer can receive the invoice in this form.</td></tr>
          <tr><td>Reports</td><td>Print</td><td>The revenue, the exceptions, the settlement position,
            and the balance of the run.</td></tr>
        </tbody>
      </table>

      <h3>7. How the application proves that a run is correct</h3>
      <p>Each program writes one control record. The control record holds five counts and four hash
      totals. The counts must obey this rule:</p>
      <p><code>records read = records written + records rejected + records summarised + records carried forward</code></p>
      <p>A program sets a flag when the rule fails. A report at the end of the run examines all the
      control records. The report shows if the run is correct.</p>

      <h3>8. The two applications</h3>
      <div class="ste-cols">
        <div>
          <p><b>CABS</b> calculates the charges and makes the invoices. It owns the usage files,
          the rate tables, and the invoice files.</p>
        </div>
        <div>
          <p><b>SETL</b> calculates the money between the companies. It owns the settlement files.</p>
        </div>
      </div>
      <p>The two applications read the files of each other directly. They do not use an interface.
      Therefore you cannot move one application to the cloud alone. You must move both applications
      together, or you must first build an interface between them. This condition is the largest
      obstacle to a move.</p>

      <h3>9. What the application does not do</h3>
      <ul>
        <li>The application does not bill the customers of the network owner. A different system does this work.</li>
        <li>The application does not control the network. It only measures the use of the network.</li>
        <li>The application does not collect the money. It sends the invoice to a different system.</li>
        <li>The application does not calculate the tax for all the products. It calculates the tax for the access products only.</li>
      </ul>

      <h3>10. Why this application exists here</h3>
      <p>This estate is synthetic. No customer data is in it. The estate is a test. It measures if a
      process can move a large mainframe application to the cloud. The estate contains 27 known
      constructs that make this move difficult. The estate also contains known defects.</p>
      <p>The defects have a purpose. A correct transformation must find them. A correct transformation
      must not repair them without a record. The list of the defects is in a separate directory.
      You must remove that directory before a blind test.</p>

    </div>
  </section>

  <!-- 2 OVERVIEW -->
  <section class="section" id="sec-overview" role="tabpanel" aria-labelledby="tab-overview" tabindex="0">
    <div class="sec-head">
      <h2>Overview — the estate at a glance</h2>
      <p id="ov-lead"></p>
    </div>
    <div class="kpis" id="ov-kpis"></div>
    <div class="grid2">
      <div class="panel"><h3>By technology</h3><div class="scroll-y"><table id="ov-tech"></table></div></div>
      <div class="panel"><h3>By folder <span class="sub">files and lines counted from the tree</span></h3>
        <div class="scroll-y"><table id="ov-folder"></table></div></div>
    </div>
    <div class="grid2">
      <div class="panel"><h3>Runnable versus reference-only <span class="sub">README.md</span></h3>
        <div class="scroll-y"><table id="ov-runnable"></table></div></div>
      <div class="panel accent"><h3>The two application boundaries</h3>
        <div id="ov-apps"></div>
        <h3 style="margin-top:12px">Cross-application coupling <span class="sub">no interface, just a filename and a record layout</span></h3>
        <div class="scroll-y" style="max-height:260px"><table id="ov-coupling"></table></div></div>
    </div>
    <div class="panel"><h3>Family manifests <span class="sub">each family documents its own placements as an independent check</span></h3>
      <div class="scroll-y"><table id="ov-manifests"></table></div></div>
  </section>

  <!-- 2 GRAPH -->
  <section class="section nopad" id="sec-graph" role="tabpanel" aria-labelledby="tab-graph" tabindex="0">
    <div id="graph-wrap">
      <div id="graph"><svg id="gsvg" role="img" aria-label="Knowledge graph of the estate"></svg></div>
      <div id="ginfo"><button class="ipanel-close" id="ginfo-close" aria-label="Close">&#10005;</button>
        <div class="i-cat" id="gi-cat"></div><div class="i-name" id="gi-name"></div>
        <dl class="i-kv" id="gi-kv"></dl><div id="gi-note"></div>
        <div class="nb-label">Outgoing</div><div class="nb-tags" id="gi-out"></div>
        <div class="nb-label">Incoming</div><div class="nb-tags" id="gi-in"></div>
      </div>
      <div id="gbar">
        <div class="stat-chip"><b id="g-n">0</b> nodes shown</div>
        <div class="stat-chip"><b id="g-e">0</b> edges shown</div>
        <div class="stat-chip" id="g-total"></div>
        <button class="btn" id="g-expand">Show more nodes</button>
        <button class="btn" id="g-all">Show all</button>
        <button class="btn" id="g-freeze">Freeze layout</button>
      </div>
      <div id="gzoom">
        <button class="zoom-btn" id="gz-in" aria-label="Zoom in">+</button>
        <button class="zoom-btn" id="gz-out" aria-label="Zoom out">&minus;</button>
        <button class="zoom-btn" id="gz-fit" aria-label="Reset view" style="font-size:12px">&#8962;</button>
      </div>
    </div>
  </section>

  <!-- 3 CONTRACTS -->
  <section class="section" id="sec-contracts" role="tabpanel" aria-labelledby="tab-contracts" tabindex="0">
    <div class="sec-head">
      <h2>Process contracts</h2>
      <p id="cn-lead"></p>
    </div>
    <div class="kpis" id="cn-kpis"></div>
    <div class="ctrl-bar">
      <label class="sr-only" for="cn-q">Filter contracts</label>
      <input type="text" id="cn-q" placeholder="Filter by process, name, job, dataset…" style="width:280px" />
      <label class="sr-only" for="cn-app">Application</label>
      <select id="cn-app"></select>
      <label class="sr-only" for="cn-fam">Family</label>
      <select id="cn-fam"></select>
      <label class="sr-only" for="cn-kind">Kind</label>
      <select id="cn-kind"></select>
      <label class="inline"><input type="checkbox" id="cn-und" /> only with UNDETERMINED items</label>
      <span class="dim" id="cn-count"></span>
      <button class="btn" id="cn-reset">Reset</button>
    </div>
    <div class="scroll-y" style="max-height:calc(100vh - 340px)"><table id="cn-table"></table></div>
    <div class="panel" style="margin-top:12px"><h3>Where the UNDETERMINED items are</h3>
      <div class="grid2"><div><table id="cn-und-summary"></table></div>
      <div id="cn-gaps"></div></div></div>
  </section>

  <!-- 4 LINEAGE -->
  <section class="section" id="sec-lineage" role="tabpanel" aria-labelledby="tab-lineage" tabindex="0">
    <div class="sec-head"><h2>Dataflow lineage</h2>
      <p>Left-to-right pipeline, with the datasets that pass between stages. Every edge is a dataset that
         one stage catalogues and the next stage reads — derived from the input and output DD statements in the
         process contracts, not drawn by hand. Click a stage to filter the process contracts view to that family.</p></div>
    <div id="lineage-host"></div>
    <div class="grid2" style="margin-top:12px">
      <div class="panel"><h3>Stage detail</h3><div id="ln-detail" class="dim">Select a stage.</div></div>
      <div class="panel"><h3>Hand-offs <span class="sub">dataset counts between stages</span></h3>
        <div class="scroll-y" style="max-height:300px"><table id="ln-flows"></table></div></div>
    </div>
  </section>

  <!-- 5 COMPLEXITIES -->
  <section class="section" id="sec-complexity" role="tabpanel" aria-labelledby="tab-complexity" tabindex="0">
    <div class="sec-head"><h2>The 27 complexities</h2>
      <p id="cx-lead"></p></div>
    <div class="ctrl-bar">
      <label class="sr-only" for="cx-q">Filter constructs</label>
      <input type="text" id="cx-q" placeholder="Filter constructs, files, paragraphs…" style="width:300px" />
      <label class="inline"><input type="checkbox" id="cx-exp" /> expand all placement lists</label>
      <span class="dim" id="cx-count"></span>
    </div>
    <div class="grid3" id="cx-cards"></div>
    <div class="panel warn" style="margin-top:14px"><h3>Recorded as a finding, not as one of the 27</h3>
      <div id="cx-notconstruct"></div></div>
  </section>

  <!-- 6 CAST -->
  <section class="section" id="sec-cast" role="tabpanel" aria-labelledby="tab-cast" tabindex="0">
    <div class="sec-head"><h2>Expected CAST findings</h2>
      <p>The first panel is ground truth — what a good static analyser should surface from this estate, taken from
         <span class="mono">DOCS/CAST_IMAGING_GUIDE.md</span> §2. The second panel is the point of the exercise:
         what no static analyser can reach here, because the information is not in the artefacts being scanned.</p></div>
    <div class="grid2">
      <div class="panel"><h3>What a good scan should report <span class="sub">§2.8</span></h3>
        <table id="cast-table"></table></div>
      <div class="panel"><h3>Ground truth by area <span class="sub">§2.1 – §2.7</span></h3>
        <div class="scroll-y" style="max-height:420px;border:none" id="cast-expected"></div></div>
    </div>
    <div class="panel danger" style="margin-top:6px">
      <h3 style="color:#991b1b">What CAST will miss — and this is the point</h3>
      <p>Nine named blind spots. None of them is a defect in the tool: the fact simply is not present in the
         scanned artefact. Each one is work a person has to do, and each one is invisible in a scan report —
         which is exactly why an estate that contains them is worth having.</p>
      <div class="grid2" id="cast-misses"></div>
    </div>
  </section>

  <!-- 7 DEFECTS -->
  <section class="section" id="sec-defects" role="tabpanel" aria-labelledby="tab-defects" tabindex="0">
    <div class="sec-head"><h2>Seeded defects</h2></div>
    <div class="panel danger">
      <h3 style="color:#991b1b">The answer key is withheld</h3>
      <p>This page carries <b>id, file and detectable_by only</b>. The construct statement, the description, the business impact
         and the correct modernization response for every defect live in <span class="mono">SEALED/answer_key_*.json</span>
         and are <b>not embedded in this file</b>. So does <span class="mono">HARNESS/defect_signatures.json</span>, which
         names each defect and describes its fingerprint. Both must be withheld for a blind run —
         <span class="mono">HARNESS/verdict.py</span> raises <span class="mono">BlindRunError</span> on either, by design.</p>
      <p id="def-note"></p>
    </div>
    <div class="panel"><h3><span id="def-count">The seeded defects</span> <span class="sub">not annotated anywhere in the source — that is a build rule</span></h3>
      <table id="def-table"></table></div>
  </section>

  <!-- 8 COMPARISON -->
  <section class="section" id="sec-compare" role="tabpanel" aria-labelledby="tab-compare" tabindex="0">
    <div class="sec-head"><h2>Comparison results</h2>
      <p>Load a JSON report produced by <span class="mono">HARNESS/report.py</span>
         (schema <span class="mono">cabs.comparison.v1</span>). Nothing is uploaded — the file is read in the browser
         and held in a JavaScript variable for this page view only.</p></div>
    <div class="ctrl-bar">
      <label class="btn primary" for="rp-file">Load report JSON…</label>
      <input type="file" id="rp-file" accept=".json,application/json" style="display:none" />
      <button class="btn" id="rp-paste-btn">Paste JSON instead</button>
      <button class="btn" id="rp-clear">Clear</button>
      <span class="dim" id="rp-status"></span>
    </div>
    <div id="rp-paste" style="display:none;margin-bottom:12px">
      <label class="sr-only" for="rp-text">Paste report JSON</label>
      <textarea id="rp-text" rows="7" style="width:100%" placeholder="Paste the contents of report.json here…"></textarea>
      <div style="margin-top:7px"><button class="btn primary" id="rp-parse">Render</button></div>
    </div>
    <div id="rp-out"></div>
  </section>

</main>
<div id="tooltip" role="status"></div>
<script>
const DATA = /*__PAYLOAD__*/null;
</script>
"""

TEMPLATE += r"""
<script>
/* ══════════════════════════════════════════════════════════════════
   helpers
   ══════════════════════════════════════════════════════════════════ */
const $ = (s, r) => (r || document).querySelector(s);
const $$ = (s, r) => Array.from((r || document).querySelectorAll(s));
const esc = s => String(s == null ? '' : s).replace(/[&<>"']/g, c =>
  ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
const n0 = v => (v == null || v === '') ? '—' : Number(v).toLocaleString('en-US');
const UND = 'UNDETERMINED';
const isUnd = v => typeof v === 'string' && v.trim() === UND;
const fv = v => {
  if (v == null || v === '') return '<span class="dim">—</span>';
  if (isUnd(v)) return '<span class="und">UNDETERMINED</span>';
  if (Array.isArray(v)) return v.map(x => isUnd(x) ? '<span class="und">UNDETERMINED</span>' : esc(x)).join('<br>');
  if (typeof v === 'object') return '<span class="mono">' + esc(JSON.stringify(v)) + '</span>';
  return esc(v);
};
function table(headers, rows, opts) {
  opts = opts || {};
  let h = '<thead><tr>' + headers.map((x, i) =>
    '<th' + (opts.num && opts.num.includes(i) ? ' class="num"' : '') + '>' + esc(x) + '</th>').join('') + '</tr></thead>';
  let b = '<tbody>' + rows.map(r => '<tr>' + r.map((c, i) =>
    '<td' + (opts.num && opts.num.includes(i) ? ' class="num"' : '') + '>' + c + '</td>').join('') + '</tr>').join('') + '</tbody>';
  return h + b;
}
const tableEl = (h, r, o) => '<table>' + table(h, r, o) + '</table>';
const hlq = (txt, q) => {
  if (!q) return esc(txt);
  const i = String(txt).toLowerCase().indexOf(q.toLowerCase());
  if (i < 0) return esc(txt);
  const s = String(txt);
  return esc(s.slice(0, i)) + '<span class="hl">' + esc(s.slice(i, i + q.length)) + '</span>' + esc(s.slice(i + q.length));
};

const TYPE_META = {
  application:    {label:'Application',        color:'#334155', sym:'star'},
  program:        {label:'COBOL program',      color:'#3b82f6', sym:'circle'},
  cics_program:   {label:'CICS program',       color:'#06b6d4', sym:'circle'},
  utility:        {label:'z/OS utility',       color:'#64748b', sym:'circle'},
  sort_exit:      {label:'Sort E15/E35 exit',  color:'#ef4444', sym:'star'},
  pli_program:    {label:'PL/I program',       color:'#84cc16', sym:'asterisk'},
  hlasm_program:  {label:'HLASM module',       color:'#a855f7', sym:'plus'},
  rexx_exec:      {label:'REXX exec',          color:'#14b8a6', sym:'wye'},
  copybook:       {label:'Copybook',           color:'#10b981', sym:'triangle'},
  dataset:        {label:'Dataset',            color:'#8b5cf6', sym:'square'},
  job:            {label:'JCL job',            color:'#f59e0b', sym:'diamond'},
  proc:           {label:'JCL PROC',           color:'#f97316', sym:'cross'},
  control_card:   {label:'Sort control card',  color:'#eab308', sym:'triangle'},
  dynamic_target: {label:'Dynamic CALL target (unresolved)', color:'#ec4899', sym:'times'},
  symbolic_program:{label:'EXEC PGM=&symbolic (unresolved)', color:'#f472b6', sym:'times'},
  library:        {label:'Load / copy library', color:'#556070', sym:'square'},
};
const EDGE_COLOR = {
  call_static:'#60a5fa', call_dynamic:'#ec4899', copy:'#059669', reads:'#a78bfa',
  writes:'#c084fc', job_program:'#a16207', proc_include:'#fb923c',
  proc_override:'#fb7185', control_card:'#facc15', sort_exit:'#ef4444',
  steplib:'#556070', membership:'#cbd5e1'
};
const HAS_D3 = (typeof d3 !== 'undefined');
function d3sym(name) {
  if (!HAS_D3) return null;
  const m = {circle:d3.symbolCircle, square:d3.symbolSquare, diamond:d3.symbolDiamond,
    triangle:d3.symbolTriangle, cross:d3.symbolCross, star:d3.symbolStar, wye:d3.symbolWye,
    asterisk:(d3.symbolAsterisk||d3.symbolCross), plus:(d3.symbolPlus||d3.symbolCross),
    times:(d3.symbolTimes||d3.symbolCross)};
  return m[name] || d3.symbolCircle;
}

/* ══════════════════════════════════════════════════════════════════
   navigation
   ══════════════════════════════════════════════════════════════════ */
const SECTIONS = [
  {id:'about',      n:'1', label:'About the Application'},
  {id:'overview',   n:'2', label:'Overview'},
  {id:'graph',      n:'3', label:'Knowledge Graph'},
  {id:'contracts',  n:'4', label:'Process Contracts'},
  {id:'lineage',    n:'5', label:'Dataflow Lineage'},
  {id:'complexity', n:'6', label:'27 Complexities'},
  {id:'cast',       n:'7', label:'Expected CAST Findings'},
  {id:'defects',    n:'8', label:'Seeded Defects'},
  {id:'compare',    n:'9', label:'Comparison Results'},
];
let CURRENT = 'about';

function buildNav() {
  const counts = {
    overview: DATA.estate.total_files + ' files',
    graph: DATA.graph_stats.nodes + ' nodes',
    contracts: DATA.contracts.processes.length,
    lineage: DATA.lineage.stages.length + ' stages',
    complexity: DATA.complexity.constructs.length,
    cast: DATA.cast.misses.length + ' blind spots',
    defects: DATA.defects.length,
    compare: '',
  };
  $('#nav').innerHTML = SECTIONS.map(s =>
    '<button class="nav-btn" role="tab" id="tab-' + s.id + '" data-sec="' + s.id + '" ' +
    'aria-controls="sec-' + s.id + '" aria-current="' + (s.id === CURRENT) + '">' +
    '<span class="nav-num">' + s.n + '</span><span>' + esc(s.label) + '</span>' +
    '<span class="nav-cnt">' + esc(counts[s.id]) + '</span></button>').join('');
  $$('#nav .nav-btn').forEach(b => {
    b.addEventListener('click', () => show(b.dataset.sec));
    b.addEventListener('keydown', e => {
      const list = $$('#nav .nav-btn'); const i = list.indexOf(b);
      if (e.key === 'ArrowDown' || e.key === 'ArrowRight') { e.preventDefault(); list[(i+1)%list.length].focus(); }
      if (e.key === 'ArrowUp' || e.key === 'ArrowLeft') { e.preventDefault(); list[(i-1+list.length)%list.length].focus(); }
      if (e.key === 'Home') { e.preventDefault(); list[0].focus(); }
      if (e.key === 'End') { e.preventDefault(); list[list.length-1].focus(); }
    });
  });
}
function show(id) {
  CURRENT = id;
  $$('.section').forEach(s => s.classList.toggle('active', s.id === 'sec-' + id));
  $$('#nav .nav-btn').forEach(b => b.setAttribute('aria-current', String(b.dataset.sec === id)));
  buildCtxFilters();
  $('#search').placeholder = ({
    graph:'Search nodes…', contracts:'Search contracts…', complexity:'Search constructs…',
    defects:'Search defects…', cast:'Search findings…'
  })[id] || 'Search the estate…';
  if (id === 'graph') { ensureGraph(); }
  if (id === 'lineage') { drawLineage(); }
}

/* context-sensitive sidebar filters */
function buildCtxFilters() {
  const host = $('#ctx-filters');
  if (CURRENT === 'graph') { host.innerHTML = graphFilterHTML(); wireGraphFilters(); }
  else if (CURRENT === 'contracts') {
    host.innerHTML = '<div class="sb-label">Families</div>' + famFilterHTML();
    $$('#ctx-filters .filter-row input').forEach(i => i.addEventListener('change', () => {
      $('#cn-fam').value = 'ALL'; renderContracts();
    }));
  } else if (CURRENT === 'complexity') {
    host.innerHTML = '<div class="sb-label">Status</div>' + statusFilterHTML();
    $$('#ctx-filters .filter-row input').forEach(i => i.addEventListener('change', renderComplexity));
  } else {
    host.innerHTML = '<div class="sb-label">Jump to</div>' +
      SECTIONS.filter(s => s.id !== CURRENT).slice(0, 4).map(s =>
        '<button class="nav-btn" data-sec="' + s.id + '"><span class="nav-num">' + s.n +
        '</span><span>' + esc(s.label) + '</span></button>').join('');
    $$('#ctx-filters .nav-btn').forEach(b => b.addEventListener('click', () => show(b.dataset.sec)));
  }
}
"""

TEMPLATE += r"""
/* ══════════════════════════════════════════════════════════════════
   1. overview
   ══════════════════════════════════════════════════════════════════ */
function renderOverview() {
  const e = DATA.estate, m = DATA.meta, c = m.counts || {};
  $('#ov-lead').innerHTML = esc(DATA.readme.what_it_is[0] || '') +
    ' <b>' + esc(m.principle || '') + '</b>';
  $('#about-note').innerHTML =
    '<b>' + esc(m.asset) + '</b><br>Register generated ' + esc(m.generated) + '.<br><br>' +
    'Everything on this page is read out of the estate. Nothing is asserted that the source does not carry — ' +
    'where the source is silent the field reads <span class="und">UNDETERMINED</span>.';

  const runnableFiles = DATA.readme.runnable_table.filter(r => r.runnable);
  const refFiles = DATA.readme.runnable_table.filter(r => !r.runnable);
  const sumLines = rows => rows.reduce((a, r) => a + (parseInt(String(r.lines).replace(/[^0-9]/g,''), 10) || 0), 0);
  const cobolLines = (e.by_tech.find(t => t.tech === 'COBOL') || {}).lines || 0;

  const kpis = [
    {v:n0(e.total_files), l:'Files in the estate', s:'walked from the tree, excluding caches'},
    {v:n0(e.total_lines), l:'Total lines', s:'all technologies, including documentation'},
    {v:n0(cobolLines), l:'COBOL lines', s:n0((e.by_tech.find(t=>t.tech==='COBOL')||{}).files) + ' .cbl members'},
    {v:n0(c.processes), l:'Process contracts', s:n0(c.cobol_program_invocations) + ' program invocations, ' + n0(c.sort_steps) + ' sort steps'},
    {v:n0(DATA.contracts.undetermined_total), l:'UNDETERMINED items', cls:'alert',
     s:'the part static reading cannot recover — a finding, not a blemish'},
    {v:n0(DATA.complexity.constructs.length), l:'Complexity constructs',
     s:n0(DATA.complexity.constructs.reduce((a,x)=>a+x.placements.length,0)) + ' verified placements'},
    {v:n0(DATA.defects.length), l:'Seeded defects', s:'answer key withheld in SEALED/'},
    {v:n0(DATA.graph_stats.nodes), l:'Graph nodes', s:n0(DATA.graph_stats.edges_excl_membership) + ' relationships'},
    {v:n0(sumLines(runnableFiles)), l:'Runnable lines', cls:'good', s:'OS/VS COBOL 1974, Hercules TK4- / MVS 3.8j'},
    {v:n0(sumLines(refFiles)), l:'Reference-only lines', s:'authored to production standard for static analysis'},
    {v:n0(c.programs_with_no_invoking_job_step), l:'Programs with no invoking job step', cls:'alert',
     s:'orphans a scan should surface'},
    {v:'2', l:'Applications', s:'CABS access billing · SETL settlement'},
  ];
  $('#ov-kpis').innerHTML = kpis.map(k =>
    '<div class="kpi ' + (k.cls || '') + '"><div class="v">' + k.v + '</div>' +
    '<div class="l">' + esc(k.l) + '</div><div class="s">' + esc(k.s || '') + '</div></div>').join('');

  const maxT = Math.max.apply(null, e.by_tech.map(t => t.lines));
  $('#ov-tech').innerHTML = table(['Technology','Files','Lines',''],
    e.by_tech.map(t => [esc(t.tech), n0(t.files), n0(t.lines),
      '<div class="bar" style="width:120px"><i style="width:' + (100*t.lines/maxT).toFixed(1) + '%"></i></div>']),
    {num:[1,2]});

  const maxF = Math.max.apply(null, e.by_top.map(t => t.lines));
  $('#ov-folder').innerHTML = table(['Folder','Files','Lines',''],
    e.by_top.map(t => ['<span class="mono">' + esc(t.folder) + '</span>', n0(t.files), n0(t.lines),
      '<div class="bar" style="width:120px"><i style="width:' + (100*t.lines/maxF).toFixed(1) + '%"></i></div>']),
    {num:[1,2]});

  $('#ov-runnable').innerHTML = table(['Folder','Files','Lines','Status','Why'],
    DATA.readme.runnable_table.map(r => [
      '<span class="mono">' + esc(r.folder) + '</span>', esc(r.files), esc(r.lines),
      '<span class="badge ' + (r.runnable ? 'ok' : 'warn') + '">' + esc(r.status) + '</span>',
      '<span class="dim">' + esc(r.why) + '</span>']), {num:[1,2]});

  $('#ov-apps').innerHTML = DATA.readme.blocker.slice(0, 2).map(p =>
    '<p>' + esc(p) + '</p>').join('') +
    '<p><b>There is no interface to preserve.</b> The coupling is a filename and a record layout. ' +
    'The nine links below are the whole of it.</p>';
  $('#ov-coupling').innerHTML = table(['Program','App','Reads','Owned by'],
    DATA.readme.coupling.map(r => ['<span class="mono">' + esc(r.program) + '</span>',
      '<span class="badge ' + (r.app === 'SETL' ? 'info' : 'acc') + '">' + esc(r.app) + '</span>',
      '<span class="mono">' + esc(r.reads) + '</span>',
      '<span class="badge">' + esc(r.owned_by) + '</span>']));

  $('#ov-manifests').innerHTML = table(['Manifest','Lines','Opens with'],
    DATA.manifests.map(m2 => ['<span class="mono">' + esc(m2.path) + '</span>', n0(m2.lines),
      '<span class="dim">' + esc(m2.lead) + '</span>']), {num:[1]});

  $('#head-chips').innerHTML = [
    ['Files', n0(e.total_files)], ['Lines', n0(e.total_lines)],
    ['Contracts', n0(DATA.contracts.processes.length)],
    ['UNDETERMINED', n0(DATA.contracts.undetermined_total)],
    ['Defects', n0(DATA.defects.length)],
  ].map(([l, v]) => '<span class="hchip">' + l + ' <b>' + v + '</b></span>').join('');
}

/* ══════════════════════════════════════════════════════════════════
   2. knowledge graph
   ══════════════════════════════════════════════════════════════════ */
let G = {inited:false, cap:320, showMembership:false, crossOnly:false, types:{}, sel:null, sim:null, q:''};

function graphFilterHTML() {
  const counts = {};
  DATA.graph.nodes.forEach(n => counts[n.type] = (counts[n.type] || 0) + 1);
  const rows = Object.keys(TYPE_META).filter(t => counts[t]).map(t =>
    '<label class="filter-row"><input type="checkbox" data-gt="' + t + '"' +
    (G.types[t] === false ? '' : ' checked') + ' />' +
    '<span class="f-dot" style="background:' + TYPE_META[t].color + '"></span>' +
    '<span>' + esc(TYPE_META[t].label) + '</span><span class="f-cnt">' + counts[t] + '</span></label>').join('');
  return '<div class="sb-label">Node types</div>' +
    '<div class="sb-actions"><button class="sb-action-btn" id="gt-all">Show all</button>' +
    '<button class="sb-action-btn" id="gt-none">Hide all</button></div>' + rows +
    '<div class="sb-divider"></div><div class="sb-label">View</div>' +
    '<label class="filter-row"><input type="checkbox" id="g-cross"' + (G.crossOnly?' checked':'') +
    ' /><span>Only cross-application edges</span></label>' +
    '<label class="filter-row"><input type="checkbox" id="g-mem"' + (G.showMembership?' checked':'') +
    ' /><span>Show application membership</span></label>' +
    '<div class="sb-divider"></div><div class="sb-label">Edge kinds</div>' +
    '<div class="sb-note">' + Object.keys(EDGE_COLOR).filter(k=>k!=='membership').map(k =>
      '<div style="display:flex;align-items:center;gap:6px;margin-bottom:2px">' +
      '<svg width="20" height="6"><line x1="0" y1="3" x2="20" y2="3" stroke="' + EDGE_COLOR[k] +
      '" stroke-width="2"' + (k === 'call_dynamic' || k === 'proc_override' ? ' stroke-dasharray="4 3"' : '') +
      '/></svg><span>' + k.replace(/_/g, ' ') + '</span></div>').join('') +
    '<div style="margin-top:6px">Dashed edges are <b>unresolved</b>: the target is not knowable from the source.</div></div>';
}
function wireGraphFilters() {
  if (!HAS_D3) return;
  $$('#ctx-filters [data-gt]').forEach(cb => cb.addEventListener('change', () => {
    G.types[cb.dataset.gt] = cb.checked; renderGraph();
  }));
  const all = $('#gt-all'), none = $('#gt-none');
  if (all) all.addEventListener('click', () => { Object.keys(TYPE_META).forEach(t => G.types[t] = true);
    $$('#ctx-filters [data-gt]').forEach(c => c.checked = true); renderGraph(); });
  if (none) none.addEventListener('click', () => { Object.keys(TYPE_META).forEach(t => G.types[t] = false);
    $$('#ctx-filters [data-gt]').forEach(c => c.checked = false); renderGraph(); });
  const cross = $('#g-cross'); if (cross) cross.addEventListener('change', () => {
    G.crossOnly = cross.checked; renderGraph(); });
  const mem = $('#g-mem'); if (mem) mem.addEventListener('change', () => {
    G.showMembership = mem.checked; renderGraph(); });
}

function ensureGraph() {
  if (!HAS_D3) {
    $('#graph').innerHTML = '<div style="padding:40px"><div class="empty">' +
      '<h4>The force-directed graph needs D3 v7</h4><p>D3 is the hub\'s one external dependency and it ' +
      'is loaded from cdnjs. This browser could not reach it, so the graph cannot draw. ' +
      'Every other section on this page works without it — the graph payload of ' +
      DATA.graph_stats.nodes + ' nodes and ' + DATA.graph_stats.edges_excl_membership +
      ' edges is embedded in the file and will render as soon as D3 is available.</p></div></div>';
    return;
  }
  if (G.inited) { if (G.sim) G.sim.alpha(0.25).restart(); return; }
  G.inited = true;
  Object.keys(TYPE_META).forEach(t => { if (G.types[t] === undefined)
    G.types[t] = (t !== 'application' && t !== 'utility' && t !== 'library'); });
  const svg = d3.select('#gsvg');
  G.root = svg.append('g');
  G.lg = G.root.append('g').attr('class', 'links');
  G.ng = G.root.append('g').attr('class', 'nodes');
  G.zoom = d3.zoom().scaleExtent([0.08, 6]).on('zoom', ev => G.root.attr('transform', ev.transform));
  svg.call(G.zoom);
  $('#gz-in').onclick = () => svg.transition().duration(200).call(G.zoom.scaleBy, 1.4);
  $('#gz-out').onclick = () => svg.transition().duration(200).call(G.zoom.scaleBy, 0.7);
  $('#gz-fit').onclick = () => fitGraph();
  $('#g-expand').onclick = () => { G.cap = Math.min(G.cap * 2, DATA.graph.nodes.length); renderGraph(); };
  $('#g-all').onclick = () => { G.cap = DATA.graph.nodes.length; renderGraph(); };
  $('#g-freeze').onclick = () => { if (G.sim) { G.sim.stop();
    $('#g-freeze').textContent = G.frozen ? 'Freeze layout' : 'Resume layout';
    if (G.frozen) { G.sim.alpha(0.3).restart(); } G.frozen = !G.frozen; } };
  $('#ginfo-close').onclick = () => { $('#ginfo').classList.remove('show'); G.sel = null; highlight(); };
  $('#g-total').textContent = DATA.graph_stats.nodes + ' nodes / ' +
    DATA.graph_stats.edges_excl_membership + ' edges in the estate';
  renderGraph();
}

function pickVisible() {
  const byId = {}; DATA.graph.nodes.forEach(n => byId[n.id] = n);
  let edges = DATA.graph.edges.filter(e => G.showMembership || e.kind !== 'membership');
  if (G.crossOnly) edges = edges.filter(e => e.cross_app);
  const typeOk = n => G.types[n.type] !== false;
  let cand = DATA.graph.nodes.filter(typeOk);
  if (G.crossOnly) {
    const keep = new Set(); edges.forEach(e => { keep.add(e.s); keep.add(e.t); });
    cand = cand.filter(n => keep.has(n.id));
  }
  cand = cand.slice().sort((a, b) => (b.deg || 0) - (a.deg || 0));
  const vis = new Set(cand.slice(0, G.cap).map(n => n.id));
  const nodes = cand.slice(0, G.cap);
  const links = edges.filter(e => vis.has(e.s) && vis.has(e.t));
  return {nodes, links, byId};
}

function renderGraph() {
  const box = $('#graph').getBoundingClientRect();
  const W = box.width || 1000, H = box.height || 700;
  const {nodes, links} = pickVisible();
  G.nodes = nodes.map(n => Object.assign({}, n));
  const idx = {}; G.nodes.forEach(n => idx[n.id] = n);
  G.links = links.map(l => Object.assign({}, l, {source: idx[l.s], target: idx[l.t]}));
  G.adj = {}; G.links.forEach(l => {
    (G.adj[l.s] = G.adj[l.s] || []).push(l); (G.adj[l.t] = G.adj[l.t] || []).push(l); });

  $('#g-n').textContent = G.nodes.length;
  $('#g-e').textContent = G.links.length;

  const size = d => d.type === 'application' ? 260 : Math.min(150, 34 + (d.deg || 0) * 3.2);
  const sym = d3.symbol();

  const link = G.lg.selectAll('path').data(G.links, d => d.s + '|' + d.t + '|' + d.r);
  link.exit().remove();
  link.enter().append('path')
    .merge(link)
    .attr('class', d => 'glink' + (d.unresolved ? ' dyn' : '') + (d.cross_app ? ' xapp' : ''))
    .attr('stroke', d => d.cross_app ? '#b91c1c' : (EDGE_COLOR[d.kind] || '#cbd5e1'));

  const node = G.ng.selectAll('g.gnode').data(G.nodes, d => d.id);
  node.exit().remove();
  const ent = node.enter().append('g').attr('class', 'gnode').attr('tabindex', 0)
    .attr('role', 'button')
    .attr('aria-label', d => (TYPE_META[d.type] || {}).label + ' ' + d.label);
  ent.append('path');
  ent.append('text').attr('class', 'lbl').attr('dy', 15);
  const all = ent.merge(node);
  all.select('path')
    .attr('d', d => sym.type(d3sym((TYPE_META[d.type] || {}).sym)).size(size(d))())
    .attr('fill', d => (TYPE_META[d.type] || {}).color || '#64748b')
    .attr('stroke', d => d.resolved === false ? '#991b1b' : 'rgba(15,20,32,.35)')
    .attr('stroke-dasharray', d => d.resolved === false ? '2 2' : null);
  all.select('text').text(d => (G.nodes.length > 260 && (d.deg || 0) < 6 && d.type !== 'application') ? '' : d.label);
  all.on('click', (ev, d) => { ev.stopPropagation(); selectNode(d.id); })
     .on('keydown', (ev, d) => { if (ev.key === 'Enter' || ev.key === ' ') { ev.preventDefault(); selectNode(d.id); } })
     .on('mouseenter', (ev, d) => {
        const t = $('#tooltip'); t.style.display = 'block';
        t.innerHTML = '<b>' + esc(d.label) + '</b><br><span style="color:#8a95ac">' +
          esc((TYPE_META[d.type] || {}).label || d.type) + (d.file ? '<br>' + esc(d.file) : '') + '</span>';
        t.style.left = (ev.clientX + 12) + 'px'; t.style.top = (ev.clientY + 12) + 'px';
     })
     .on('mousemove', ev => { const t = $('#tooltip');
        t.style.left = (ev.clientX + 12) + 'px'; t.style.top = (ev.clientY + 12) + 'px'; })
     .on('mouseleave', () => { $('#tooltip').style.display = 'none'; })
     .call(d3.drag()
        .on('start', (ev, d) => { if (!ev.active) G.sim.alphaTarget(0.18).restart(); d.fx = d.x; d.fy = d.y; })
        .on('drag', (ev, d) => { d.fx = ev.x; d.fy = ev.y; })
        .on('end', (ev, d) => { if (!ev.active) G.sim.alphaTarget(0); }));

  d3.select('#gsvg').on('click', () => { $('#ginfo').classList.remove('show'); G.sel = null; highlight(); });

  if (G.sim) G.sim.stop();
  const big = G.nodes.length > 240 || G.links.length > 900;
  G.sim = d3.forceSimulation(G.nodes)
    .force('link', d3.forceLink(G.links).id(d => d.id).distance(d => d.kind === 'membership' ? 160 : 52).strength(0.32))
    .force('charge', d3.forceManyBody().strength(big ? -90 : -190).distanceMax(520))
    .force('center', d3.forceCenter(W / 2, H / 2))
    .force('collide', d3.forceCollide().radius(d => 10 + Math.sqrt(size(d)) / 2))
    .force('x', d3.forceX(W / 2).strength(0.035))
    .force('y', d3.forceY(H / 2).strength(0.045))
    .alphaDecay(big ? 0.055 : 0.032)
    .velocityDecay(0.42);

  let tick = 0;
  G.sim.on('tick', () => {
    tick++;
    if (big && tick % 2) return;                       // throttle DOM writes on large graphs
    G.lg.selectAll('path').attr('d', d => {
      const x1 = d.source.x, y1 = d.source.y, x2 = d.target.x, y2 = d.target.y;
      const dx = x2 - x1, dy = y2 - y1, dr = Math.sqrt(dx * dx + dy * dy) * 1.9;
      return 'M' + x1 + ',' + y1 + 'A' + dr + ',' + dr + ' 0 0,1 ' + x2 + ',' + y2;
    });
    G.ng.selectAll('g.gnode').attr('transform', d => 'translate(' + d.x + ',' + d.y + ')');
  });
  G.sim.on('end', () => fitGraph());
  G.frozen = false; $('#g-freeze').textContent = 'Freeze layout';
  highlight();
}

function fitGraph() {
  if (!G.nodes || !G.nodes.length) return;
  const xs = G.nodes.map(n => n.x), ys = G.nodes.map(n => n.y);
  const x0 = Math.min.apply(null, xs), x1 = Math.max.apply(null, xs);
  const y0 = Math.min.apply(null, ys), y1 = Math.max.apply(null, ys);
  const box = $('#graph').getBoundingClientRect();
  const k = Math.min(box.width / (x1 - x0 + 120), box.height / (y1 - y0 + 120), 2.2);
  const tx = box.width / 2 - k * (x0 + x1) / 2, ty = box.height / 2 - k * (y0 + y1) / 2;
  d3.select('#gsvg').transition().duration(320)
    .call(G.zoom.transform, d3.zoomIdentity.translate(tx, ty).scale(k));
}

function selectNode(id) {
  G.sel = id; highlight();
  const n = G.nodes.find(x => x.id === id) || DATA.graph.nodes.find(x => x.id === id);
  if (!n) return;
  const meta = TYPE_META[n.type] || {label: n.type, color: '#64748b'};
  $('#gi-cat').textContent = meta.label; $('#gi-cat').style.color = meta.color;
  $('#gi-name').textContent = n.label;
  const kv = [];
  if (n.file) kv.push(['Path', n.file]);
  if (n.lang) kv.push(['Language', n.lang]);
  if (n.lines) kv.push(['Lines', n0(n.lines)]);
  if (n.app) kv.push(['Application', n.app]);
  if (n.dsn) kv.push(['DSN', n.dsn]);
  if (n.gdg) kv.push(['GDG', 'yes — generation selected by relative position']);
  if (n.resolved === false) kv.push(['Resolved', 'NO — no source in the estate']);
  kv.push(['Degree', String(n.deg || 0)]);
  $('#gi-kv').innerHTML = kv.map(([k, v]) =>
    '<dt>' + esc(k) + '</dt><dd class="mono">' + esc(v) + '</dd>').join('');
  let note = n.note ? '<p style="font-size:11px;color:#4a5568;line-height:1.6">' + esc(n.note) + '</p>' : '';
  if (n.rules && n.rules.length) note += '<div class="nb-label">Control card statements</div><pre class="code">' +
    esc(n.rules.join('\n')) + '</pre>';
  $('#gi-note').innerHTML = note;
  const out = (G.adj[id] || []).filter(l => l.s === id);
  const inn = (G.adj[id] || []).filter(l => l.t === id);
  const tag = (l, other) => '<button class="nb-tag" data-goto="' + esc(other) + '">' +
    esc(l.r) + ' → ' + esc((G.nodes.find(x => x.id === other) || {label: other}).label) + '</button>';
  $('#gi-out').innerHTML = out.slice(0, 40).map(l => tag(l, l.t)).join('') || '<span class="dim">none in view</span>';
  $('#gi-in').innerHTML = inn.slice(0, 40).map(l => tag(l, l.s)).join('') || '<span class="dim">none in view</span>';
  $$('#ginfo .nb-tag').forEach(b => b.addEventListener('click', () => selectNode(b.dataset.goto)));
  $('#ginfo').classList.add('show');
}

function highlight() {
  const q = G.q.toLowerCase();
  const matched = new Set();
  if (q) G.nodes.forEach(n => { if (n.label.toLowerCase().includes(q) ||
    (n.file || '').toLowerCase().includes(q)) matched.add(n.id); });
  const near = new Set();
  if (G.sel) { near.add(G.sel); (G.adj[G.sel] || []).forEach(l => { near.add(l.s); near.add(l.t); }); }
  G.ng.selectAll('g.gnode')
    .classed('hi', d => matched.has(d.id))
    .classed('sel', d => d.id === G.sel)
    .classed('dim', d => (q && !matched.has(d.id)) || (G.sel && !near.has(d.id)));
  G.lg.selectAll('path')
    .classed('hi', d => G.sel && (d.s === G.sel || d.t === G.sel))
    .classed('dim', d => (G.sel && d.s !== G.sel && d.t !== G.sel) ||
                         (q && !matched.has(d.s) && !matched.has(d.t)));
}
"""

TEMPLATE += r"""
/* ══════════════════════════════════════════════════════════════════
   3. process contracts
   ══════════════════════════════════════════════════════════════════ */
let CN = {open: new Set(), fams: {}, limit: 120};

function famFilterHTML() {
  const c = {};
  DATA.contracts.processes.forEach(p => c[p.family] = (c[p.family] || 0) + 1);
  return Object.keys(c).sort().map(f =>
    '<label class="filter-row"><input type="checkbox" data-fam="' + esc(f) + '"' +
    (CN.fams[f] === false ? '' : ' checked') + ' /><span>' + esc(f) + '</span>' +
    '<span class="f-cnt">' + c[f] + '</span></label>').join('');
}

function initContracts() {
  const ps = DATA.contracts.processes;
  const uniq = k => Array.from(new Set(ps.map(p => p[k]))).sort();
  $('#cn-app').innerHTML = '<option value="ALL">All applications</option>' +
    uniq('application').map(a => '<option>' + esc(a) + '</option>').join('');
  $('#cn-fam').innerHTML = '<option value="ALL">All families</option>' +
    uniq('family').map(a => '<option>' + esc(a) + '</option>').join('');
  $('#cn-kind').innerHTML = '<option value="ALL">All kinds</option>' +
    uniq('kind').map(a => '<option>' + esc(a) + '</option>').join('');
  ['#cn-q','#cn-app','#cn-fam','#cn-kind','#cn-und'].forEach(s =>
    $(s).addEventListener('input', renderContracts));
  $('#cn-reset').addEventListener('click', () => {
    $('#cn-q').value = ''; $('#cn-app').value = 'ALL'; $('#cn-fam').value = 'ALL';
    $('#cn-kind').value = 'ALL'; $('#cn-und').checked = false; CN.fams = {}; CN.limit = 120;
    buildCtxFilters(); renderContracts();
  });

  const m = DATA.contracts.meta;
  $('#cn-lead').innerHTML = esc(m.authored_from || '') + ' <b>' + esc(m.honesty_rule || '') + '</b>';

  const withUnd = ps.filter(p => p.undetermined_count > 0).length;
  $('#cn-kpis').innerHTML = [
    {v: n0(ps.length), l: 'Process contracts', s: 'each one a declared boundary'},
    {v: n0(DATA.contracts.undetermined_total), l: 'UNDETERMINED items', cls: 'alert',
     s: 'must be supplied by a human'},
    {v: n0(withUnd), l: 'Contracts carrying a gap', cls: 'alert',
     s: (100 * withUnd / ps.length).toFixed(0) + '% of the register'},
    {v: n0(m.counts.programs_with_no_invoking_job_step), l: 'Not invoked by any job step',
     s: 'no JCL in the estate runs them'},
    {v: n0(m.counts.sort_steps), l: 'Sort steps', s: 'business rules on a control card'},
    {v: esc(m.balancing_equation ? '5-term' : '—'), l: 'Balancing equation',
     s: m.balancing_equation || ''},
  ].map(k => '<div class="kpi ' + (k.cls || '') + '"><div class="v">' + k.v + '</div><div class="l">' +
    esc(k.l) + '</div><div class="s">' + esc(k.s) + '</div></div>').join('');

  $('#cn-und-summary').innerHTML = table(['Field', 'Count'],
    DATA.contracts.undetermined_summary.map(u =>
      ['<span class="mono">' + esc(u.field) + '</span>', n0(u.count)]), {num: [1]});
  $('#cn-gaps').innerHTML = DATA.contracts.global_gaps.map(g =>
    '<div class="io-card"><div class="dd mono">' + esc(g.field) + '</div>' +
    '<p style="font-size:11px;color:#4a5568;margin-top:4px;line-height:1.6">' + esc(g.note) + '</p></div>').join('');
  renderContracts();
}

function contractFilter() {
  const q = ($('#cn-q').value || '').trim().toLowerCase();
  const app = $('#cn-app').value, fam = $('#cn-fam').value, kind = $('#cn-kind').value;
  const undOnly = $('#cn-und').checked;
  const famOff = Object.keys(CN.fams).filter(k => CN.fams[k] === false);
  return DATA.contracts.processes.filter(p => {
    if (app !== 'ALL' && p.application !== app) return false;
    if (fam !== 'ALL' && p.family !== fam) return false;
    if (kind !== 'ALL' && p.kind !== kind) return false;
    if (famOff.includes(p.family)) return false;
    if (undOnly && !p.undetermined_count) return false;
    if (q) {
      const hay = (p.process_id + ' ' + p.name + ' ' + p.source + ' ' + (p.trigger && p.trigger.job || '') +
        ' ' + JSON.stringify(p.inputs) + ' ' + JSON.stringify(p.outputs)).toLowerCase();
      if (!hay.includes(q)) return false;
    }
    return true;
  });
}

function renderContracts() {
  CN.fams = {}; $$('#ctx-filters [data-fam]').forEach(c => CN.fams[c.dataset.fam] = c.checked);
  const rows = contractFilter();
  const q = ($('#cn-q').value || '').trim();
  $('#cn-count').textContent = rows.length + ' of ' + DATA.contracts.processes.length + ' contracts';
  const shown = rows.slice(0, CN.limit);
  let body = '';
  shown.forEach(p => {
    const open = CN.open.has(p.process_id);
    body += '<tr class="row-x' + (open ? ' open' : '') + '" data-pid="' + esc(p.process_id) + '" tabindex="0">' +
      '<td class="mono">' + hlq(p.process_id, q) + '</td>' +
      '<td>' + hlq(p.name, q) + '</td>' +
      '<td><span class="badge ' + (p.application === 'SETL' ? 'info' : 'acc') + '">' + esc(p.application) + '</span></td>' +
      '<td><span class="badge">' + esc(p.family) + '</span></td>' +
      '<td class="dim">' + esc(p.kind) + '</td>' +
      '<td class="mono dim">' + esc((p.trigger && p.trigger.job) || '—') + '</td>' +
      '<td class="num">' + (p.inputs || []).length + '</td>' +
      '<td class="num">' + (p.outputs || []).length + '</td>' +
      '<td class="num">' + (p.undetermined_count
        ? '<span class="badge bad">' + p.undetermined_count + '</span>' : '<span class="badge ok">0</span>') + '</td>' +
      '<td class="dim">' + (p.compare_levels || []).join(' ') + '</td></tr>';
    if (open) body += '<tr><td class="detail-cell" colspan="10">' + contractDetail(p) + '</td></tr>';
  });
  $('#cn-table').innerHTML = table(
    ['Process', 'Name', 'App', 'Family', 'Kind', 'Job', 'In', 'Out', 'UNDET', 'Levels'],
    [], {num: [6, 7, 8]}).replace('<tbody></tbody>', '<tbody>' + body + '</tbody>') +
    (rows.length > CN.limit ? '' : '');
  $$('#cn-table .row-x').forEach(tr => {
    const toggle = () => {
      const id = tr.dataset.pid;
      if (CN.open.has(id)) CN.open.delete(id); else CN.open.add(id);
      renderContracts();
    };
    tr.addEventListener('click', toggle);
    tr.addEventListener('keydown', e => { if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); toggle(); } });
  });
  const oldWrap = $('#cn-more-wrap'); if (oldWrap) oldWrap.remove();
  if (rows.length > CN.limit) {
    const b = document.createElement('div');
    b.id = 'cn-more-wrap';
    b.style.padding = '9px'; b.style.textAlign = 'center';
    b.innerHTML = '<button class="btn" id="cn-more">Show ' +
      Math.min(200, rows.length - CN.limit) + ' more (' + (rows.length - CN.limit) + ' hidden)</button>';
    $('#cn-table').parentNode.parentNode.appendChild(b);
    $('#cn-more').addEventListener('click', () => { CN.limit += 200; renderContracts(); });
  }
}

function ioCard(r, side) {
  const keys = side === 'in'
    ? ['dsn','copybook','lrecl','recfm','organization','key','key_source','access','sequence',
       'gdg_relative_generation','disposition','expected_volume','produced_by']
    : ['dsn','copybook','lrecl','recfm','organization','key','key_source','sequence',
       'gdg_relative_generation','disposition','expected_volume','read_by'];
  let h = '<div class="io-card"><div class="dd mono">' + esc(r.dd || '—') + '</div><dl class="io-kv">';
  keys.forEach(k => { if (r[k] !== undefined) h += '<dt>' + esc(k) + '</dt><dd>' + fv(r[k]) + '</dd>'; });
  Object.keys(r).forEach(k => { if (!keys.includes(k) && k !== 'dd')
    h += '<dt>' + esc(k) + '</dt><dd>' + fv(r[k]) + '</dd>'; });
  return h + '</dl></div>';
}

function contractDetail(p) {
  const t = p.trigger || {}, ct = p.control_totals || {}, rs = p.restart || {}, ts = p.target_state || {};
  const kv = o => '<dl class="io-kv">' + Object.keys(o || {}).map(k =>
    '<dt>' + esc(k) + '</dt><dd>' + fv(o[k]) + '</dd>').join('') + '</dl>';
  return '<div class="detail-inner"><div class="dgrid">' +
    '<div class="dblock"><h4>Trigger</h4>' + kv(t) +
      '<h4 style="margin-top:9px">Source</h4><div class="mono" style="font-size:10.5px">' + esc(p.source || '—') + '</div>' +
      (p.control_process_id ? '<h4 style="margin-top:9px">Control process id</h4><div class="mono">' +
        esc(p.control_process_id) + '</div>' : '') + '</div>' +
    '<div class="dblock"><h4>Inputs (' + (p.inputs || []).length + ')</h4>' +
      (p.inputs || []).map(r => ioCard(r, 'in')).join('') + '</div>' +
    '<div class="dblock"><h4>Outputs (' + (p.outputs || []).length + ')</h4>' +
      (p.outputs || []).map(r => ioCard(r, 'out')).join('') + '</div>' +
    '<div class="dblock"><h4>Control totals</h4>' +
      (isUnd(ct) ? '<span class="und">UNDETERMINED</span>' :
        '<dl class="io-kv"><dt>dd</dt><dd>' + fv(ct.dd) + '</dd><dt>copybook</dt><dd>' + fv(ct.copybook) +
        '</dd></dl><div class="pill-list">' + ((ct.fields || []).map(f =>
          '<span class="badge">' + esc(f) + '</span>').join('') || '<span class="dim">no fields declared</span>') +
        '</div>' + (ct.note ? '<p style="font-size:10.5px;color:#4a5568;margin-top:6px;line-height:1.55">' +
          esc(ct.note) + '</p>' : '')) +
      '<h4 style="margin-top:10px">Balancing rule</h4><div style="font-size:11px;color:' +
        (String(p.balancing_rule).startsWith('NONE') || isUnd(p.balancing_rule) ? '#a16207' : '#047857') + '">' +
        fv(p.balancing_rule) + '</div>' +
      (p.sort_rules ? '<h4 style="margin-top:10px">Sort control card</h4><pre class="code">' +
        esc([].concat(p.sort_rules).join('\n')) + '</pre>' : '') + '</div>' +
    '<div class="dblock"><h4>Restart semantics</h4>' + (isUnd(rs) ? '<span class="und">UNDETERMINED</span>' : kv(rs)) + '</div>' +
    '<div class="dblock"><h4>Target-state mapping</h4>' + kv(ts) +
      '<h4 style="margin-top:9px">Compare levels</h4><div class="pill-list">' +
      (p.compare_levels || []).map(l => '<span class="badge acc">' + esc(l) + '</span>').join('') + '</div></div>' +
    '</div></div>';
}

/* ══════════════════════════════════════════════════════════════════
   4. lineage
   ══════════════════════════════════════════════════════════════════ */
function drawLineage() {
  const host = $('#lineage-host');
  const stages = DATA.lineage.stages, flows = DATA.lineage.flows;
  const W = Math.max(host.clientWidth || 1000, 980);
  const colW = W / stages.length, boxW = colW - 34, boxH = 92, top = 34;
  const dsRows = 7, H = top + boxH + 30 + dsRows * 15 + 60;
  const cx = i => 18 + i * colW + boxW / 2;

  let svg = '<svg id="lineage-svg" viewBox="0 0 ' + W + ' ' + H + '" role="img" ' +
    'aria-label="Left to right dataflow from usage ingest through to reporting">' +
    '<defs><marker id="arw" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" ' +
    'orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="#69748c"/></marker>' +
    '<marker id="arwx" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" ' +
    'orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="#b91c1c"/></marker></defs>';

  // flows: adjacent forward flows drawn straight; skip-forward and back flows arced
  flows.forEach(f => {
    const i = stages.findIndex(s => s.code === f.from), j = stages.findIndex(s => s.code === f.to);
    if (i < 0 || j < 0) return;
    const x1 = cx(i) + boxW / 2 - 6, x2 = cx(j) - boxW / 2 + 6;
    const back = j < i;
    const y = top + boxH / 2 + (back ? 26 : 0);
    const arc = Math.abs(j - i) > 1 || back;
    const col = f.cross_app ? '#b91c1c' : '#4f7ef8';
    const d = arc
      ? 'M' + (back ? cx(i) : x1) + ',' + (top + (back ? boxH : boxH / 2)) +
        ' C' + ((cx(i) + cx(j)) / 2) + ',' + (top + boxH + 40 * Math.abs(j - i)) + ' ' +
        ((cx(i) + cx(j)) / 2) + ',' + (top + boxH + 40 * Math.abs(j - i)) + ' ' +
        (back ? cx(j) : x2) + ',' + (top + (back ? boxH : boxH / 2))
      : 'M' + x1 + ',' + y + ' L' + x2 + ',' + y;
    svg += '<path class="ln-flow" data-flow="' + esc(f.from + '>' + f.to) + '" d="' + d +
      '" stroke="' + col + '" marker-end="url(#' + (f.cross_app ? 'arwx' : 'arw') + ')"><title>' +
      esc(f.from + ' → ' + f.to + ': ' + f.count + ' dataset(s)') + '</title></path>';
  });

  stages.forEach((s, i) => {
    const x = 18 + i * colW, col = s.app === 'SETL' ? '#06b6d4' : '#4f7ef8';
    svg += '<g class="ln-stage" data-stage="' + esc(s.code) + '" tabindex="0" role="button" ' +
      'aria-label="' + esc(s.name + ', ' + s.processes + ' processes') + '">' +
      '<rect x="' + x + '" y="' + top + '" width="' + boxW + '" height="' + boxH +
      '" rx="8" fill="#111726" stroke="' + col + '"/>' +
      '<text class="ln-title" x="' + (x + 12) + '" y="' + (top + 22) + '">' + esc(s.name) + '</text>' +
      '<text class="ln-sub" x="' + (x + 12) + '" y="' + (top + 38) + '">' + esc(s.code) +
      ' · ' + s.programs + ' programs · ' + s.processes + ' contracts</text>' +
      '<text class="ln-sub" x="' + (x + 12) + '" y="' + (top + 54) + '">' + esc(s.app) + '</text>' +
      '<text class="ln-sub" x="' + (x + 12) + '" y="' + (top + 74) + '">' +
        esc(s.desc.length > 46 ? s.desc.slice(0, 44) + '…' : s.desc) + '</text></g>';
    svg += '<text class="ln-sub" x="' + (x) + '" y="' + (top + boxH + 44) + '">outputs</text>';
    s.outputs.slice(0, dsRows).forEach((d, k) => {
      svg += '<text class="ln-ds" x="' + x + '" y="' + (top + boxH + 60 + k * 15) + '">' +
        esc(d.replace('TELCABS.', '')) + '</text>';
    });
  });
  host.innerHTML = svg + '</svg>' +
    '<div class="legend"><span><svg width="22" height="6"><line x1="0" y1="3" x2="22" y2="3" ' +
    'stroke="#4f7ef8" stroke-width="2"/></svg> in-application hand-off</span>' +
    '<span><svg width="22" height="6"><line x1="0" y1="3" x2="22" y2="3" stroke="#b91c1c" ' +
    'stroke-width="2"/></svg> crosses the CABS / SETL boundary</span>' +
    '<span>Curved edges run backwards or skip a stage.</span></div>';

  $$('#lineage-svg .ln-stage').forEach(g => {
    const act = () => selectStage(g.dataset.stage);
    g.addEventListener('click', act);
    g.addEventListener('keydown', e => { if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); act(); } });
  });

  $('#ln-flows').innerHTML = table(['From', 'To', 'Datasets', 'Examples'],
    flows.map(f => [
      '<span class="badge">' + esc(f.from) + '</span>', '<span class="badge">' + esc(f.to) + '</span>',
      '<span class="num">' + f.count + '</span>',
      '<span class="mono dim">' + esc(f.datasets.map(d => d.replace('TELCABS.', '')).join(', ')) + '</span>']));
}

function selectStage(code) {
  const s = DATA.lineage.stages.find(x => x.code === code);
  if (!s) return;
  $$('#lineage-svg .ln-flow').forEach(el => {
    const f = el.getAttribute('data-flow') || '';
    el.classList.toggle('hi', f.startsWith(code + '>') || f.endsWith('>' + code));
  });
  $('#ln-detail').innerHTML =
    '<div class="i-name">' + esc(s.name) + ' <span class="badge">' + esc(s.code) + '</span></div>' +
    '<p style="font-size:11.5px;color:#4a5568">' + esc(s.desc) + '</p>' +
    '<dl class="io-kv" style="grid-template-columns:110px 1fr"><dt>Application</dt><dd>' + esc(s.app) + '</dd>' +
    '<dt>Programs</dt><dd>' + s.programs + '</dd><dt>Contracts</dt><dd>' + s.processes + '</dd></dl>' +
    '<div class="nb-label">Datasets it catalogues</div><div class="pill-list">' +
    s.outputs.map(d => '<span class="badge">' + esc(d.replace('TELCABS.', '')) + '</span>').join('') + '</div>' +
    '<div class="nb-label">Datasets it reads</div><div class="pill-list">' +
    s.inputs.map(d => '<span class="badge">' + esc(d.replace('TELCABS.', '')) + '</span>').join('') + '</div>' +
    '<div style="margin-top:11px"><button class="btn primary" id="ln-goto">Filter process contracts to ' +
    esc(s.code) + '</button></div>';
  $('#ln-goto').addEventListener('click', () => {
    show('contracts'); $('#cn-fam').value = s.code; CN.limit = 120; CN.fams = {}; renderContracts();
  });
}
"""

TEMPLATE += r"""
/* ══════════════════════════════════════════════════════════════════
   5. 27 complexities
   ══════════════════════════════════════════════════════════════════ */
let CX = {open: new Set(), status: {}};

function statusFilterHTML() {
  const c = {};
  DATA.complexity.constructs.forEach(x => c[x.status] = (c[x.status] || 0) + 1);
  return Object.keys(c).sort().map(s =>
    '<label class="filter-row"><input type="checkbox" data-cxs="' + esc(s) + '"' +
    (CX.status[s] === false ? '' : ' checked') + ' /><span>' + esc(s) + '</span>' +
    '<span class="f-cnt">' + c[s] + '</span></label>').join('') +
    '<div class="sb-divider"></div><div class="sb-label">Verification</div>' +
    '<div class="sb-note">' + esc((DATA.complexity.verification_method || {}).principle || '') + '</div>';
}

function initComplexity() {
  $('#cx-lead').innerHTML =
    'Each construct is placed in a named paragraph, in a named file, and recorded in ' +
    '<span class="mono">CONTRACTS/complexity_placement.json</span>, which is authoritative — if a workbook ' +
    'and the JSON disagree, the JSON wins. <b>required</b> is what the placement plan demanded; ' +
    '<b>as built</b> is what a scan of the source actually found. ' +
    esc((DATA.complexity.verification_method || {}).principle || '');
  $('#cx-q').addEventListener('input', renderComplexity);
  $('#cx-exp').addEventListener('change', () => {
    if ($('#cx-exp').checked) DATA.complexity.constructs.forEach(c => CX.open.add(c.id));
    else CX.open.clear();
    renderComplexity();
  });
  $('#cx-notconstruct').innerHTML = (DATA.complexity.not_a_construct || []).map(s =>
    '<p>' + esc(s) + '</p>').join('');
  renderComplexity();
}

function renderComplexity() {
  CX.status = {}; $$('#ctx-filters [data-cxs]').forEach(c => CX.status[c.dataset.cxs] = c.checked);
  const q = ($('#cx-q').value || '').trim().toLowerCase();
  const off = Object.keys(CX.status).filter(k => CX.status[k] === false);
  const list = DATA.complexity.constructs.filter(c => {
    if (off.includes(c.status)) return false;
    if (!q) return true;
    return (c.name + ' ' + c.plain_english + ' ' + JSON.stringify(c.placements)).toLowerCase().includes(q);
  });
  $('#cx-count').textContent = list.length + ' of ' + DATA.complexity.constructs.length + ' constructs · ' +
    list.reduce((a, c) => a + c.placements.length, 0) + ' placements shown';
  $('#cx-cards').innerHTML = list.map(c => {
    const open = CX.open.has(c.id);
    const met = String(c.status).toUpperCase() === 'MET';
    const over = (c.as_built_count || 0) > (c.required_count || 0);
    return '<article class="cx-card' + (open ? ' open' : '') + '" data-cx="' + c.id + '" tabindex="0" ' +
      'role="button" aria-expanded="' + open + '">' +
      '<div class="cx-head"><span class="cx-id">' + String(c.id).padStart(2, '0') + '</span>' +
      '<span class="cx-name">' + hlq(c.name, q) + '</span></div>' +
      '<div class="cx-plain">' + hlq(c.plain_english || '', q) + '</div>' +
      '<div class="cx-counts">' +
        '<span class="badge">required ' + n0(c.required_count) + '</span>' +
        '<span class="badge ' + (over ? 'info' : '') + '">as built ' + n0(c.as_built_count) + '</span>' +
        '<span class="badge ' + (met ? 'ok' : 'warn') + '">' + esc(c.status) + '</span>' +
        '<span class="badge">' + c.placements.length + ' placements</span></div>' +
      '<div class="cx-places">' +
        (c.as_built_unit ? '<div class="dim" style="font-size:10.5px;margin-bottom:7px">' +
          esc(c.as_built_unit) + (c.required_basis ? ' · ' + esc(c.required_basis) : '') + '</div>' : '') +
        (c.placements.length ? c.placements.map(p =>
          '<div class="place"><div class="pf mono">' + esc(p.file || '—') + '</div>' +
          '<div class="pp mono">' + esc(p.program || '') + (p.paragraph ? ' · ' + esc(p.paragraph) : '') + '</div>' +
          (p.note ? '<div class="pn">' + esc(p.note) + '</div>' : '') +
          (p.verification ? '<span class="badge ' + (p.verification === 'PASS' ? 'ok' :
            (p.verification === 'PARTIAL' ? 'warn' : 'info')) + '" style="margin-top:4px">' +
            esc(p.verification) + '</span>' : '') + '</div>').join('')
          : '<div class="dim">No individual placements catalogued for this construct.</div>') +
      '</div></article>';
  }).join('');
  $$('#cx-cards .cx-card').forEach(el => {
    const t = () => { const id = +el.dataset.cx;
      if (CX.open.has(id)) CX.open.delete(id); else CX.open.add(id); renderComplexity(); };
    el.addEventListener('click', t);
    el.addEventListener('keydown', e => { if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); t(); } });
  });
}

/* ══════════════════════════════════════════════════════════════════
   6. CAST findings
   ══════════════════════════════════════════════════════════════════ */
function renderCast() {
  $('#cast-table').innerHTML = table(['Finding class', 'Expected'],
    DATA.cast.expected_table.map(r => [esc(r.finding), '<span class="mono">' + esc(r.count) + '</span>']));
  $('#cast-expected').innerHTML = DATA.cast.expected_sections.map(s =>
    '<div class="dblock" style="margin-bottom:9px"><h4>' + esc(s.title) + '</h4>' +
    '<p style="font-size:11px;color:#4a5568;line-height:1.6">' + esc(s.lead) + '</p>' +
    (s.bullets.length ? '<ul style="margin:6px 0 0 15px;font-size:10.5px;color:#5a6478;line-height:1.6">' +
      s.bullets.map(b => '<li>' + esc(b) + '</li>').join('') + '</ul>' : '') + '</div>').join('');
  $('#cast-misses').innerHTML = DATA.cast.misses.map(s =>
    '<div class="dblock" style="border-color:#fde8e8"><h4 style="color:#991b1b">' + esc(s.title) + '</h4>' +
    '<p style="font-size:11px;color:#4a5568;line-height:1.6">' + esc(s.lead) + '</p>' +
    (s.quote ? '<p style="font-size:10.5px;color:#5a6478;border-left:2px solid #fde8e8;padding-left:9px;' +
      'margin-top:7px;line-height:1.6">' + esc(s.quote) + '</p>' : '') +
    (s.bullets.length ? '<ul style="margin:7px 0 0 15px;font-size:10.5px;color:#5a6478;line-height:1.6">' +
      s.bullets.map(b => '<li>' + esc(b) + '</li>').join('') + '</ul>' : '') + '</div>').join('');
}

/* ══════════════════════════════════════════════════════════════════
   7. seeded defects
   ══════════════════════════════════════════════════════════════════ */
function renderDefects() {
  $('#def-note').textContent = DATA.complexity.seeded_defect_note || '';
  /* the heading counts the rows rather than restating a number that then rots */
  $('#def-count').textContent = 'All ' + DATA.defects.length;
  $('#def-table').innerHTML = table(['ID', 'File', 'Detectable by', 'Answer key'],
    DATA.defects.map(d => {
      const det = d.detectable_by;
      const detHtml = Array.isArray(det)
        ? det.map(x => '<div style="margin-bottom:3px">' + esc(typeof x === 'string' ? x : JSON.stringify(x)) + '</div>').join('')
        : (det && typeof det === 'object'
            ? '<dl class="io-kv" style="grid-template-columns:118px 1fr">' + Object.keys(det).map(k =>
                '<dt>' + esc(k) + '</dt><dd>' + esc(typeof det[k] === 'string' ? det[k] : JSON.stringify(det[k])) +
                '</dd>').join('') + '</dl>'
            : esc(det || '—'));
      return ['<span class="badge bad">' + esc(d.id) + '</span>',
        '<span class="mono">' + esc(d.file) + '</span>',
        '<div style="font-size:10.5px;color:#4a5568;line-height:1.55">' + detHtml + '</div>',
        '<span class="mono dim">' + esc(d.key_file) + '</span>'];
    }));
}

/* ══════════════════════════════════════════════════════════════════
   8. comparison report viewer
   ══════════════════════════════════════════════════════════════════ */
let RP = null;
const RP_EMPTY =
  '<div class="empty"><h4>No comparison report loaded</h4>' +
  '<p style="max-width:82ch;margin:0 auto 12px;line-height:1.7">Produce one with the harness, then load the ' +
  'JSON here. The blind run refuses the answer key and the signature file; the attributed run applies the key ' +
  'afterwards. The variance lists are identical — only the verdicts change, and the difference between the two ' +
  'reports is the score.</p>' +
  '<pre class="code" style="text-align:left;max-width:78ch;margin:0 auto">cd HARNESS\n' +
  'python3 run_compare.py --legacy L --candidate C --blind --out ../DATA/blind\n' +
  'python3 run_compare.py --legacy L --candidate C         --out ../DATA/attributed</pre>' +
  '<p style="max-width:82ch;margin:12px auto 0;text-align:left;line-height:1.7">Expected shape ' +
  '(<span class="mono">report_schema: cabs.comparison.v1</span>):</p>' +
  '<pre class="code" style="text-align:left;max-width:78ch;margin:8px auto 0">{\n' +
  '  "report_schema": "cabs.comparison.v1",\n' +
  '  "overall_verdict": "...",\n' +
  '  "score":  { "defects_detected": [...], "defects_missed": [...] },\n' +
  '  "money":  { "net_by_verdict": {...}, "absolute_by_verdict": {...},\n' +
  '              "net_overall": "...", "net_by_field": {...} },\n' +
  '  "levels": [ { "level": "L1", "name": "...", "ran": true,\n' +
  '                "verdict": "MATCH|DIVERGENT|DIVERGENT-BY-DESIGN|NOT RUN",\n' +
  '                "records_compared": 0, "verdict_counts": {...} } ],\n' +
  '  "variance_groups": [ { "level":"L2","field":"...","count":0,"verdicts":{...} } ]\n}</pre></div>';

function initCompare() {
  $('#rp-out').innerHTML = RP_EMPTY;
  $('#rp-file').addEventListener('change', ev => {
    const f = ev.target.files && ev.target.files[0];
    if (!f) return;
    const rd = new FileReader();
    rd.onload = () => loadReport(rd.result, f.name);
    rd.readAsText(f);
  });
  $('#rp-paste-btn').addEventListener('click', () => {
    const p = $('#rp-paste'); p.style.display = p.style.display === 'none' ? 'block' : 'none';
  });
  $('#rp-parse').addEventListener('click', () => loadReport($('#rp-text').value, 'pasted JSON'));
  $('#rp-clear').addEventListener('click', () => {
    RP = null; $('#rp-out').innerHTML = RP_EMPTY; $('#rp-status').textContent = '';
    $('#rp-text').value = ''; $('#rp-file').value = '';
  });
}

function loadReport(txt, name) {
  let j;
  try { j = JSON.parse(txt); }
  catch (e) { $('#rp-status').innerHTML = '<span class="badge bad">not valid JSON — ' + esc(e.message) + '</span>'; return; }
  if (!j || typeof j !== 'object') { $('#rp-status').innerHTML = '<span class="badge bad">not an object</span>'; return; }
  RP = j;
  const schema = j.report_schema || '(no report_schema)';
  $('#rp-status').innerHTML = '<span class="badge ' + (schema === 'cabs.comparison.v1' ? 'ok' : 'warn') + '">' +
    esc(name) + ' · ' + esc(schema) + '</span>';
  renderReport();
}

function renderReport() {
  const j = RP; if (!j) return;
  const sc = j.score || {}, money = j.money || {}, levels = j.levels || [];
  const totals = {};
  levels.forEach(l => Object.keys(l.verdict_counts || {}).forEach(k =>
    totals[k] = (totals[k] || 0) + (l.verdict_counts[k] || 0)));
  const vClass = v => /BY-DESIGN/i.test(v) ? 'info' : (/^MATCH/i.test(v) ? 'ok' :
    (/NOT RUN/i.test(v) ? 'warn' : 'bad'));

  let h = '<div class="kpis">' +
    '<div class="kpi"><div class="v">' + esc(j.overall_verdict || '—') + '</div>' +
    '<div class="l">Overall verdict</div><div class="s">' + esc(j.generated_at_utc || '') + '</div></div>' +
    ['MATCH','DIVERGENT','DIVERGENT-BY-DESIGN'].map(k =>
      '<div class="kpi ' + (k === 'DIVERGENT' ? 'alert' : (k === 'MATCH' ? 'good' : '')) + '">' +
      '<div class="v">' + n0(totals[k] || 0) + '</div><div class="l">' + k + '</div>' +
      '<div class="s">summed across all levels</div></div>').join('') +
    '<div class="kpi"><div class="v">' + esc(money.net_overall != null ? money.net_overall : '—') + '</div>' +
    '<div class="l">Net penny variance</div><div class="s">signed, scaled fields only</div></div>' +
    '<div class="kpi"><div class="v">' + esc(money.absolute_overall != null ? money.absolute_overall : '—') + '</div>' +
    '<div class="l">Absolute variance</div><div class="s">' +
      n0(money.variances_carrying_a_delta) + ' variances carry a delta</div></div>' +
    '</div>';

  h += '<div class="grid2"><div class="panel"><h3>Per-level verdicts</h3>' +
    tableEl(['Level','Name','Ran','Verdict','Records','Fields','Variances','Counts'],
      levels.map(l => ['<span class="badge acc">' + esc(l.level) + '</span>', esc(l.name || ''),
        l.ran ? '<span class="badge ok">yes</span>' : '<span class="badge warn">no — ' +
          esc(l.skipped_reason || 'skipped') + '</span>',
        '<span class="badge ' + vClass(l.verdict) + '">' + esc(l.verdict) + '</span>',
        n0(l.records_compared), n0(l.fields_compared), n0(l.variance_count),
        Object.keys(l.verdict_counts || {}).map(k => '<span class="badge ' + vClass(k) + '">' +
          esc(k) + ' ' + n0(l.verdict_counts[k]) + '</span>').join(' ')]),
      {num: [4,5,6]}) + '</div>';

  const det = sc.defects_detected || sc.detected || [];
  const mis = sc.defects_missed || sc.missed || [];
  const known = DATA.defects.map(d => d.id);
  const norm = a => (a || []).map(x => typeof x === 'string' ? x : (x && (x.id || x.defect_id)) || String(x));
  const dset = new Set(norm(det)), mset = new Set(norm(mis));
  h += '<div class="panel"><h3>Seeded defect recall</h3>' +
    '<p>Detected ' + dset.size + ' of ' + known.length + '. A missed defect is a statement about the harness, ' +
    'not about the candidate.</p>' +
    tableEl(['Defect','File','Result'], DATA.defects.map(d => [
      '<span class="badge">' + esc(d.id) + '</span>',
      '<span class="mono dim">' + esc(d.file) + '</span>',
      dset.has(d.id) ? '<span class="badge ok">detected</span>' :
      (mset.has(d.id) ? '<span class="badge bad">missed</span>' :
       '<span class="badge warn">not reported</span>')])) + '</div></div>';

  if (money.net_by_verdict) {
    h += '<div class="grid2"><div class="panel"><h3>Penny variance by verdict</h3>' +
      tableEl(['Verdict','Net','Absolute'], Object.keys(money.net_by_verdict).map(k =>
        ['<span class="badge ' + vClass(k) + '">' + esc(k) + '</span>',
         '<span class="mono">' + esc(money.net_by_verdict[k]) + '</span>',
         '<span class="mono">' + esc((money.absolute_by_verdict || {})[k] || '—') + '</span>'])) +
      (money.note ? '<p style="margin-top:8px">' + esc(money.note) + '</p>' : '') + '</div>';
    const nbf = money.net_by_field || {};
    h += '<div class="panel"><h3>Largest net movements by field</h3><div class="scroll-y" style="max-height:280px">' +
      tableEl(['Field','Net'], Object.keys(nbf).slice(0, 40).map(k =>
        ['<span class="mono">' + esc(k) + '</span>', '<span class="mono">' + esc(nbf[k]) + '</span>'])) +
      '</div></div></div>';
  }

  const vg = j.variance_groups || [];
  if (vg.length) {
    h += '<div class="panel"><h3>Variance groups <span class="sub">' + vg.length + ' signatures</span></h3>' +
      '<div class="scroll-y" style="max-height:340px">' +
      tableEl(['Level','Kind','Layout','Field','Count','Net delta','Verdicts','Defects'],
        vg.slice(0, 300).map(g => ['<span class="badge acc">' + esc(g.level) + '</span>', esc(g.kind || ''),
          esc(g.layout || ''), '<span class="mono">' + esc(g.field || '') + '</span>', n0(g.count),
          '<span class="mono">' + esc(g.net_delta != null ? g.net_delta : '') + '</span>',
          Object.keys(g.verdicts || {}).map(k => '<span class="badge ' + vClass(k) + '">' + esc(k) + ' ' +
            n0(g.verdicts[k]) + '</span>').join(' '),
          (g.defect_ids || []).map(d => '<span class="badge bad">' + esc(d) + '</span>').join(' ')]),
        {num: [4]}) + '</div></div>';
  }
  if (j.datasets) {
    h += '<div class="panel"><h3>Datasets compared</h3><pre class="code">' +
      esc(JSON.stringify(j.datasets, null, 2).slice(0, 4000)) + '</pre></div>';
  }
  $('#rp-out').innerHTML = h;
}

/* ══════════════════════════════════════════════════════════════════
   search + boot
   ══════════════════════════════════════════════════════════════════ */
function wireSearch() {
  const box = $('#search'), clear = $('#clear-search');
  const apply = () => {
    const v = box.value;
    clear.style.display = v ? 'block' : 'none';
    if (CURRENT === 'graph') { G.q = v; if (G.inited) highlight(); }
    else if (CURRENT === 'contracts') { $('#cn-q').value = v; CN.limit = 120; renderContracts(); }
    else if (CURRENT === 'complexity') { $('#cx-q').value = v; renderComplexity(); }
    else if (CURRENT === 'defects' || CURRENT === 'cast' || CURRENT === 'overview') {
      const host = $('#sec-' + CURRENT);
      $$('tbody tr', host).forEach(tr => {
        tr.style.display = !v || tr.textContent.toLowerCase().includes(v.toLowerCase()) ? '' : 'none';
      });
    }
  };
  box.addEventListener('input', apply);
  clear.addEventListener('click', () => { box.value = ''; apply(); box.focus(); });
  document.addEventListener('keydown', e => {
    if ((e.ctrlKey || e.metaKey) && e.key === 'k') { e.preventDefault(); box.focus(); box.select(); }
    if (e.key === 'Escape' && document.activeElement === box) { box.value = ''; apply(); box.blur(); }
  });
}

let rsz;
window.addEventListener('resize', () => {
  clearTimeout(rsz);
  rsz = setTimeout(() => {
    if (CURRENT === 'graph' && G.inited) { G.sim && G.sim.alpha(0.15).restart(); fitGraph(); }
    if (CURRENT === 'lineage') drawLineage();
  }, 220);
});

(function boot() {
  buildNav();
  renderOverview();
  initContracts();
  initComplexity();
  renderCast();
  renderDefects();
  initCompare();
  wireSearch();
  show('overview');
})();
</script>
</body>
</html>
"""


if __name__ == "__main__":
    main()
