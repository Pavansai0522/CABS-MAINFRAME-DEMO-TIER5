#!/usr/bin/env python3
"""
verify_syntax.py - build gate for the CABS Tier 5 estate.

Re-runnable.  Reads the tree; writes nothing.  Exits non-zero if any
blocking check fails.

    python3 BUILDER/verify_syntax.py
    python3 BUILDER/verify_syntax.py --baseline /path/to/pre-repair/copy

The seven checks:

  1  no source line extends past its format limit - COBOL code column 72,
     JCL statements column 71, instream card images column 80
  2  no data description entry is left without a terminating period
  3  no alphanumeric literal is left unterminated (continuation aware)
  4  CABING08 CF-VARIANT-VOICE computes to exactly 96 bytes
  5  no EVALUATE / INITIALIZE / scope terminator / inline PERFORM /
     reference modification outside the Enterprise COBOL exemption.  The
     exempt folders and files are read from the ENTERPRISE-COBOL-EXEMPTION
     table in CONVENTIONS.md, not hardcoded here, and hits are reported as
     exempt or violating separately.  Only violations block.  Every exempt
     program must also carry a 'COMPILER : ENTERPRISE COBOL' header line.
  6  every program still carries P0000-MAINLINE, P8000-CONTROL and
     COPY CABSWRK where it carried them before
  7  per-file changed-line counts, and no PROCEDURE DIVISION statement
     altered  (both require --baseline)

Check 1 is reported split between code lines and comment lines.  Only the
code-line count is blocking: columns 73-80 are the sequence area, so an
over-long comment is a convention breach rather than a compile error, and
the estate carries a documented population of them.
"""
import os, re, sys, argparse

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)

SRC_EXT = ('.cbl', '.cpy', '.jcl', '.prc', '.ctl')
COB_EXT = ('.cbl', '.cpy')

# The Enterprise COBOL exemption is declared in CONVENTIONS.md, between the
# ENTERPRISE-COBOL-EXEMPTION markers, and is read from there rather than
# hardcoded here.  A row whose path ends in '/' exempts a whole folder; any
# other row exempts one file.
CONVENTIONS = os.path.join(ROOT, 'CONVENTIONS.md')
EXEMPT_START = '<!-- ENTERPRISE-COBOL-EXEMPTION -->'
EXEMPT_END = '<!-- END ENTERPRISE-COBOL-EXEMPTION -->'


def load_exemptions(path=None):
    """Parse the exemption table out of CONVENTIONS.md.

    Returns (dirs, files, source) where source is the path actually read, so
    the gate can say where its policy came from.  Raises if the markers are
    missing - an unreadable policy must not silently become an empty one.
    """
    path = path or CONVENTIONS
    with open(path, encoding='utf-8') as fh:
        text = fh.read()
    if EXEMPT_START not in text or EXEMPT_END not in text:
        raise SystemExit(
            "verify_syntax: %s carries no %s block; check 5 has no policy to "
            "apply and will not guess one." % (rel(path), EXEMPT_START))
    block = text.split(EXEMPT_START, 1)[1].split(EXEMPT_END, 1)[0]
    dirs, files = [], []
    for line in block.split('\n'):
        line = line.strip()
        if not line.startswith('|'):
            continue
        cells = [c.strip() for c in line.strip('|').split('|')]
        if len(cells) < 2:
            continue
        p = cells[0].strip('`').strip()
        if not p or p.lower() == 'path' or set(p) <= set('-: '):
            continue
        (dirs if p.endswith('/') else files).append(p)
    if not dirs and not files:
        raise SystemExit(
            "verify_syntax: the %s block in %s is empty." % (EXEMPT_START, rel(path)))
    return tuple(dirs), tuple(files), path

LEVELS = set(range(1, 50)) | {66, 77, 88}
DIV_RE = re.compile(r'\b(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION', re.I)
LEVEL_START_RE = re.compile(r'^\s*(\d{2})(\s|$)')
SECT_RE = re.compile(r'^\s*(WORKING-STORAGE|LINKAGE|FILE|LOCAL-STORAGE|REPORT|COMMUNICATION)\s+SECTION', re.I)
FD_RE = re.compile(r'^\s*(FD|SD|RD|CD)\s+', re.I)
COPY_RE = re.compile(r'^\s*(COPY|EJECT|SKIP\d?)\b', re.I)


# ------------------------------------------------------------------ lexing
def code_area(line):
    return line[7:72] if len(line) > 7 else ''


def indicator(line):
    return line[6] if len(line) > 6 else ' '


def is_comment(line):
    return indicator(line) in ('*', '/')


def is_continuation(line):
    return indicator(line) == '-'


def is_blank(line):
    return not line.strip()


def scan_quotes(body, inlit=False, q=None, cont=False):
    """Return (period_positions, inlit, q).  Handles doubled quotes and
    fixed-format literal continuation: on a line with '-' in column 7 the
    first quote in Area B resumes the open literal, it does not close it."""
    periods = []
    i = 0
    if cont and inlit:
        j = 0
        while j < len(body) and body[j] == ' ':
            j += 1
        if j < len(body) and body[j] == q:
            i = j + 1
    n = len(body)
    while i < n:
        ch = body[i]
        if inlit:
            if ch == q:
                if i + 1 < n and body[i + 1] == q:
                    i += 2
                    continue
                inlit = False
                q = None
        else:
            if ch in ("'", '"'):
                inlit = True
                q = ch
            elif ch == '.':
                periods.append(i)
        i += 1
    return periods, inlit, q


def starts_entry(line):
    m = LEVEL_START_RE.match(code_area(line))
    if not m:
        return None
    lvl = int(m.group(1))
    return lvl if lvl in LEVELS else None


def divisions(lines):
    div = None
    for i, line in enumerate(lines):
        if not is_comment(line) and not is_blank(line):
            m = DIV_RE.search(code_area(line))
            if m:
                div = m.group(1).upper()
        yield i, line, div


def sources(exts, root=None):
    root = root or ROOT
    out = []
    for dp, dn, fn in os.walk(root):
        if '__pycache__' in dp:
            continue
        for f in fn:
            if f.lower().endswith(exts):
                out.append(os.path.join(dp, f))
    return sorted(out)


def read(path):
    return open(path, encoding='utf-8', errors='replace').read().split('\n')


def rel(path, root=None):
    return os.path.relpath(path, root or ROOT).replace(os.sep, '/')


def is_exempt(r, dirs, files):
    return any(r.startswith(d) for d in dirs) or r in files


def in_runnable_core(r, dirs=None, files=None):
    if dirs is None or files is None:
        dirs, files, _ = load_exemptions()
    return r.endswith('.cbl') and not is_exempt(r, dirs, files)


# ------------------------------------------------------------------ checks
def classify_over(r, line):
    """Which population an over-length line belongs to.

    cobol_comment / jcl_comment  cols 73-80 are the sequence area - a
                                 convention breach, not a compile error
    cobol_code                   the compiler drops column 73 onward, so the
                                 statement is truncated: BLOCKING
    jcl_stmt                     a JCL statement must close by column 71,
                                 column 72 being the continuation indicator:
                                 BLOCKING
    jcl_card                     instream data after DD * - an 80-byte card
                                 image, so 73-80 is legitimate payload and
                                 only past 80 is a defect
    """
    if r.endswith(COB_EXT):
        return 'cobol_comment' if is_comment(line) else 'cobol_code'
    if line.startswith('//*') or line.lstrip().startswith('*'):
        return 'jcl_comment'
    if line.startswith('//'):
        return 'jcl_stmt'
    return 'jcl_card'


def check_1():
    pops = {}
    for p in sources(SRC_EXT):
        r = rel(p)
        instream = False
        for i, line in enumerate(read(p)):
            line = line.rstrip()
            if r.endswith(('.jcl', '.prc')):
                if re.match(r'^//\S*\s+DD\s+\*', line) or re.match(r'^//\S*\s+DD\s+DATA', line):
                    instream = True
                elif line.startswith('/*') or line.startswith('//'):
                    instream = False
            L = len(line)
            # A JCL statement must close by column 71 - column 72 is the
            # continuation indicator, so a data character there is read as
            # the indicator and lost from the operand.  COBOL and card
            # images are only in breach past 72 and 80 respectively.
            if L <= 71:
                continue
            k = classify_over(r, line)
            if k == 'jcl_card' and not instream:
                k = 'jcl_stmt'
            if k != 'jcl_stmt' and L <= 72:
                continue
            if k == 'jcl_card' and L <= 80:
                k = 'jcl_card_ok'
            pops.setdefault(k, []).append((r, i + 1, L, line))
    return pops


BLOCKING_OVER = ('cobol_code', 'jcl_stmt', 'jcl_card')


def unterminated_entries(lines):
    res, pending, last = [], None, None
    inlit, q = False, None
    for i, line, div in divisions(lines):
        if is_comment(line) or is_blank(line):
            continue
        if div not in ('DATA', None):
            if pending is not None:
                res.append((pending, last))
            pending = last = None
            inlit, q = False, None
            continue
        body = code_area(line)
        lvl = starts_entry(line)
        other = bool(SECT_RE.match(body) or FD_RE.match(body)
                     or COPY_RE.match(body) or DIV_RE.search(body))
        if (lvl is not None or other) and not is_continuation(line) and not inlit:
            if pending is not None:
                res.append((pending, last))
            pending = last = None
        periods, inlit, q = scan_quotes(body, inlit, q, is_continuation(line))
        if lvl is not None and pending is None and not other:
            if not periods:
                pending = last = i
        elif pending is not None:
            last = i
            if periods:
                pending = last = None
    if pending is not None:
        res.append((pending, last))
    return res


def check_2():
    bad = []
    for p in sources(COB_EXT):
        lines = read(p)
        for first, _ in unterminated_entries(lines):
            bad.append((rel(p), first + 1, lines[first].rstrip()))
    return bad


def check_3():
    bad = []
    for p in sources(COB_EXT):
        lines = read(p)
        inlit, q, start = False, None, None
        for i, line in enumerate(lines):
            if is_comment(line) or is_blank(line):
                continue
            cont = is_continuation(line)
            if inlit and not cont:
                bad.append((rel(p), start + 1, lines[start].rstrip()))
                inlit, q = False, None
            _, inlit, q = scan_quotes(code_area(line), inlit, q, cont)
            start = i if inlit else None
        if inlit:
            bad.append((rel(p), start + 1, lines[start].rstrip()))
    return bad


PIC_RE = re.compile(r'PIC(?:TURE)?\s+(?:IS\s+)?(\S+)', re.I)


def pic_bytes(pic, usage):
    """Storage size of an elementary item.  Adequate for the layouts here."""
    expanded = re.sub(r'([9XAZPSV])\((\d+)\)',
                      lambda m: m.group(1) * int(m.group(2)), pic.upper())
    digits = expanded.count('9') + expanded.count('Z') + expanded.count('P')
    u = (usage or '').upper()
    if 'COMP-3' in u or 'PACKED' in u:
        return digits // 2 + 1
    if 'COMP-1' in u:
        return 4
    if 'COMP-2' in u:
        return 8
    if 'COMP' in u:
        return 2 if digits <= 4 else (4 if digits <= 9 else 8)
    return sum(1 for c in expanded if c not in 'SV')


def check_4():
    p = os.path.join(ROOT, 'BATCH', 'INGEST', 'CABING08.cbl')
    lines = read(p)
    target, members, seen = None, [], False
    for i, line in enumerate(lines):
        if is_comment(line) or is_blank(line):
            continue
        body = code_area(line).strip()
        if 'CF-VARIANT-AREA' in body and 'REDEFINES' not in body.upper():
            m = PIC_RE.search(body)
            if m:
                target = pic_bytes(m.group(1).rstrip('.'), body)
        if 'CF-VARIANT-VOICE' in body and 'REDEFINES' in body.upper():
            seen = True
            continue
        if seen:
            lvl = starts_entry(line)
            if lvl is not None and lvl <= 5:
                break
            m = PIC_RE.search(body)
            if m:
                name = body.split()[1]
                members.append((name, pic_bytes(m.group(1).rstrip('.'), body)))
    return target, members, sum(b for _, b in members)


BANNED = [
    ('EVALUATE', re.compile(r'\bEVALUATE\b')),
    ('END-EVALUATE', re.compile(r'\bEND-EVALUATE\b')),
    ('INITIALIZE', re.compile(r'\bINITIALIZE\b')),
    ('END-IF', re.compile(r'\bEND-IF\b')),
    ('END-READ', re.compile(r'\bEND-READ\b')),
    ('END-PERFORM', re.compile(r'\bEND-PERFORM\b')),
    ('END-CALL', re.compile(r'\bEND-CALL\b')),
    ('END-WRITE', re.compile(r'\bEND-WRITE\b')),
    ('END-SEARCH', re.compile(r'\bEND-SEARCH\b')),
    ('END-COMPUTE', re.compile(r'\bEND-COMPUTE\b')),
    ('END-STRING', re.compile(r'\bEND-STRING\b')),
    ('END-UNSTRING', re.compile(r'\bEND-UNSTRING\b')),
    ('END-ADD', re.compile(r'\bEND-ADD\b')),
    ('END-SUBTRACT', re.compile(r'\bEND-SUBTRACT\b')),
    ('END-MULTIPLY', re.compile(r'\bEND-MULTIPLY\b')),
    ('END-DIVIDE', re.compile(r'\bEND-DIVIDE\b')),
    ('END-START', re.compile(r'\bEND-START\b')),
    ('END-RETURN', re.compile(r'\bEND-RETURN\b')),
    ('refmod', re.compile(r'[A-Z0-9\-]\s*\(\s*[A-Z0-9\-]+\s*:\s*[A-Z0-9\-]*\s*\)')),
]


COMPILER_DECL = re.compile(r'^\s*\*\s*COMPILER\s*:\s*ENTERPRISE COBOL', re.I)


def check_5(root=None):
    """Banned constructs, split by the CONVENTIONS.md exemption.

    Returns (violating, exempt, undeclared, source):
      violating   {construct: [(file, line)]} outside the exempt set - blocking
      exempt      {construct: [(file, line)]} inside it - reported, not blocking
      undeclared  exempt .cbl carrying no 'COMPILER : ENTERPRISE COBOL' header
      source      the policy file the exemption list was read from
    """
    dirs, files, source = load_exemptions()
    violating, exempt, undeclared = {}, {}, []
    for p in sources(('.cbl',), root):
        r = rel(p, root)
        lines = read(p)
        if is_exempt(r, dirs, files):
            bucket = exempt
            if not any(COMPILER_DECL.match(l[6:]) for l in lines if is_comment(l)):
                undeclared.append(r)
        else:
            bucket = violating
        for i, line in enumerate(lines):
            if is_comment(line) or is_blank(line):
                continue
            body = code_area(line).upper()
            for name, rx in BANNED:
                if rx.search(body):
                    bucket.setdefault(name, []).append((r, i + 1))
    return violating, exempt, undeclared, source


ANCHORS = ('P0000-MAINLINE', 'P8000-CONTROL', 'COPY CABSWRK')


def anchors_of(path):
    text = '\n'.join(l for l in read(path) if not is_comment(l))
    return tuple(a in text for a in ANCHORS)


def check_6(baseline=None):
    now = {rel(p): anchors_of(p) for p in sources(('.cbl',))}
    if not baseline:
        return now, None
    was = {rel(p, baseline): anchors_of(p) for p in sources(('.cbl',), baseline)}
    lost = {r: (was[r], now[r]) for r in was
            if r in now and any(w and not n for w, n in zip(was[r], now[r]))}
    return now, lost


def proc_statements(lines):
    """Non-comment PROCEDURE DIVISION source, whitespace-normalised."""
    out = []
    for i, line, div in divisions(lines):
        if div == 'PROCEDURE' and not is_comment(line) and not is_blank(line):
            out.append(' '.join(code_area(line).split()))
    return out


def check_7(baseline):
    rows, proc_diffs = [], []
    for p in sources(('.cbl',), baseline):
        r = rel(p, baseline)
        cur = os.path.join(ROOT, r)
        if not os.path.exists(cur):
            continue
        a, b = read(p), read(cur)
        n = sum(1 for x, y in zip(a, b) if x != y) + abs(len(a) - len(b))
        if n:
            rows.append((r, n, len(b) - len(a)))
        pa, pb = proc_statements(a), proc_statements(b)
        if pa != pb:
            proc_diffs.append((r, [(x, y) for x, y in zip(pa, pb) if x != y][:5],
                               len(pb) - len(pa)))
    return sorted(rows, key=lambda x: -x[1]), proc_diffs


# ------------------------------------------------------------------- main
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--baseline', default=None,
                    help='pre-repair copy of the tree, for checks 6 and 7')
    a = ap.parse_args()
    fail = 0

    print("=" * 74)
    print("CABS Tier 5 syntax gate   root: %s" % ROOT)
    print("=" * 74)

    pops = check_1()
    blocking = sum(len(pops.get(k, [])) for k in BLOCKING_OVER)
    print("\n[1] lines past their format limit  COBOL code 72 / JCL stmt 71 / card 80")
    order = ['cobol_code', 'jcl_stmt', 'jcl_card', 'jcl_card_ok',
             'cobol_comment', 'jcl_comment']
    for k in order:
        v = pops.get(k, [])
        mark = '  BLOCKING' if k in BLOCKING_OVER and v else ''
        print("    %-14s %4d%s" % (k, len(v), mark))
    print("    total %d, blocking %d   %s"
          % (sum(len(v) for v in pops.values()), blocking,
             "OK" if blocking == 0 else "FAIL"))
    for k in BLOCKING_OVER:
        for d in pops.get(k, [])[:40]:
            print("        [%s] %s:%d len=%d" % (k, d[0], d[1], d[2]))
    if blocking:
        fail += 1

    bad2 = check_2()
    print("\n[2] data description entries with no terminating period")
    print("    count: %d   %s" % (len(bad2), "OK" if not bad2 else "FAIL"))
    for b in bad2[:20]:
        print("        %s:%d |%s|" % b)
    if bad2:
        fail += 1

    bad3 = check_3()
    print("\n[3] unterminated alphanumeric literals")
    print("    count: %d   %s" % (len(bad3), "OK" if not bad3 else "FAIL"))
    for b in bad3[:20]:
        print("        %s:%d |%s|" % b)
    if bad3:
        fail += 1

    target, members, total = check_4()
    print("\n[4] CABING08 CF-VARIANT-VOICE against CF-VARIANT-AREA")
    for name, b in members:
        print("        %-24s %3d" % (name, b))
    print("        %-24s %3d  target %s   %s"
          % ('TOTAL', total, target,
             "OK" if total == target else "FAIL"))
    if total != target:
        fail += 1

    violating, exempt, undeclared, source = check_5()
    dirs, files, _ = load_exemptions()
    print("\n[5] banned constructs, against the exemption declared in %s"
          % rel(source))
    print("    exempt set: %d folder(s) %s + %d file(s)"
          % (len(dirs), ' '.join(dirs), len(files)))
    for f in files:
        print("                %s" % f)

    n_ex = sum(len(v) for v in exempt.values())
    print("\n    EXEMPT (Enterprise COBOL, permitted) - %d occurrence(s)" % n_ex)
    if not exempt:
        print("        none")
    else:
        by_dir = {}
        for k, v in exempt.items():
            for r, ln in v:
                by_dir[os.path.dirname(r) or '.'] = by_dir.get(os.path.dirname(r) or '.', 0) + 1
        for k, v in sorted(exempt.items()):
            print("        %-14s %3d" % (k, len(v)))
        for d, n in sorted(by_dir.items(), key=lambda x: -x[1]):
            print("        by location   %-24s %4d" % (d, n))
    print("    exempt programs with no COMPILER header line: %d   %s"
          % (len(undeclared), "OK" if not undeclared else "FAIL"))
    for r in undeclared[:20]:
        print("        %s" % r)
    if undeclared:
        fail += 1

    n_vi = sum(len(v) for v in violating.values())
    print("\n    VIOLATING (OS/VS COBOL, not permitted) - %d occurrence(s)   %s"
          % (n_vi, "OK" if not violating else "FAIL"))
    for k, v in sorted(violating.items()):
        print("        %-14s %3d   %s" % (k, len(v),
              ', '.join('%s:%d' % x for x in v[:6])))
    if violating:
        fail += 1

    if a.baseline:
        base_vi, _, _, _ = check_5(a.baseline)
        # the baseline holds only the files this repair touched, so the
        # comparison is restricted to that same set
        scope = {rel(p, a.baseline) for p in sources(('.cbl',), a.baseline)}
        vi_s = {k: [x for x in v if x[0] in scope] for k, v in violating.items()}
        new = {k: len(v) - len(base_vi.get(k, []))
               for k, v in vi_s.items() if len(v) > len(base_vi.get(k, []))}
        print("    violations introduced by this repair: %s"
              % (new if new else "none   OK"))
        if new:
            fail += 1

    now, lost = check_6(a.baseline)
    print("\n[6] structural anchors  %s" % ' / '.join(ANCHORS))
    tot = len(now)
    for j, name in enumerate(ANCHORS):
        print("    %-16s present in %d of %d .cbl" % (name, sum(1 for v in now.values() if v[j]), tot))
    if a.baseline:
        print("    anchors lost against baseline: %d   %s"
              % (len(lost), "OK" if not lost else "FAIL"))
        for r, (w, n) in list(lost.items())[:10]:
            print("        %s  was %s now %s" % (r, w, n))
        if lost:
            fail += 1
    else:
        print("    (pass --baseline to prove none were lost)")

    print("\n[7] diff against baseline")
    if not a.baseline:
        print("    skipped - no --baseline given")
    else:
        rows, proc_diffs = check_7(a.baseline)
        print("    files changed: %d    lines changed: %d"
              % (len(rows), sum(r[1] for r in rows)))
        print("    %-36s %7s %7s" % ("file", "lines", "delta"))
        for r, n, d in rows:
            print("    %-36s %7d %7d" % (r, n, d))
        print("    PROCEDURE DIVISION statements altered: %d   %s"
              % (len(proc_diffs), "OK" if not proc_diffs else "FAIL"))
        for r, ex, d in proc_diffs[:10]:
            print("        %s (net %+d lines)" % (r, d))
            for x, y in ex:
                print("            - %s\n            + %s" % (x, y))
        if proc_diffs:
            fail += 1

    print("\n" + "=" * 74)
    print("BLOCKING FAILURES: %d" % fail)
    print("=" * 74)
    return 1 if fail else 0


if __name__ == '__main__':
    sys.exit(main())
