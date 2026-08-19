#!/usr/bin/env python3
"""
build_families.py - bulk builder for the CABS Tier 5 utility program tier.

WHAT THIS IS FOR
----------------
The complexity-carrying programs in this estate (BATCH/INGEST, BATCH/RATING,
BATCH/BILLCALC, BATCH/JURIS, BATCH/SETTLE, BATCH/FORMAT, BATCH/REPORT and the
CTC multi-site family) are hand-authored, because each one carries specific
constructs from the 27-complexity catalogue in a specific paragraph, and those
placements are traceable in CONTRACTS/complexity_placement.json.

A real access-billing estate is not made only of those programs.  Underneath
them sits a much larger tier of utility work - table maintenance, extracts,
cross-reference reports and file conversions - that carries no interesting
complexity at all but does carry volume, and volume is itself a property of
the estate that has to be represented.  In the client scan this tier was the
bulk of the file count and roughly half the lines.

This builder produces that tier.  It is deliberately a generator and not a
copier: the point of the utility tier is that it is *varied but uninteresting*,
not that it is duplicated.  If these programs came out as near-copies of one
another, CAST Imaging clone detection would light up on them and report a
duplication problem that the estate does not actually have - a false signal
that would then have to be explained away in front of the client.  Every
structural knob below is therefore randomised per program from a per-program
seed derived from the program name, so the output is deterministic (rerunning
reproduces byte-identical files) but no two programs share a paragraph
sequence, a field inventory, a REDEFINES count or a CALL pattern.

WHAT IT EMITS
-------------
  * OS/VS COBOL (1974) conforming to CONVENTIONS.md:
      - no EVALUATE, no INITIALIZE, no inline PERFORM
      - no scope terminators (END-IF, END-PERFORM, END-READ ...)
      - no reference modification
      - PERFORM para THRU para-EXIT, periods terminate statements
      - fixed format, nothing beyond column 72
      - mandatory P0000-MAINLINE shape and mandatory P8000-CONTROL that
        writes CABS-CONTROL-RECORD to DD CTLOUT and sets CT-BAL-IND
      - COPY CABSWRK in every program
      - period-authentic header comment block with a revision history
        spanning 1987-2019
  * one JCL member per job, where a job runs one to three of the programs
  * a manifest of everything produced

FOUR ARCHETYPES
---------------
  RT  rate-table maintenance   CABURTnn   add / change / expire table rows
  EX  dataset extract          CABUEXnn   selective extract to a flat file
  XR  cross-reference report   CABUXRnn   two-file cross reference and print
  CV  file conversion          CABUCVnn   reformat between two record layouts

SIZE DISTRIBUTION
-----------------
Follows the observed real-estate distribution - most programs 200-500 lines,
a substantial minority 500-1000, a few 1000-2000.  Do not level this out; a
flat size distribution is one of the tells that an estate is synthetic.

USAGE
-----
    python3 BUILDER/build_families.py                 # default build
    python3 BUILDER/build_families.py --programs 95 --jobs 55
    python3 BUILDER/build_families.py --dry-run       # report only
    python3 BUILDER/build_families.py --seed 20260815

Rerunning with the same arguments overwrites the same files with the same
bytes.  Delete BATCH/UTIL and the JCL/CABU*.jcl members to start clean.

STYLE RULE (enforced by review, not by this script)
---------------------------------------------------
No comment emitted by this builder names its own construct or admits a
defect.  The comment pools below are period-authentic operational prose.
"""

import argparse
import hashlib
import json
import os
import random
import sys

# --------------------------------------------------------------------------
# column discipline
# --------------------------------------------------------------------------

AREA_A = ' ' * 7
AREA_B = ' ' * 11
CONT = ' ' * 15
MAXCOL = 72


def _chk(line):
    if len(line) > MAXCOL:
        raise ValueError('line exceeds column 72 (%d): %r' % (len(line), line))
    return line


def a(text):
    """Emit at Area A (column 8)."""
    return _chk(AREA_A + text)


def b(text):
    """Emit at Area B (column 12)."""
    return _chk(AREA_B + text)


def c(text):
    """Emit a continuation line, indented under Area B."""
    return _chk(CONT + text)


def cmt(text):
    """Emit one or more comment lines, wrapped at column 72."""
    out = []
    words = text.split()
    line = ''
    for w in words:
        cand = (line + ' ' + w).strip()
        if len(cand) > 62:
            out.append('      * ' + line)
            line = w
        else:
            line = cand
    if line:
        out.append('      * ' + line)
    return out


def perf(nm, prefix=AREA_B, term='.'):
    """PERFORM para THRU para-EXIT, wrapped if it will not fit."""
    one = 'PERFORM %s THRU %s-EXIT%s' % (nm, nm, term)
    if len(prefix + one) <= MAXCOL:
        return [_chk(prefix + one)]
    return [_chk(prefix + 'PERFORM %s THRU' % nm),
            _chk(prefix + '    %s-EXIT%s' % (nm, term))]


BANNER = '      ' + '*' * 65


def banner_block(lines):
    """Header block - pad each comment line and close it at column 71."""
    out = [BANNER]
    for ln in lines:
        body = ('      * ' + ln).rstrip()
        if len(body) > 70:
            raise ValueError('header line too long: %r' % ln)
        out.append(body.ljust(70) + '*')
    out.append(BANNER)
    return out


# --------------------------------------------------------------------------
# period-authentic people, dates and prose
# --------------------------------------------------------------------------

AUTHORS = [
    'R.T.WHEELER', 'D.OKONKWO', 'J.M.CASTILLO', 'P.NAIR', 'A.BUKOWSKI',
    'S.MARCHETTI', 'G.PRZYBYLSKI', 'L.FERREIRA', 'M.DELACROIX', 'K.O.BRIEN',
    'T.YAMASHITA', 'B.R.HALVORSEN', 'C.ADEYEMI', 'W.J.MCALLISTER',
]

REV_NOTES = [
    'INITIAL RELEASE',
    'CONTROL RECORD ADDED PER CABS-STD-002',
    'REGION SIZE REDUCED - TABLE MOVED OUT OF WORKING STORAGE',
    'SUSPENSE WRITE ADDED - RECORDS WERE BEING DROPPED',
    'PARM CARD EXTENDED, POSITIONS 40 THROUGH 48',
    'EFFECTIVE DATE FILTER ADDED PER AUDIT FINDING',
    'RECOMPILE ONLY - COPYBOOK CHANGE UPSTREAM',
    'REPORT PAGINATION CORRECTED',
    'HASH TOTAL ADDED TO THE CONTROL RECORD',
    'TABLE LIMIT RAISED FOR THE SOUTHEAST CENTRES',
    'CENTURY PIVOT APPLIED TO THE CYCLE DATE',
    'SECOND OUTPUT FILE ADDED FOR THE FACTOR STUDY',
    'RESTART KEY WRITTEN SO A RERUN CAN POSITION',
    'OCCURS RAISED AFTER THE FEBRUARY OVERFLOW',
    'CARRIER TYPE BROUGHT ONTO THE EXTRACT',
    'ROUNDING RULE TAKEN FROM THE RATE ROW',
    'BLOCK SIZE SET TO ZERO - SYSTEM DETERMINED',
    'PRINT LINE WIDENED TO 133',
    'RETIRED THE SECOND SORT STEP - DONE IN PROGRAM',
    'JOB PARAMETER MADE MANDATORY',
]

PARA_COMMENTS = [
    'THE COUNTS BELOW FEED THE BALANCING EQUATION AND MUST NOT BE '
    'RESET INSIDE THE LOOP.',
    'THE INPUT ARRIVES IN KEY SEQUENCE FROM THE SORT STEP THAT '
    'PRECEDES THIS PROGRAM IN THE JOB.',
    'A FAILURE HERE SUSPENDS THE RECORD, IT DOES NOT STOP THE STEP.',
    'ACCUMULATE IN THE ORDER THE RECORDS ARRIVE - THE PROOF FILE IS '
    'BUILT ON THE SAME ORDER.',
    'THE TABLE IS SEARCHED WITH A SUBSCRIPTED WALK BECAUSE THE INPUT '
    'IS NOT GUARANTEED TO BE ASCENDING.',
    'THE PARM CARD IS POSITIONAL.  THERE IS NO KEYWORD PARSER AND '
    'THERE NEVER HAS BEEN.',
    'THE EFFECTIVE WINDOW IS INCLUSIVE AT BOTH ENDS.  AN EXPIRY OF '
    'ZERO IS OPEN ENDED.',
    'THIS PARAGRAPH IS PERFORMED ONCE PER CONTROL BREAK AND ONCE '
    'MORE AT END OF FILE.',
    'THE WHOLE INPUT RECORD IS CARRIED ONTO THE SUSPENSE FILE SO THE '
    'RECYCLE JOB DOES NOT NEED THE SOURCE.',
    'FIELDS NOT PRESENT ON THE INPUT LAYOUT ARE LEFT AS SPACES '
    'RATHER THAN LOW VALUES.',
    'THE PRINT LINE IS BUILT COLUMN BY COLUMN.  THE CARRIAGE CONTROL '
    'CHARACTER CARRIES MEANING DOWNSTREAM.',
    'THE COUNTERS PRINTED HERE ARE FOR THE AUDIT REPORT ONLY AND ARE '
    'NOT PART OF THE BALANCE.',
]


def rev_history(rng, releases):
    """Build a plausible revision history spanning 1987-2019.

    The note is wrapped onto continuation lines indented under the author,
    which is how the hand-authored programs in this estate carry theirs.
    """
    years = sorted(rng.sample(range(1987, 2020), releases))
    lines = []
    major = 1
    minor = 0
    for i, y in enumerate(years):
        m = rng.randint(1, 12)
        d = rng.randint(1, 28)
        who = rng.choice(AUTHORS)
        note = REV_NOTES[0] if i == 0 else rng.choice(REV_NOTES[1:])
        stem = '  V%d.%02d  %04d-%02d-%02d  %-12s ' % (major, minor, y, m,
                                                       d, who)
        lim_first = 62 - len(stem)
        first, rest, cur = '', [], ''
        for w in note.split():
            cand = (cur + ' ' + w).strip()
            limit = lim_first if (not first and not rest) else 40
            if len(cand) > limit:
                if not first:
                    first = cur
                else:
                    rest.append(cur)
                cur = w
            else:
                cur = cand
        if not first:
            first = cur
        else:
            rest.append(cur)
        lines.append(stem + first)
        for r in rest:
            lines.append('                     ' + r)
        minor += rng.randint(1, 4)
        if minor > 40:
            major += 1
            minor = 0
    return lines


# --------------------------------------------------------------------------
# archetypes
# --------------------------------------------------------------------------

ARCHETYPES = {
    'RT': {
        'code': 'RT',
        'title': 'RATE TABLE MAINTENANCE',
        'purposes': [
            'RATE TABLE ROW ADD AND CHANGE',
            'RATE TABLE EXPIRY SWEEP',
            'RATE ELEMENT DESCRIPTION MAINTENANCE',
            'BAND TABLE MAINTENANCE',
            'TARIFF CODE TABLE REFRESH',
            'RATE TABLE EFFECTIVE DATE ROLL',
            'RATE OVERRIDE TABLE LOAD',
            'JURISDICTION TABLE MAINTENANCE',
        ],
        'copies': ['CABSRATE', 'CABSRT01', 'CABSCOMM'],
        'in_dd': ['TBLIN', 'MNTIN', 'CTLIN', 'RATIN', 'TARIN', 'BNDIN',
                  'ELMIN', 'OVRIN'],
        'out_dd': ['TBLOUT', 'MNTOUT', 'AUDOUT', 'RATOUT', 'TAROUT',
                   'BNDOUT', 'ELMOUT'],
        'nouns': ['ROW', 'ELEMENT', 'TARIFF', 'BAND', 'WINDOW', 'KEY',
                  'DESCRIPTION', 'OVERRIDE'],
    },
    'EX': {
        'code': 'EX',
        'title': 'DATASET EXTRACT',
        'purposes': [
            'CARRIER EXTRACT FOR THE SETTLEMENT FEED',
            'BILLED ACCOUNT EXTRACT',
            'USAGE EXTRACT BY JURISDICTION',
            'CIRCUIT INVENTORY EXTRACT',
            'SUSPENSE EXTRACT FOR THE RECYCLE JOB',
            'ADJUSTMENT EXTRACT FOR THE GENERAL LEDGER',
            'CONTROL RECORD EXTRACT',
            'FACTOR STUDY EXTRACT',
        ],
        'copies': ['CABSCARR', 'CABSCIRC', 'CABSCDR', 'CABSFCTR'],
        'in_dd': ['EXTIN', 'MSTIN', 'SELIN', 'CARIN', 'CIRIN', 'USGIN',
                  'ADJIN', 'GLIN'],
        'out_dd': ['EXTOUT', 'SELOUT', 'DROPOUT', 'CAROUT', 'CIROUT',
                   'FEEDOUT', 'GLOUT'],
        'nouns': ['SELECTION', 'EXTRACT', 'MASTER', 'SUBSET', 'RANGE',
                  'FILTER', 'CANDIDATE'],
    },
    'XR': {
        'code': 'XR',
        'title': 'CROSS REFERENCE REPORT',
        'purposes': [
            'CARRIER TO BILLING ACCOUNT CROSS REFERENCE',
            'CIRCUIT TO ACCOUNT CROSS REFERENCE',
            'RATE ELEMENT TO TARIFF CROSS REFERENCE',
            'ACCOUNT TO INVOICE CROSS REFERENCE',
            'SUSPENSE CODE CROSS REFERENCE',
            'JURISDICTION TO STATE CROSS REFERENCE',
            'CONTROL RECORD CROSS REFERENCE',
            'ORPHAN KEY CROSS REFERENCE',
        ],
        'copies': ['CABSCARR', 'CABSBHDR', 'CABSCIRC', 'CABSCOMM'],
        'in_dd': ['XRFIN', 'REFIN', 'MSTIN', 'LFTIN', 'RGTIN', 'KEYIN',
                  'INVIN', 'SUSIN'],
        'out_dd': ['XRFOUT', 'ORPOUT', 'PAIROUT', 'LNKOUT', 'GRPOUT',
                   'MTCOUT'],
        'nouns': ['REFERENCE', 'MATCH', 'ORPHAN', 'PAIR', 'LINK', 'SIDE',
                  'GROUP'],
    },
    'CV': {
        'code': 'CV',
        'title': 'FILE CONVERSION',
        'purposes': [
            'FIXED TO VARIABLE RECORD CONVERSION',
            'EMI FORMAT CONVERSION',
            'PACKED TO DISPLAY CONVERSION',
            'RECORD LAYOUT UPLIFT',
            'CENTURY FIELD CONVERSION',
            'CODE PAGE AND SIGN CONVERSION',
            'LEGACY LAYOUT DOWN CONVERSION',
            'INTERCHANGE FORMAT CONVERSION',
        ],
        'copies': ['CABSCDR', 'CABSBILL', 'CABSSETL', 'CABSCOMM'],
        'in_dd': ['CNVIN', 'OLDIN', 'SRCIN', 'EMIIN', 'PCKIN', 'LEGIN',
                  'IXCIN'],
        'out_dd': ['CNVOUT', 'NEWOUT', 'REJOUT', 'TGTOUT', 'DSPOUT',
                   'UPLOUT'],
        'nouns': ['FIELD', 'LAYOUT', 'SIGN', 'CENTURY', 'PACKED', 'ZONE',
                  'RECORD'],
    },
}

TAG_POOL = ['AA', 'AB', 'AC', 'AD', 'AE', 'AF', 'AG', 'AH', 'AJ', 'AK',
            'AL', 'AM', 'AN', 'AP', 'AQ', 'AR', 'AS', 'AT', 'AU', 'AV',
            'AW', 'AX', 'AY', 'AZ', 'BA', 'BC', 'BD', 'BE', 'BF', 'BG',
            'BH', 'BJ', 'BK', 'BL', 'BM', 'BN', 'BP', 'BQ', 'BR', 'BS',
            'BT', 'BU', 'BV', 'BW', 'BX', 'BY', 'BZ', 'CA', 'CB', 'CD',
            'CE', 'CF', 'CG', 'CH', 'CJ', 'CK', 'CL', 'CM', 'CN', 'CP',
            'CQ', 'CR', 'CS', 'CT', 'CU', 'CV', 'CW', 'CX', 'CY', 'CZ',
            'DA', 'DB', 'DC', 'DE', 'DF', 'DG', 'DH', 'DJ', 'DK', 'DL',
            'DM', 'DN', 'DP', 'DQ', 'DR', 'DS', 'DT', 'DU', 'DV', 'DW',
            'DX', 'DY', 'DZ', 'EA', 'EB', 'EC', 'ED', 'EF', 'EG', 'EH']

FIELD_NOUNS = ['OCN', 'BAN', 'ELEM', 'TARIFF', 'STATE', 'JURIS', 'CIRCUIT',
               'CYCLE', 'PERIOD', 'SEQ', 'TYPE', 'CODE', 'CLASS', 'GROUP',
               'REGION', 'CENTRE', 'STATUS', 'SOURCE', 'TARGET', 'MEDIA',
               'CARRIER', 'ACCOUNT', 'INVOICE', 'SEGMENT', 'BAND', 'LEVEL']

EXT_CALLS = ['CABDTCNV', 'CABHASH', 'CABPARMR', 'CABEDITF', 'CABFMTR',
             'CABCTLWR', 'CABSEQCK', 'CABTBLLU']


# --------------------------------------------------------------------------
# per-program symbol table
# --------------------------------------------------------------------------

def make_symbols(rng, arch, size_class, tag):
    """Build the field inventory this program will use.

    Counts are randomised so that no two programs declare the same shape of
    working storage.  Everything a paragraph body can reference is declared
    here first, so generated bodies never reference an undefined field.
    """
    if size_class == 'S':
        n_cnt, n_amt, n_qty, n_txt, n_sw, n_sub = (
            rng.randint(3, 6), rng.randint(2, 4), rng.randint(2, 4),
            rng.randint(2, 5), rng.randint(2, 3), rng.randint(2, 3))
        n_grp = rng.randint(0, 1)
        n_red = rng.randint(1, 2)
        occ = rng.choice([50, 80, 100, 120, 150])
    elif size_class == 'M':
        n_cnt, n_amt, n_qty, n_txt, n_sw, n_sub = (
            rng.randint(5, 9), rng.randint(3, 6), rng.randint(3, 6),
            rng.randint(4, 8), rng.randint(3, 5), rng.randint(3, 4))
        n_grp = rng.randint(2, 3)
        n_red = rng.randint(2, 5)
        occ = rng.choice([150, 200, 250, 300, 400])
    else:
        n_cnt, n_amt, n_qty, n_txt, n_sw, n_sub = (
            rng.randint(8, 12), rng.randint(5, 9), rng.randint(5, 8),
            rng.randint(6, 12), rng.randint(4, 7), rng.randint(4, 6))
        n_grp = rng.randint(3, 5)
        n_red = rng.randint(4, 8)
        occ = rng.choice([300, 400, 500, 600, 750])

    def names(kind, n):
        return ['WS-%s-%s-%02d' % (tag, kind, i + 1) for i in range(n)]

    sym = {
        'tag': tag,
        'cnt': names('CNT', n_cnt),
        'amt': names('AMT', n_amt),
        'qty': names('QTY', n_qty),
        'txt': names('TXT', n_txt),
        'sub': names('SUB', n_sub),
        'n_groups': n_grp,
        'n_redef': n_red,
        'sw': [],
        'occurs': occ,
    }
    for i in range(n_sw):
        base = 'WS-%s-SW-%02d' % (tag, i + 1)
        sym['sw'].append((base,
                          'WS-%s-ON-%02d' % (tag, i + 1),
                          'WS-%s-OFF-%02d' % (tag, i + 1)))
    sym['tab'] = {
        'name': 'WS-%s-TABLE' % tag,
        'cnt': 'WS-%s-TAB-CNT' % tag,
        'ent': 'WS-%s-TB-ENTRY' % tag,
        'idx': 'WS-%s-IX' % tag,
        'occ': occ,
        'key': 'WS-%s-TB-KEY' % tag,
        'val': 'WS-%s-TB-VAL' % tag,
        'txt': 'WS-%s-TB-TXT' % tag,
        'eff': 'WS-%s-TB-EFF' % tag,
        'exp': 'WS-%s-TB-EXP' % tag,
    }
    return sym


def make_record(rng, prefix, n_fields, packed_ok=True):
    """Build a record layout.  Returns (fields, total_bytes)."""
    fields = []
    total = 0
    used = set()
    for i in range(n_fields):
        noun = rng.choice(FIELD_NOUNS)
        name = '%s-%s' % (prefix, noun)
        k = 2
        while name in used:
            name = '%s-%s%d' % (prefix, noun, k)
            k += 1
        used.add(name)
        r = rng.random()
        if r < 0.42:
            n = rng.choice([2, 3, 4, 6, 8, 10, 13, 16, 20])
            pic = 'PIC X(%02d)' % n
            total += n
        elif r < 0.62:
            n = rng.choice([2, 3, 4, 5, 6, 7, 9])
            pic = 'PIC 9(%02d)' % n
            total += n
        elif r < 0.82 and packed_ok:
            n = rng.choice([5, 7, 9, 11, 13, 15])
            pic = 'PIC S9(%02d) COMP-3' % n
            total += (n // 2) + 1
        else:
            n = rng.choice([7, 9, 11, 13])
            d = rng.choice([2, 2, 5])
            pic = 'PIC S9(%02d)V9(%02d) COMP-3' % (n, d)
            total += ((n + d) // 2) + 1
        fields.append((name, pic))
    return fields, total


def fit_record(rng, prefix, n_fields, limit):
    """Build a record layout that fits inside `limit` bytes."""
    fields, total = make_record(rng, prefix, n_fields)
    while total >= limit and n_fields > 3:
        n_fields -= 1
        fields, total = make_record(rng, prefix, n_fields)
    while total >= limit:
        fields = fields[:-1]
        total = sum(1 for _ in fields) * 4
    return fields, total


# --------------------------------------------------------------------------
# paragraph body generators
# --------------------------------------------------------------------------

def body_move_count(rng, sym, ctx):
    out = []
    for _ in range(rng.randint(2, 4)):
        src, _p = rng.choice(ctx['in_fields'])
        dst = rng.choice(sym['txt'])
        out.append(b('MOVE %s TO %s.' % (src, dst)))
    out.append(b('ADD 1 TO %s.' % rng.choice(sym['cnt'])))
    return out


def body_range_edit(rng, sym, ctx):
    fld, pic = rng.choice([f for f in ctx['in_fields'] if '9' in f[1]]
                          or ctx['in_fields'])
    sw, on, off = rng.choice(sym['sw'])
    lo = rng.randint(1, 40)
    hi = rng.randint(41, 9999)
    out = [b("MOVE 'Y' TO %s." % sw),
           b('IF %s < %d' % (fld, lo)),
           c("MOVE 'N' TO %s" % sw),
           c('ADD 1 TO %s.' % rng.choice(sym['cnt'])),
           b('IF %s > %d' % (fld, hi)),
           c("MOVE 'N' TO %s" % sw),
           c('ADD 1 TO %s.' % rng.choice(sym['cnt']))]
    return out


def body_accumulate(rng, sym, ctx):
    out = []
    tgt = rng.choice(sym['amt'])
    q = rng.choice(sym['qty'])
    src = [f for f in ctx['in_fields'] if 'COMP-3' in f[1]]
    if src:
        s1 = rng.choice(src)[0]
        out.append(b('ADD %s TO %s.' % (s1, q)))
    if rng.random() < 0.45:
        out.append(b('COMPUTE %s ROUNDED = %s * %s.'
                     % (tgt, q, rng.choice(sym['qty']))))
    else:
        out.append(b('COMPUTE %s = %s * %s.'
                     % (tgt, q, rng.choice(sym['qty']))))
    out.append(b('ADD %s TO %s.' % (tgt, rng.choice(sym['amt']))))
    return out


def body_table_load(rng, sym, ctx):
    t = sym['tab']
    key = rng.choice(ctx['in_fields'])[0]
    return [b('IF %s NOT < %d' % (t['cnt'], t['occ'])),
            c("MOVE 'Y' TO %s" % sym['sw'][0][0]),
            c('ADD 1 TO %s' % rng.choice(sym['cnt'])),
            b('ELSE'),
            c('ADD 1 TO %s' % t['cnt']),
            c('SET %s TO %s' % (t['idx'], t['cnt'])),
            c('MOVE %s TO %s (%s)' % (key, t['key'], t['idx'])),
            c('MOVE 0 TO %s (%s)' % (t['val'], t['idx'])),
            c('MOVE SPACES TO %s (%s)' % (t['txt'], t['idx'])),
            c('ADD 1 TO %s.' % rng.choice(sym['cnt']))]


def body_table_search(rng, sym, ctx):
    t = sym['tab']
    sub = rng.choice(sym['sub'])
    sw = rng.choice(sym['sw'])[0]
    return [b("MOVE 'N' TO %s." % sw),
            b('IF %s > 0' % t['cnt'])] + \
        perf(ctx['helper'], CONT, '') + [
            c('VARYING %s FROM 1 BY 1' % sub),
            c('UNTIL %s > %s' % (sub, t['cnt'])),
            c("OR %s = 'Y'." % sw)]


def helper_compare(rng, sym, ctx, name):
    t = sym['tab']
    sub = ctx['search_sub']
    sw = ctx['search_sw']
    key = rng.choice(ctx['in_fields'])[0]
    return ([a('%s.' % name),
             b('SET %s TO %s.' % (t['idx'], sub)),
             b('IF %s (%s) = %s' % (t['key'], t['idx'], key)),
             c("MOVE 'Y' TO %s" % sw),
             c('MOVE %s (%s) TO %s' % (t['val'], t['idx'],
                                       rng.choice(sym['qty']))),
             c('MOVE %s TO %s.' % (sub, rng.choice(sym['sub']))),
             a('%s-EXIT.' % name),
             b('EXIT.')])


def body_string_build(rng, sym, ctx):
    dst = rng.choice(sym['txt'])
    f1 = rng.choice(ctx['in_fields'])[0]
    f2 = rng.choice(ctx['in_fields'])[0]
    return [b('MOVE SPACES TO %s.' % dst),
            b('STRING %s DELIMITED BY SIZE' % f1),
            c("'-' DELIMITED BY SIZE"),
            c('%s DELIMITED BY SIZE' % f2),
            c('INTO %s.' % dst)]


def body_inspect(rng, sym, ctx):
    fld = rng.choice(sym['txt'])
    cn = rng.choice(sym['cnt'])
    return [b('MOVE 0 TO %s.' % cn),
            b('INSPECT %s TALLYING %s' % (fld, cn)),
            c('FOR ALL SPACES.'),
            b('INSPECT %s REPLACING ALL LOW-VALUES BY SPACES.' % fld)]


def body_unstring(rng, sym, ctx):
    src = rng.choice(sym['txt'])
    d1 = rng.choice(sym['txt'])
    d2 = rng.choice(sym['txt'])
    cn = rng.choice(sym['cnt'])
    return [b("UNSTRING %s DELIMITED BY '/'" % src),
            c('INTO %s' % d1),
            c('%s' % d2),
            c('TALLYING IN %s.' % cn)]


def body_date_pivot(rng, sym, ctx):
    return [b('MOVE %s TO DW-CUR-YY DW-CUR-DDD.' % ctx['cycle_fld']),
            b('COMPUTE DW-CENTURY-WORK = 1900 + DW-CUR-YY.'),
            b('IF DW-CUR-YY < DW-PIVOT-YY'),
            c('COMPUTE DW-CENTURY-WORK = 2000 + DW-CUR-YY.'),
            b("CALL 'CABDTCNV' USING %s DW-GREG-DATE" % ctx['cycle_view']),
            c('WS-RC-DTCNV.')]


def body_call_ext(rng, sym, ctx):
    tgt = rng.choice([x for x in EXT_CALLS if x != 'CABDTCNV'])
    arg = rng.choice(sym['txt'])
    rc = 'WS-RC-EXT'
    return [b("CALL '%s' USING %s %s." % (tgt, arg, rc)),
            b('IF %s NOT = 0' % rc),
            c('ADD 1 TO %s.' % rng.choice(sym['cnt']))]


def body_switch_cascade(rng, sym, ctx):
    fld = rng.choice(ctx['in_fields'])[0]
    v = [rng.choice(['A', 'B', 'C', 'D', 'E', 'S', 'X'])
         for _ in range(3)]
    cn = sym['cnt']
    return [b("IF %s = '%s'" % (fld, v[0])),
            c('ADD 1 TO %s' % rng.choice(cn)),
            b('ELSE'),
            c("IF %s = '%s'" % (fld, v[1])),
            c('    ADD 1 TO %s' % rng.choice(cn)),
            c('ELSE'),
            c("    IF %s = '%s'" % (fld, v[2])),
            c('        ADD 1 TO %s' % rng.choice(cn)),
            c('    ELSE'),
            c('        ADD 1 TO %s.' % rng.choice(cn))]


def body_subscript_walk(rng, sym, ctx):
    t = sym['tab']
    sub = rng.choice(sym['sub'])
    return [b('MOVE 0 TO %s.' % rng.choice(sym['qty']))] + \
        perf(ctx['helper2'], AREA_B, '') + [
            c('VARYING %s FROM 1 BY 1' % sub),
            c('UNTIL %s > %s.' % (sub, t['cnt']))]


def helper_walk(rng, sym, ctx, name):
    t = sym['tab']
    sub = ctx['walk_sub']
    return ([a('%s.' % name),
             b('SET %s TO %s.' % (t['idx'], sub)),
             b('IF %s (%s) NOT = SPACES' % (t['key'], t['idx'])),
             c('ADD %s (%s) TO %s.' % (t['val'], t['idx'],
                                       rng.choice(sym['qty']))),
             a('%s-EXIT.' % name),
             b('EXIT.')])


def body_percent(rng, sym, ctx):
    a1 = rng.choice(sym['amt'])
    a2 = rng.choice(sym['amt'])
    q = rng.choice(sym['qty'])
    return [b('IF %s NOT = 0' % a2),
            c('COMPUTE %s = %s * 100 / %s' % (q, a1, a2)),
            b('ELSE'),
            c('MOVE 0 TO %s.' % q)]


def body_collar(rng, sym, ctx):
    a1 = rng.choice(sym['amt'])
    lo = rng.randint(1, 50)
    hi = rng.randint(5000, 99999)
    return [b('IF %s < %d' % (a1, lo)),
            c('MOVE %d TO %s' % (lo, a1)),
            c('ADD 1 TO %s.' % rng.choice(sym['cnt'])),
            b('IF %s > %d' % (a1, hi)),
            c('MOVE %d TO %s' % (hi, a1)),
            c('ADD 1 TO %s.' % rng.choice(sym['cnt']))]


def body_absolute(rng, sym, ctx):
    a1 = rng.choice(sym['amt'])
    a2 = rng.choice(sym['amt'])
    return [b('MOVE %s TO %s.' % (a1, a2)),
            b('IF %s < 0' % a2),
            c('COMPUTE %s = 0 - %s.' % (a2, a1))]


def body_compare_keys(rng, sym, ctx):
    k1 = rng.choice(sym['txt'])
    k2 = rng.choice(sym['txt'])
    sw = rng.choice(sym['sw'])[0]
    return [b("MOVE 'N' TO %s." % sw),
            b('IF %s NOT = %s' % (k1, k2)),
            c("MOVE 'Y' TO %s" % sw),
            c('MOVE %s TO %s' % (k1, k2)),
            c('ADD 1 TO %s.' % rng.choice(sym['cnt']))]


def body_reset(rng, sym, ctx):
    out = []
    for f in rng.sample(sym['qty'], min(len(sym['qty']), rng.randint(2, 3))):
        out.append(b('MOVE 0 TO %s.' % f))
    for f in rng.sample(sym['amt'], min(len(sym['amt']), rng.randint(1, 2))):
        out.append(b('MOVE 0 TO %s.' % f))
    return out


def body_write_detail(rng, sym, ctx):
    out = [b('MOVE SPACES TO %s.' % ctx['out_rec'])]
    n = min(len(ctx['out_fields']), rng.randint(4, 9))
    for dst, _p in ctx['out_fields'][:n]:
        src = rng.choice(ctx['in_fields'])[0]
        out.append(b('MOVE %s TO %s.' % (src, dst)))
    out.append(b('WRITE %s.' % ctx['out_rec']))
    out.append(b('ADD 1 TO WS-WRITE-CNT.'))
    return out


def body_print_line(rng, sym, ctx):
    return [b('MOVE SPACES TO CABS-PRINT-LINE.'),
            b("MOVE ' ' TO PC-CC."),
            b('MOVE %s TO PC-COL-001-020.' % rng.choice(sym['txt'])),
            b('MOVE %s TO PC-COL-021-060.' % rng.choice(sym['txt'])),
            b('MOVE %s TO %s.' % (rng.choice(sym['amt']), ctx['amt_edit'])),
            b('MOVE %s TO PC-COL-091-132.' % ctx['amt_edit']),
            b('WRITE CABS-PRINT-LINE.'),
            b('ADD 1 TO WS-RPT-LINE-NBR.')]


def body_suspense(rng, sym, ctx):
    return [b('MOVE SPACES TO CABS-SUSPENSE-RECORD.'),
            b('MOVE %s TO SU-ERR-CODE.' % rng.choice(
                ['EC-OCN-UNKNOWN', 'EC-BAN-UNKNOWN', 'EC-DATE-INVALID',
                 'EC-RATE-NOT-FOUND', 'EC-DUP-SEQ'])),
            b("MOVE 'E' TO SU-ERR-SEVERITY."),
            b('MOVE WS-PGM-NAME TO SU-DETECT-PGM.'),
            b('MOVE PC1-RUN-ID TO SU-RUN-ID.'),
            b('MOVE %s TO SU-ORIG-RECORD.' % ctx['in_rec']),
            b('ADD 1 TO WS-REJECT-CNT.')]


def body_hash(rng, sym, ctx):
    return [b("CALL 'CABHASH' USING %s WS-ACC-OCN-HASH."
              % rng.choice(ctx['in_fields'])[0]),
            b('ADD %s TO WS-ACC-SEQ-HASH.' % rng.choice(sym['cnt']))]


BODY_POOL = [
    body_move_count, body_range_edit, body_accumulate, body_string_build,
    body_inspect, body_unstring, body_call_ext, body_switch_cascade,
    body_percent, body_collar, body_absolute, body_compare_keys,
    body_reset, body_write_detail, body_print_line, body_suspense,
    body_hash,
]


# --------------------------------------------------------------------------
# program builder
# --------------------------------------------------------------------------

DEFAULT_KNOBS = {
    'S': {'p2': 3, 'p3': 2, 'p4': 0},
    'M': {'p2': 7, 'p3': 4, 'p4': 2},
    'L': {'p2': 12, 'p3': 6, 'p4': 8},
}


def build_program(pgm, arch_key, size_class, seed_base, jobname, stepname,
                  knobs=None, tag=None):
    """Emit one complete COBOL program.  Returns (text, meta).

    `knobs` carries the paragraph counts for the main, output and secondary
    sections.  The driver adjusts them to land the program on its target
    line count, which is how the size distribution is held to the shape
    observed in the real estate.
    """
    if knobs is None:
        knobs = dict(DEFAULT_KNOBS[size_class])
    rng = random.Random(hashlib.sha256(
        ('%s|%s|%d' % (pgm, arch_key, seed_base)).encode()).hexdigest())
    arch = ARCHETYPES[arch_key]
    if tag is None:
        tag = rng.choice(TAG_POOL)
    sym = make_symbols(rng, arch, size_class, tag)
    purpose = rng.choice(arch['purposes'])

    if size_class == 'S':
        n_in, n_out = 1, 1
        p_print, p_susp, p_table = 0.35, 0.40, 0.35
    elif size_class == 'M':
        n_in, n_out = rng.randint(1, 2), rng.randint(1, 2)
        p_print, p_susp, p_table = 0.80, 0.65, 0.75
    else:
        n_in, n_out = rng.randint(2, 3), rng.randint(2, 3)
        p_print, p_susp, p_table = 0.90, 0.80, 0.90
    in_dds = rng.sample(arch['in_dd'], min(n_in, len(arch['in_dd'])))
    out_dds = rng.sample(arch['out_dd'], min(n_out, len(arch['out_dd'])))
    use_print = rng.random() < p_print
    use_susp = rng.random() < p_susp
    extra_copies = rng.sample(arch['copies'],
                              rng.randint(0, min(2, len(arch['copies']))))

    # record layouts -------------------------------------------------------
    in_prefix = 'I%s' % tag[0]
    out_prefix = 'O%s' % tag[0]
    nf_in = {'S': rng.randint(8, 14), 'M': rng.randint(12, 20),
             'L': rng.randint(18, 28)}[size_class]
    nf_out = {'S': rng.randint(7, 12), 'M': rng.randint(10, 18),
              'L': rng.randint(16, 26)}[size_class]
    in_fields, in_bytes = make_record(rng, in_prefix, nf_in)
    out_fields, out_bytes = make_record(rng, out_prefix, nf_out)
    in_lrecl = max(80, ((in_bytes // 10) + 1) * 10)
    out_lrecl = max(80, ((out_bytes // 10) + 1) * 10)
    in_rec = 'CABS-%s-IN-RECORD' % tag
    out_rec = 'CABS-%s-OUT-RECORD' % tag

    L = []

    # header ---------------------------------------------------------------
    hdr = ['%s - %s' % (pgm, purpose),
           'APPLICATION : CABS',
           'INPUTS      : DDNAME  DSN                          COPYBOOK']
    for dd in in_dds:
        hdr.append('              %-7s TELCABS.CABS.%-14s (LOCAL)'
                   % (dd, dd[:6]))
    hdr.append('OUTPUTS     : DDNAME  DSN                          COPYBOOK')
    for dd in out_dds:
        hdr.append('              %-7s TELCABS.CABS.%-14s (LOCAL)'
                   % (dd, dd[:6]))
    if use_print:
        hdr.append('              RPTOUT  SYSOUT CLASS A               '
                   'CABSPRNT')
    if use_susp:
        hdr.append('              SUSOUT  TELCABS.CABS.UTIL.SUSP    '
                   'CABSERR')
    hdr.append('CONTROL     : CTLOUT                               CABSCTL')
    hdr.append('BALANCE     : CT-READ = CT-WRITTEN + CT-REJECTED +')
    hdr.append('              CT-SUMMARISED + CT-CARRIED-FWD')
    hdr.append('RESTART     : %s'
               % rng.choice(['FULL RERUN - NOTHING IS UPDATED IN PLACE',
                             'RESTARTABLE FROM CT-RESTART-KEY',
                             'FULL RERUN - THE OUTPUT IS REBUILT EACH RUN']))
    hdr.append('REVISION HISTORY')
    n_rev = {'S': rng.randint(3, 4), 'M': rng.randint(4, 6),
             'L': rng.randint(6, 9)}[size_class]
    hdr.extend(rev_history(rng, n_rev))
    L.extend(banner_block(hdr))

    L.append(a('IDENTIFICATION DIVISION.'))
    L.append(a('PROGRAM-ID. %s.' % pgm))
    L.append(a('AUTHOR. TELCABS APPLICATIONS - %s.' % arch['title']))
    desc = rng.choice([
        'THIS STEP IS SCHEDULED INSIDE THE NIGHTLY ACCESS BILLING '
        'STREAM AND HAS NO INTERACTIVE ENTRY POINT.',
        'THE STEP IS DRIVEN ENTIRELY FROM THE SYSIN PARM CARD AND THE '
        'DD ALLOCATIONS IN THE JOB.',
        'THE STEP RUNS ONCE PER BILL CYCLE AND IS RERUN FROM THE TOP '
        'IF IT FAILS.',
        'THE STEP IS SCHEDULED MONTHLY AND ALSO RUN ON DEMAND WHEN A '
        'CENTRE ASKS FOR IT.',
    ])
    L.extend(banner_block(
        [x[8:] for x in cmt('%s.  %s' % (purpose, desc))] +
        [x[8:] for x in cmt(rng.choice(PARA_COMMENTS))]))

    # environment ----------------------------------------------------------
    L.append(a('ENVIRONMENT DIVISION.'))
    L.append(a('CONFIGURATION SECTION.'))
    L.append(a('SOURCE-COMPUTER. IBM-370.'))
    L.append(a('OBJECT-COMPUTER. IBM-370.'))
    L.append(a('SPECIAL-NAMES.'))
    L.append(b('C01 IS TO-NEW-PAGE.'))
    L.append(a('INPUT-OUTPUT SECTION.'))
    L.append(a('FILE-CONTROL.'))
    all_files = ([(dd, 'WS-FS-INPUT') for dd in in_dds] +
                 [(dd, 'WS-FS-OUTPUT') for dd in out_dds] +
                 ([('SUSOUT', 'WS-FS-SUSPENSE')] if use_susp else []) +
                 [('CTLOUT', 'WS-FS-CONTROL')] +
                 ([('RPTOUT', 'WS-FS-OUTPUT')] if use_print else []))
    for dd, fs in all_files:
        L.append(b('SELECT %s ASSIGN TO UT-S-%s' % (dd, dd)))
        L.append(c('ORGANIZATION IS SEQUENTIAL'))
        L.append(c('ACCESS MODE IS SEQUENTIAL'))
        L.append(c('FILE STATUS IS %s.' % fs))

    # data division --------------------------------------------------------
    L.append(a('DATA DIVISION.'))
    L.append(a('FILE SECTION.'))

    def fd(dd, lrecl, recfm='F'):
        L.extend(cmt('%s - %s.' % (dd, rng.choice([
            'ALLOCATED BY THE JOB, NEVER BY THE PROGRAM',
            'SEQUENTIAL, BLOCKED BY THE SYSTEM',
            'CATALOGUED GENERATION DATA GROUP',
            'WORK FILE, DELETED AT STEP END',
            'PERMANENT DATASET HELD ON DASD']))))
        L.append(a('FD  %s' % dd))
        L.append(b('RECORDING MODE IS %s' % recfm))
        L.append(b('LABEL RECORDS ARE STANDARD'))
        L.append(b('BLOCK CONTAINS 0 RECORDS'))
        L.append(b('RECORD CONTAINS %d CHARACTERS.' % lrecl))

    fill_seq = [0]

    def layout(level_rec, fields, lrecl, used):
        L.append(a('01  %s.' % level_rec))
        for nm, pic in fields:
            L.append(b('05  %-27s %s.' % (nm, pic)))
        pad = lrecl - used
        if pad > 0:
            fill_seq[0] += 1
            L.append(b('05  %-27s PIC X(%d).'
                       % ('%s-FILL-%02d' % (tag, fill_seq[0]), pad)))

    # input files
    for i, dd in enumerate(in_dds):
        fd(dd, in_lrecl)
        if i == 0:
            layout(in_rec, in_fields, in_lrecl, in_bytes)
        else:
            alt, altb = fit_record(rng, 'A%d' % i, rng.randint(6, 12),
                                   in_lrecl)
            layout('CABS-%s-ALT%d-RECORD' % (tag, i), alt, in_lrecl, altb)
    # redefines on the primary input
    for r in range(sym['n_redef']):
        rf, rb = fit_record(rng, 'R%d%s' % (r, tag[0]),
                            rng.randint(4, 9), in_lrecl)
        L.extend(cmt(rng.choice([
            'ALTERNATE VIEW OF THE SAME BYTES.  THE RECORD TYPE MUST BE '
            'TESTED BEFORE ANY FIELD BELOW IS REFERENCED.',
            'THIS VIEW IS USED WHEN THE SOURCE SYSTEM SENDS THE OLDER '
            'LAYOUT.  BOTH ARE STILL ARRIVING.',
            'THE SECOND VIEW EXISTS BECAUSE THE FEED CARRIES TWO CARD '
            'FORMATS AND ONLY THE FIRST BYTE TELLS THEM APART.'])))
        L.append(a('01  CABS-%s-VIEW%d REDEFINES %s.' % (tag, r + 1, in_rec)))
        for nm, pic in rf:
            L.append(b('05  %-27s %s.' % (nm, pic)))
        L.append(b('05  %-27s PIC X(%d).'
                   % ('R%d%s-REST' % (r, tag[0]), in_lrecl - rb)))

    # output files
    for i, dd in enumerate(out_dds):
        fd(dd, out_lrecl)
        if i == 0:
            layout(out_rec, out_fields, out_lrecl, out_bytes)
        else:
            L.append(a('01  %-27s PIC X(%d).'
                       % ('CABS-%s-OUT%d-RECORD' % (tag, i), out_lrecl)))
    if use_susp:
        fd('SUSOUT', 300)
        L.append(a('01  CABS-SUSOUT-RECORD              PIC X(300).'))
    fd('CTLOUT', 180)
    L.append(a('01  CABS-CTLOUT-RECORD              PIC X(180).'))
    if use_print:
        fd('RPTOUT', 133, 'FA')
        L.append(a('COPY CABSPRNT.'))

    # working storage ------------------------------------------------------
    L.append(a('WORKING-STORAGE SECTION.'))
    L.extend(cmt('STANDARD SHARED WORKING STORAGE.  SEE CABSWRK.'))
    L.append(a('COPY CABSWRK.'))
    for cp in extra_copies:
        L.extend(cmt('SHARED LAYOUT PULLED IN FOR THE %s SIDE.'
                     % rng.choice(arch['nouns'])))
        L.append(a('COPY %s.' % cp))
    L.append(a('01  WS-PROGRAM-CONSTANTS.'))
    L.append(b("05  %-27s PIC X(08) VALUE '%s'." % ('WS-PGM-NAME', pgm)))
    L.append(b("05  %-27s PIC X(05) VALUE 'V%d.%02d'."
               % ('WS-PGM-VERSION', rng.randint(1, 3), rng.randint(1, 30))))
    L.append(b('05  %-27s PIC S9(04) COMP-3' % 'WS-TABLE-LIMIT'))
    L.append(c('                          VALUE %d.' % sym['occurs']))
    L.extend(cmt('SYSIN PARM CARD.  POSITIONAL LAYOUT ONLY.'))
    L.append(a('01  WS-PARM-CARD                    PIC X(80).'))
    L.append(a('01  WS-PARM-CARD-R1 REDEFINES WS-PARM-CARD.'))
    for nm, pic in (('PC1-REC-ID', 'PIC X(02)'),
                    ('PC1-RUN-ID', 'PIC X(12)'),
                    ('PC1-CYCLE-YYDDD', 'PIC 9(05)'),
                    ('PC1-BILL-PERIOD', 'PIC 9(06)'),
                    ('PC1-JOBNAME', 'PIC X(08)'),
                    ('PC1-STEPNAME', 'PIC X(08)'),
                    ('PC1-OPT-ONE', 'PIC X(01)'),
                    ('PC1-OPT-TWO', 'PIC X(01)'),
                    ('PC1-FILLER', 'PIC X(37)')):
        L.append(b('05  %-27s %s.' % (nm, pic)))
    if rng.random() < 0.6:
        L.append(a('01  WS-PARM-CARD-R2 REDEFINES WS-PARM-CARD.'))
        L.append(b('05  %-27s PIC X(14).' % 'PC2-LEAD'))
        L.append(b('05  PC2-CYCLE-VIEW.'))
        L.append(b('    10  %-27s PIC 9(02).' % 'PC2-CV-YY'))
        L.append(b('    10  %-27s PIC 9(03).' % 'PC2-CV-DDD'))
        L.append(b('05  %-27s PIC X(61).' % 'PC2-REST'))

    L.append(a('01  WS-COUNT-AREA.'))
    for f in sym['cnt']:
        L.append(b('05  %-27s PIC S9(09) COMP-3 VALUE 0.' % f))
    L.append(a('01  WS-QUANTITY-AREA.'))
    for f in sym['qty']:
        L.append(b('05  %-27s PIC S9(11)V9(02) COMP-3' % f))
        L.append(c('                                   VALUE 0.'))
    L.append(a('01  WS-AMOUNT-AREA.'))
    for f in sym['amt']:
        L.append(b('05  %-27s PIC S9(13)V9(05) COMP-3' % f))
        L.append(c('                                   VALUE 0.'))
    L.append(a('01  WS-TEXT-AREA.'))
    for f in sym['txt']:
        L.append(b('05  %-27s PIC X(%02d) VALUE SPACES.'
                   % (f, rng.choice([8, 10, 12, 16, 20, 26, 30]))))
    L.append(a('01  WS-SWITCH-AREA.'))
    for base, on, off in sym['sw']:
        L.append(b("05  %-27s PIC X(01) VALUE 'N'." % base))
        L.append(b('    88  %-27s VALUE %s.' % (on, "'Y'")))
        L.append(b('    88  %-27s VALUE %s.' % (off, "'N'")))
    L.append(a('01  WS-SUBSCRIPT-AREA.'))
    for f in sym['sub']:
        L.append(b('05  %-27s PIC S9(04) COMP-3 VALUE 0.' % f))

    t = sym['tab']
    L.extend(cmt(rng.choice([
        'IN CORE TABLE.  LOADED ONCE AT INITIALISATION AND SEARCHED '
        'WITH A SUBSCRIPTED WALK.',
        'IN CORE TABLE.  THE LIMIT WAS RAISED WHEN THE SOUTHEAST '
        'CENTRES WERE MERGED AND HAS NOT BEEN REVIEWED SINCE.',
        'IN CORE TABLE.  A ROW THAT DOES NOT FIT IS COUNTED AND '
        'DROPPED - THE STEP DOES NOT FAIL.'])))
    L.append(a('01  %s.' % t['name']))
    L.append(b('05  %-27s PIC S9(04) COMP-3 VALUE 0.' % t['cnt']))
    L.append(b('05  %s OCCURS %d TIMES' % (t['ent'], t['occ'])))
    L.append(c('                        INDEXED BY %s.' % t['idx']))
    L.append(b('    10  %-27s PIC X(%02d).'
               % (t['key'], rng.choice([4, 6, 8, 10, 13]))))
    L.append(b('    10  %-27s PIC S9(09)V9(05) COMP-3.' % t['val']))
    L.append(b('    10  %-27s PIC X(%02d).'
               % (t['txt'], rng.choice([20, 30, 40]))))
    L.append(b('    10  %-27s PIC 9(05).' % t['eff']))
    L.append(b('    10  %-27s PIC 9(05).' % t['exp']))

    for g in range(sym['n_groups']):
        L.append(a('01  WS-%s-WORK-GROUP-%d.' % (tag, g + 1)))
        for k in range(rng.randint(3, 8)):
            nm = 'WS-%s-G%d-%s' % (tag, g + 1, rng.choice(FIELD_NOUNS))
            pic = rng.choice(['PIC X(10)', 'PIC X(20)', 'PIC 9(05)',
                              'PIC 9(07)', 'PIC S9(09) COMP-3',
                              'PIC S9(11)V9(02) COMP-3'])
            L.append(b('05  %-27s %s.' % (nm[:24], pic)))

    amt_edit = 'WS-%s-AMT-EDIT' % tag
    L.append(a('01  WS-REPORT-WORK.'))
    L.append(b('05  %-27s PIC S9(03) COMP-3' % 'WS-RPT-PAGE-NBR'))
    L.append(c('                                VALUE 0.'))
    L.append(b('05  %-27s PIC S9(03) COMP-3' % 'WS-RPT-LINE-NBR'))
    L.append(c('                                VALUE 0.'))
    L.append(b('05  %-27s PIC S9(03) COMP-3' % 'WS-RPT-MAX-LINES'))
    L.append(c('                                VALUE %d.'
               % rng.choice([50, 55, 58, 60])))
    L.append(b('05  %-27s PIC X(60) VALUE' % 'WS-RPT-TITLE1'))
    L.append(c("    '%s - %s'." % (pgm, purpose[:38])))
    L.append(b('05  %-27s PIC X(60) VALUE' % 'WS-RPT-TITLE2'))
    L.append(c("    'TELCABS WHOLESALE ACCESS BILLING'."))
    L.append(b('05  %-27s PIC ZZ,ZZZ,ZZZ,ZZ9.99-.' % amt_edit))
    L.append(b('05  %-27s PIC ZZZ,ZZZ,ZZ9.' % ('WS-%s-CNT-EDIT' % tag)))
    L.append(a('01  WS-ABEND-WORK.'))
    L.append(b('05  %-27s PIC X(08).' % 'WS-AB-PGM'))
    L.append(b('05  %-27s PIC X(30).' % 'WS-AB-PARA'))
    L.append(b('05  %-27s PIC X(60).' % 'WS-AB-REASON'))
    L.append(b('05  %-27s PIC 9(04) VALUE %d.'
               % ('WS-AB-USER-CODE', rng.randint(9910, 9989))))
    L.append(a('01  WS-EXT-CALL-RC.'))
    for nm in ('WS-RC-DTCNV', 'WS-RC-EXT', 'WS-RC-PARMR', 'WS-RC-ABEND'):
        L.append(b('05  %-27s PIC 9(04) VALUE 0.' % nm))
    cycle_view = 'WS-%s-CYCLE-CCYYDDD' % tag
    L.append(a('01  WS-CYCLE-VIEW.'))
    L.append(b('05  %-27s PIC 9(05) VALUE 0.'
               % ('WS-%s-CYCLE-YYDDD' % tag)))
    L.append(b('05  %-27s PIC 9(07) VALUE 0.' % cycle_view))

    # ---------------------------------------------------------------- code
    ctx = {
        'in_fields': in_fields, 'out_fields': out_fields,
        'in_rec': in_rec, 'out_rec': out_rec,
        'amt_edit': amt_edit,
        'cycle_fld': 'PC1-CYCLE-YYDDD',
        'cycle_view': cycle_view,
    }

    L.append(a('PROCEDURE DIVISION.'))
    L.extend(cmt('P0000-MAINLINE - MANDATORY CABS BATCH SHAPE.  ONE PASS '
                 'OF P2000-PROCESS CONSUMES ONE INPUT RECORD.'))
    L.append(a('P0000-MAINLINE.'))
    L.append(b('PERFORM P1000-INIT THRU P1000-EXIT.'))
    L.append(b('PERFORM P2000-PROCESS THRU P2000-EXIT UNTIL WS-EOF.'))
    L.append(b('PERFORM P8000-CONTROL THRU P8000-EXIT.'))
    L.append(b('PERFORM P9000-TERM THRU P9000-EXIT.'))
    L.append(b('STOP RUN.'))

    # how many paragraphs in each section - driven by the caller's knobs
    n_p2 = max(2, knobs['p2'])
    n_p3 = max(1, knobs['p3'])
    n_p4 = max(0, knobs['p4'])

    helper = 'P2%d0-COMPARE-%s' % (rng.randint(5, 8), rng.choice(arch['nouns']))
    helper2 = 'P3%d0-WALK-%s' % (rng.randint(5, 8), rng.choice(arch['nouns']))
    ctx['helper'] = helper
    ctx['helper2'] = helper2
    ctx['search_sub'] = rng.choice(sym['sub'])
    ctx['search_sw'] = rng.choice(sym['sw'])[0]
    ctx['walk_sub'] = rng.choice(sym['sub'])

    para_names = []

    # --- S100 initialisation
    L.extend(cmt('S100-INITIALISATION SECTION'))
    L.append(a('S100-INITIALISATION SECTION.'))
    L.append(a('P1000-INIT.'))
    L.append(b('PERFORM P1100-OPEN-FILES THRU P1100-EXIT.'))
    L.append(b('PERFORM P1200-READ-PARM THRU P1200-EXIT.'))
    load_table = rng.random() < p_table
    if load_table:
        L.append(b('PERFORM P1300-LOAD-TABLE THRU P1300-EXIT.'))
    L.append(b('PERFORM P1400-PRIME-READ THRU P1400-EXIT.'))
    L.append(a('P1000-EXIT.'))
    L.append(b('EXIT.'))
    para_names.append('P1000-INIT')

    L.append(a('P1100-OPEN-FILES.'))
    msg = rng.choice([
        "'%s OPEN FAILED - FILE STATUS BAD'",
        "'OPEN FAILED ON %s - CHECK THE ALLOCATION'",
        "'%s COULD NOT BE OPENED - STEP CANNOT RUN'",
        "'BAD FILE STATUS ON OPEN OF %s'",
        "'%s NOT AVAILABLE - OPEN REJECTED'"])
    show_fs = rng.choice([0, 0, 1, 2])
    for dd, fs in all_files:
        mode = 'INPUT' if dd in in_dds else 'OUTPUT'
        L.append(b('OPEN %s %s.' % (mode, dd)))
        L.append(b("IF %s NOT = '00'" % fs))
        if show_fs == 1:
            L.append(c("DISPLAY '%s FILE STATUS = ' %s" % (dd, fs)))
        L.append(c("MOVE 'P1100-OPEN-FILES' TO WS-AB-PARA"))
        L.append(c('MOVE %s TO' % (msg % dd)))
        L.append(c('    WS-AB-REASON'))
        if show_fs == 2:
            L.append(c("DISPLAY '%s FILE STATUS = ' %s" % (dd, fs)))
        L.append(c('PERFORM P9900-FATAL-OPEN THRU P9900-EXIT.'))
    L.append(a('P1100-EXIT.'))
    L.append(b('EXIT.'))
    para_names.append('P1100-OPEN-FILES')

    L.extend(cmt('P1200-READ-PARM - THE CYCLE DATE ARRIVES AS TWO DIGITS '
                 'AND IS PIVOTED ON DW-PIVOT-YY BEFORE ANY DATE MATH.'))
    L.append(a('P1200-READ-PARM.'))
    L.append(b('MOVE SPACES TO WS-PARM-CARD.'))
    L.append(b('ACCEPT WS-PARM-CARD FROM SYSIN.'))
    L.append(b("CALL 'CABPARMR' USING WS-PARM-CARD WS-RC-PARMR."))
    L.append(b('MOVE PC1-CYCLE-YYDDD TO WS-%s-CYCLE-YYDDD.' % tag))
    L.append(b('COMPUTE %s = 19000000 + PC1-CYCLE-YYDDD.' % cycle_view))
    L.extend(body_date_pivot(rng, sym, ctx))
    for f in rng.sample(sym['cnt'], min(len(sym['cnt']), 3)):
        L.append(b('MOVE 0 TO %s.' % f))
    L.append(a('P1200-EXIT.'))
    L.append(b('EXIT.'))
    para_names.append('P1200-READ-PARM')

    if load_table:
        L.extend(cmt(rng.choice(PARA_COMMENTS)))
        L.append(a('P1300-LOAD-TABLE.'))
        L.append(b('PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.'))
        L.append(b('PERFORM P1310-STORE-TABLE-ROW THRU P1310-EXIT'))
        L.append(c('UNTIL %s.' % sym['sw'][0][1]))
        L.append(a('P1300-EXIT.'))
        L.append(b('EXIT.'))
        L.append(a('P1310-STORE-TABLE-ROW.'))
        L.extend(body_table_load(rng, sym, ctx))
        L.append(b('PERFORM P1320-READ-TABLE-ROW THRU P1320-EXIT.'))
        L.append(a('P1310-EXIT.'))
        L.append(b('EXIT.'))
        L.append(a('P1320-READ-TABLE-ROW.'))
        L.append(b('READ %s' % in_dds[0]))
        L.append(c("AT END MOVE 'Y' TO %s." % sym['sw'][0][0]))
        L.append(a('P1320-EXIT.'))
        L.append(b('EXIT.'))
        para_names.extend(['P1300-LOAD-TABLE', 'P1310-STORE-TABLE-ROW',
                           'P1320-READ-TABLE-ROW'])

    L.append(a('P1400-PRIME-READ.'))
    L.append(b('PERFORM P2100-READ-INPUT THRU P2100-EXIT.'))
    L.append(a('P1400-EXIT.'))
    L.append(b('EXIT.'))
    L.append(a('P9900-FATAL-OPEN.'))
    L.append(b('MOVE WS-PGM-NAME TO WS-AB-PGM.'))
    L.append(b("CALL 'CABABEND' USING WS-AB-PGM WS-AB-PARA WS-AB-REASON"))
    L.append(c('WS-AB-USER-CODE WS-RC-ABEND.'))
    L.append(a('P9900-EXIT.'))
    L.append(b('EXIT.'))
    para_names.extend(['P1400-PRIME-READ', 'P9900-FATAL-OPEN'])

    # --- S200 main processing
    verbs = ['VALIDATE', 'DERIVE', 'EDIT', 'BUILD', 'APPLY', 'CHECK',
             'SELECT', 'CONVERT', 'MATCH', 'RESOLVE', 'EXPAND', 'SPLIT']
    p2_names = []
    used_nm = set()
    for i in range(n_p2):
        v = rng.choice(verbs)
        n = rng.choice(arch['nouns'])
        nm = 'P2%d00-%s-%s' % (i + 2, v, n)
        while nm in used_nm:
            n = rng.choice(FIELD_NOUNS)
            nm = 'P2%d00-%s-%s' % (i + 2, v, n)
        used_nm.add(nm)
        p2_names.append(nm)

    L.extend(cmt('S200-MAIN-PROCESSING SECTION - ONE PASS PER INPUT '
                 'RECORD.'))
    L.append(a('S200-MAIN-PROCESSING SECTION.'))
    L.append(a('P2000-PROCESS.'))
    L.append(b('ADD 1 TO WS-READ-CNT.'))
    for nm in p2_names:
        if rng.random() < 0.3:
            L.append(b('IF %s' % rng.choice(sym['sw'])[1]))
            L.extend(perf(nm, CONT))
        else:
            L.extend(perf(nm))
    L.append(b('PERFORM P2100-READ-INPUT THRU P2100-EXIT.'))
    L.append(a('P2000-EXIT.'))
    L.append(b('EXIT.'))
    L.append(a('P2100-READ-INPUT.'))
    L.append(b('IF NOT WS-EOF'))
    L.append(c('READ %s' % in_dds[0]))
    L.append(c("    AT END MOVE 'Y' TO WS-EOF-SW."))
    L.append(a('P2100-EXIT.'))
    L.append(b('EXIT.'))
    para_names.extend(['P2000-PROCESS', 'P2100-READ-INPUT'])

    pool = list(BODY_POOL)
    rng.shuffle(pool)
    need_search = False
    for i, nm in enumerate(p2_names):
        if rng.random() < 0.55:
            L.extend(cmt(rng.choice(PARA_COMMENTS)))
        L.append(a('%s.' % nm))
        gen = pool[i % len(pool)]
        L.extend(gen(rng, sym, ctx))
        if i == 0 and load_table:
            L.extend(body_table_search(rng, sym, ctx))
            need_search = True
        L.append(a('%s-EXIT.' % nm))
        L.append(b('EXIT.'))
        para_names.append(nm)
    if need_search:
        L.extend(helper_compare(rng, sym, ctx, helper))
        para_names.append(helper)

    # --- S300 output
    L.extend(cmt('S300-OUTPUT SECTION'))
    L.append(a('S300-OUTPUT SECTION.'))
    p3_names = []
    for i in range(n_p3):
        v = rng.choice(['WRITE', 'FORMAT', 'RELEASE', 'POST', 'STAGE',
                        'EMIT', 'CLOSE-OFF'])
        n = rng.choice(arch['nouns'])
        nm = 'P3%d00-%s-%s' % (i + 1, v, n)
        while nm in used_nm:
            n = rng.choice(FIELD_NOUNS)
            nm = 'P3%d00-%s-%s' % (i + 1, v, n)
        used_nm.add(nm)
        p3_names.append(nm)
    pool2 = [body_write_detail, body_print_line, body_reset, body_hash,
             body_string_build, body_move_count, body_accumulate]
    if use_susp:
        pool2.append(body_suspense)
    rng.shuffle(pool2)
    for i, nm in enumerate(p3_names):
        if rng.random() < 0.4:
            L.extend(cmt(rng.choice(PARA_COMMENTS)))
        L.append(a('%s.' % nm))
        g = pool2[i % len(pool2)]
        if g is body_print_line and not use_print:
            g = body_move_count
        if g is body_suspense and not use_susp:
            g = body_reset
        L.extend(g(rng, sym, ctx))
        if g is body_suspense:
            L.append(b('MOVE CABS-SUSPENSE-RECORD TO CABS-SUSOUT-RECORD.'))
            L.append(b('WRITE CABS-SUSOUT-RECORD.'))
        if g is body_write_detail and out_dds[0] != out_rec:
            pass
        L.append(a('%s-EXIT.' % nm))
        L.append(b('EXIT.'))
        para_names.append(nm)

    # --- S400 secondary (larger programs only)
    if n_p4:
        L.extend(cmt('S400-SECONDARY-PROCESSING SECTION'))
        L.append(a('S400-SECONDARY-PROCESSING SECTION.'))
        p4_names = []
        for i in range(n_p4):
            v = rng.choice(['SUMMARISE', 'RECONCILE', 'AUDIT', 'TRACE',
                            'COMPARE', 'ADJUST', 'NORMALISE', 'REPORT'])
            n = rng.choice(arch['nouns'] + FIELD_NOUNS)
            nm = 'P4%d00-%s-%s' % (i + 1, v, n)
            while nm in used_nm:
                n = rng.choice(FIELD_NOUNS)
                nm = 'P4%d00-%s-%s' % (i + 1, v, n)
            used_nm.add(nm)
            p4_names.append(nm)
        L.append(a('P4000-SECONDARY.'))
        for nm in p4_names:
            L.extend(perf(nm))
        L.append(a('P4000-EXIT.'))
        L.append(b('EXIT.'))
        para_names.append('P4000-SECONDARY')
        pool3 = list(BODY_POOL)
        rng.shuffle(pool3)
        for i, nm in enumerate(p4_names):
            if rng.random() < 0.5:
                L.extend(cmt(rng.choice(PARA_COMMENTS)))
            L.append(a('%s.' % nm))
            g = pool3[i % len(pool3)]
            if g is body_print_line and not use_print:
                g = body_percent
            if g is body_suspense and not use_susp:
                g = body_collar
            L.extend(g(rng, sym, ctx))
            if g is body_suspense:
                L.append(b('MOVE CABS-SUSPENSE-RECORD TO '
                           'CABS-SUSOUT-RECORD.'))
                L.append(b('WRITE CABS-SUSOUT-RECORD.'))
            L.append(a('%s-EXIT.' % nm))
            L.append(b('EXIT.'))
            para_names.append(nm)
        L.extend(body_subscript_walk(rng, sym, ctx))
        L.extend(helper_walk(rng, sym, ctx, helper2))
        para_names.append(helper2)

    # --- S800 control
    L.extend(cmt('S800-CONTROL SECTION - THE MANDATORY CABS CONTROL '
                 'BOUNDARY.'))
    L.append(a('S800-CONTROL SECTION.'))
    L.append(a('P8000-CONTROL.'))
    if n_p4:
        L.append(b('PERFORM P4000-SECONDARY THRU P4000-EXIT.'))
    if use_print:
        L.append(b('PERFORM P8010-PRINT-AUDIT-REPORT THRU P8010-EXIT.'))
    L.append(b('PERFORM P8100-BUILD-CONTROL-REC THRU P8100-EXIT.'))
    L.append(b('PERFORM P8200-CHECK-BALANCE THRU P8200-EXIT.'))
    L.append(b('PERFORM P8300-WRITE-CONTROL-REC THRU P8300-EXIT.'))
    L.append(a('P8000-EXIT.'))
    L.append(b('EXIT.'))
    para_names.append('P8000-CONTROL')

    if use_print:
        L.append(a('P8010-PRINT-AUDIT-REPORT.'))
        L.append(b('ADD 1 TO WS-RPT-PAGE-NBR.'))
        L.append(b('MOVE SPACES TO CABS-PRINT-LINE.'))
        L.append(b("MOVE '1' TO PC-CC."))
        L.append(b('MOVE WS-RPT-TITLE1 TO PC-TEXT.'))
        L.append(b('WRITE CABS-PRINT-LINE.'))
        L.append(b('MOVE SPACES TO CABS-PRINT-LINE.'))
        L.append(b("MOVE ' ' TO PC-CC."))
        L.append(b('MOVE WS-RPT-TITLE2 TO PC-TEXT.'))
        L.append(b('WRITE CABS-PRINT-LINE.'))
        wording = rng.choice([
            ['RECORDS READ', 'RECORDS WRITTEN', 'RECORDS REJECTED',
             'RECORDS SUMMARISED', 'RECORDS CARRIED FWD'],
            ['INPUT RECORDS', 'OUTPUT RECORDS', 'SUSPENDED',
             'SUMMARISED', 'HELD FOR NEXT RUN'],
            ['READ FROM INPUT', 'WRITTEN TO OUTPUT', 'REJECTED',
             'ROLLED INTO SUMMARY', 'CARRIED FORWARD'],
            ['DETAIL IN', 'DETAIL OUT', 'DETAIL SUSPENDED',
             'DETAIL SUMMARISED', 'DETAIL CARRIED FWD']])
        srcs = ['WS-READ-CNT', 'WS-WRITE-CNT', 'WS-REJECT-CNT',
                'WS-SUMM-CNT', 'WS-CFWD-CNT']
        pairs = list(zip(wording, srcs))
        if rng.random() < 0.5:
            rng.shuffle(pairs)
        extra = rng.randint(1, min(4, len(sym['cnt'])))
        for lbl, s in pairs:
            L.append(b('MOVE SPACES TO CABS-PRINT-LINE.'))
            L.append(b("MOVE ' ' TO PC-CC."))
            L.append(b("MOVE '%s' TO PC-COL-001-020." % lbl))
            L.append(b('MOVE %s TO WS-%s-CNT-EDIT.' % (s, tag)))
            L.append(b('MOVE WS-%s-CNT-EDIT TO PC-COL-021-060.' % tag))
            L.append(b('WRITE CABS-PRINT-LINE.'))
        for k in range(extra):
            L.append(b('MOVE SPACES TO CABS-PRINT-LINE.'))
            L.append(b("MOVE ' ' TO PC-CC."))
            L.append(b("MOVE 'LOCAL COUNTER %02d' TO PC-COL-001-020." % (k + 1)))
            L.append(b('MOVE %s TO WS-%s-CNT-EDIT.' % (sym['cnt'][k], tag)))
            L.append(b('MOVE WS-%s-CNT-EDIT TO PC-COL-021-060.' % tag))
            L.append(b('WRITE CABS-PRINT-LINE.'))
        L.append(a('P8010-EXIT.'))
        L.append(b('EXIT.'))
        para_names.append('P8010-PRINT-AUDIT-REPORT')

    L.append(a('P8100-BUILD-CONTROL-REC.'))
    # every one of these is an independent MOVE, and every author who ever
    # touched this suite wrote them in a different order
    context_moves = [
        'MOVE PC1-RUN-ID TO CT-RUN-ID.',
        'MOVE WS-PGM-NAME TO CT-PROCESS-ID.',
        'MOVE %d TO CT-STEP-SEQ.' % rng.randint(1, 9),
        'MOVE WS-%s-CYCLE-YYDDD TO CT-CYCLE-YYDDD.' % tag,
        'MOVE PC1-BILL-PERIOD TO CT-BILL-PERIOD.',
        'MOVE 0 TO CT-RERUN-NBR.',
        'MOVE PC1-JOBNAME TO CT-JOBNAME.',
        'MOVE PC1-STEPNAME TO CT-STEPNAME.',
    ]
    count_moves = [
        'MOVE WS-READ-CNT TO CT-READ.',
        'MOVE WS-WRITE-CNT TO CT-WRITTEN.',
        'MOVE WS-REJECT-CNT TO CT-REJECTED.',
        'MOVE WS-SUMM-CNT TO CT-SUMMARISED.',
        'MOVE WS-CFWD-CNT TO CT-CARRIED-FWD.',
    ]
    hash_moves = [
        'MOVE WS-ACC-MINUTES TO CT-HASH-MINUTES.',
        'MOVE WS-ACC-AMOUNT TO CT-HASH-AMOUNT.',
        'MOVE WS-ACC-SEQ-HASH TO CT-HASH-SEQ.',
        'MOVE WS-ACC-OCN-HASH TO CT-HASH-OCN.',
    ]
    tail_moves = [
        rng.choice(['MOVE SPACES TO CT-RESTART-KEY.',
                    'MOVE %s TO CT-RESTART-KEY.' % rng.choice(sym['txt'])]),
        'MOVE SPACES TO CT-FILLER.',
        rng.choice(['MOVE 0 TO CT-RC.',
                    'MOVE 0 TO CT-RC.',
                    'MOVE %s TO CT-RC.' % rng.choice(sym['cnt'])]),
        'MOVE SPACES TO CT-ABEND-CD.',
    ]
    rng.shuffle(context_moves)
    rng.shuffle(count_moves)
    rng.shuffle(hash_moves)
    rng.shuffle(tail_moves)
    order = [context_moves, count_moves, hash_moves, tail_moves]
    rng.shuffle(order)
    for grp in order:
        for mv in grp:
            L.append(b(mv))
    L.append(a('P8100-EXIT.'))
    L.append(b('EXIT.'))
    bal_style = rng.randint(0, 3)
    if bal_style == 0:
        L.extend(cmt('P8200-CHECK-BALANCE - EVERY RECORD READ IS EITHER '
                     'WRITTEN, REJECTED, SUMMARISED OR CARRIED FORWARD.'))
    elif bal_style == 1:
        L.extend(cmt('P8200-CHECK-BALANCE - THE REPORT LINES ARE NOT '
                     'RECORDS, SO THE WRITTEN COUNT IS ZEROED BEFORE THE '
                     'EQUATION IS TESTED.'))
    elif bal_style == 2:
        L.extend(cmt('P8200-CHECK-BALANCE - A RECORD THAT WAS HELD FOR THE '
                     'NEXT CYCLE IS BROUGHT INTO THE CARRIED FORWARD SIDE '
                     'BEFORE THE EQUATION IS TESTED.'))
    else:
        L.extend(cmt('P8200-CHECK-BALANCE - THE EQUATION IS TESTED AS IT '
                     'STANDS AND THE RETURN CODE IS SET FROM THE RESULT SO '
                     'THE SCHEDULER CAN SEE IT.'))
    L.append(a('P8200-CHECK-BALANCE.'))
    if bal_style == 1:
        L.append(b('MOVE 0 TO CT-WRITTEN.'))
    elif bal_style == 2:
        L.append(b('ADD %s TO CT-CARRIED-FWD.' % rng.choice(sym['cnt'])))
    L.append(b('IF CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED +'))
    L.append(c('        CT-CARRIED-FWD'))
    L.append(c("MOVE 'B' TO CT-BAL-IND"))
    L.append(b('ELSE'))
    L.append(c("MOVE 'O' TO CT-BAL-IND."))
    if bal_style == 3:
        L.append(b('IF CT-OUT-OF-BAL'))
        L.append(c('MOVE 0004 TO CT-RC.'))
    L.append(a('P8200-EXIT.'))
    L.append(b('EXIT.'))
    L.append(a('P8300-WRITE-CONTROL-REC.'))
    L.append(b('MOVE CABS-CONTROL-RECORD TO CABS-CTLOUT-RECORD.'))
    L.append(b('WRITE CABS-CTLOUT-RECORD.'))
    L.append(a('P8300-EXIT.'))
    L.append(b('EXIT.'))
    para_names.extend(['P8100-BUILD-CONTROL-REC', 'P8200-CHECK-BALANCE',
                       'P8300-WRITE-CONTROL-REC'])

    # --- S900 termination
    L.extend(cmt('S900-TERMINATION SECTION.'))
    L.append(a('S900-TERMINATION SECTION.'))
    L.append(a('P9000-TERM.'))
    for dd, _fs in all_files:
        L.append(b('CLOSE %s.' % dd))
    L.append(b("DISPLAY '%s - %s'."
               % (pgm, rng.choice(['RUN COMPLETE', 'NORMAL END OF JOB',
                                   'STEP COMPLETE', 'END OF RUN']))))
    disp = [("DISPLAY '  READ      = ' WS-READ-CNT."),
            ("DISPLAY '  WRITTEN   = ' WS-WRITE-CNT."),
            ("DISPLAY '  REJECTED  = ' WS-REJECT-CNT."),
            ("DISPLAY '  SUMMARISED= ' WS-SUMM-CNT.")]
    rng.shuffle(disp)
    for d in disp:
        L.append(b(d))
    for f in rng.sample(sym['cnt'], min(len(sym['cnt']), rng.randint(2, 4))):
        L.append(b("DISPLAY '  %s = ' %s." % (f[-9:], f)))
    L.append(a('P9000-EXIT.'))
    L.append(b('EXIT.'))
    para_names.append('P9000-TERM')

    text = '\n'.join(L) + '\n'
    meta = {
        'program': pgm,
        'archetype': arch_key,
        'archetype_name': ARCHETYPES[arch_key]['title'],
        'purpose': purpose,
        'size_class': size_class,
        'lines': len(L),
        'paragraphs': len(para_names),
        'redefines': sym['n_redef'] + (1 if 'WS-PARM-CARD-R2' in text else 1),
        'copybooks': ['CABSWRK'] + extra_copies +
                     (['CABSPRNT'] if use_print else []),
        'occurs': sym['occurs'],
        'ws_fields': (len(sym['cnt']) + len(sym['amt']) + len(sym['qty']) +
                      len(sym['txt']) + len(sym['sub']) + len(sym['sw'])),
        'in_dd': in_dds, 'out_dd': out_dds,
        'susout': use_susp, 'rptout': use_print,
        'in_lrecl': in_lrecl, 'out_lrecl': out_lrecl,
        'job': jobname, 'step': stepname,
        'para_signature': hashlib.sha256(
            '|'.join(para_names).encode()).hexdigest()[:16],
    }
    return text, meta


# --------------------------------------------------------------------------
# JCL builder
# --------------------------------------------------------------------------

def build_job(jobname, metas, rng):
    """Emit one JCL member running one to three of the generated programs."""
    J = []
    desc = metas[0]['purpose'][:28]
    J.append("//%s JOB (CABS,UTIL),'%s'," % (jobname, desc))
    J.append('//             CLASS=%s,MSGCLASS=X,MSGLEVEL=(1,1),'
             % rng.choice('ABCD'))
    J.append('//             NOTIFY=&SYSUID,REGION=0M,TIME=1440')
    J.append('//' + '*' * 65)
    J.append('//* %-62s*' % ('%s - %s' % (jobname, metas[0]['purpose'])))
    J.append('//* %-62s*' % '')
    for m in metas:
        J.append('//* %-62s*' % ('%s  %s' % (m['step'], m['program'])))
    J.append('//* %-62s*' % '')
    J.append('//* %-62s*' % 'UTILITY STREAM.  SCHEDULED AFTER THE NIGHTLY')
    J.append('//* %-62s*' % 'ACCESS BILLING STREAM COMPLETES.')
    J.append('//' + '*' * 65)
    for m in metas:
        J.append('//%-8s EXEC PGM=%s,REGION=%dM,'
                 % (m['step'], m['program'], rng.choice([4, 6, 8, 12])))
        J.append("//             PARM='&CYCLE'")
        J.append('//STEPLIB  DD DSN=TELCABS.CABS.LOADLIB,DISP=SHR')
        J.append('//         DD DSN=TELCABS.COMMON.LOADLIB,DISP=SHR')
        J.append('//         DD DSN=SYS1.COB2LIB,DISP=SHR')
        for dd in m['in_dd']:
            J.append('//%-8s DD DSN=TELCABS.CABS.%s(0),DISP=SHR'
                     % (dd, dd[:6]))
        for dd in m['out_dd']:
            J.append('//%-8s DD DSN=TELCABS.CABS.%s(+1),' % (dd, dd[:6]))
            J.append('//             DISP=(NEW,CATLG,DELETE),')
            J.append('//             UNIT=SYSDA,SPACE=(CYL,(%d,%d),RLSE),'
                     % (rng.choice([5, 10, 20]), rng.choice([5, 10])))
            J.append('//             DCB=(RECFM=FB,LRECL=%d,BLKSIZE=0)'
                     % m['out_lrecl'])
        if m['susout']:
            J.append('//SUSOUT   DD DSN=TELCABS.CABS.UTIL.SUSP(+1),')
            J.append('//             DISP=(MOD,CATLG,DELETE),')
            J.append('//             UNIT=SYSDA,SPACE=(TRK,(15,15),RLSE),')
            J.append('//             DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)')
        J.append('//CTLOUT   DD DSN=TELCABS.CABS.CONTROL(+1),')
        J.append('//             DISP=(MOD,CATLG,DELETE),')
        J.append('//             UNIT=SYSDA,SPACE=(TRK,(5,5),RLSE),')
        J.append('//             DCB=(RECFM=FB,LRECL=180,BLKSIZE=0)')
        if m['rptout']:
            J.append('//RPTOUT   DD SYSOUT=*')
        J.append('//SYSOUT   DD SYSOUT=*')
        J.append('//SYSUDUMP DD SYSOUT=D')
        J.append('//SYSIN    DD *')
        J.append('RN%%RUNID  %%CYCLDT%%BILLPR%-8s%-8s%s%s'
                 % (jobname, m['step'], rng.choice('YN'), rng.choice('YN')))
        J.append('/*')
        J.append('//*')
    J.append('//')
    return '\n'.join(J) + '\n'


# --------------------------------------------------------------------------
# driver
# --------------------------------------------------------------------------

SIZE_BANDS = {'S': (380, 480), 'M': (540, 850), 'L': (1050, 1450)}


def size_plan(n, rng):
    """Most 200-500, a substantial minority 500-1000, a few 1000-2000.

    Do not level this out.  A flat size distribution is one of the tells
    that an estate has been generated rather than accumulated.
    """
    n_large = max(1, round(n * 0.075))
    n_mid = max(1, round(n * 0.250))
    n_small = n - n_large - n_mid
    plan = ['S'] * n_small + ['M'] * n_mid + ['L'] * n_large
    rng.shuffle(plan)
    return plan


def build_to_target(pgm, arch, size_class, seed, jobname, step, target,
                    tag):
    """Build, measure, adjust the paragraph counts, rebuild.

    Deterministic: the per-program RNG is reseeded from the program name on
    every attempt, so a given set of knobs always yields the same bytes.
    """
    knobs = dict(DEFAULT_KNOBS[size_class])
    best = None
    for _ in range(20):
        text, meta = build_program(pgm, arch, size_class, seed, jobname,
                                   step, knobs, tag)
        d = target - meta['lines']
        if best is None or abs(d) < best[0]:
            best = (abs(d), text, meta)
        if abs(d) <= 20:
            break
        stepsz = max(1, abs(d) // 14)
        if d > 0:
            if size_class == 'L' and knobs['p4'] < 26:
                knobs['p4'] += stepsz
            elif knobs['p2'] < 30:
                knobs['p2'] += stepsz
            else:
                knobs['p3'] += stepsz
        else:
            if knobs['p4'] > 0:
                knobs['p4'] = max(0, knobs['p4'] - stepsz)
            elif knobs['p2'] > 2:
                knobs['p2'] = max(2, knobs['p2'] - stepsz)
            elif knobs['p3'] > 1:
                knobs['p3'] = max(1, knobs['p3'] - stepsz)
            else:
                break
    return best[1], best[2]


def main():
    here = os.path.dirname(os.path.abspath(__file__))
    root_default = os.path.dirname(here)
    ap = argparse.ArgumentParser(description=__doc__.split('\n')[1])
    ap.add_argument('--root', default=root_default)
    ap.add_argument('--programs', type=int, default=95)
    ap.add_argument('--jobs', type=int, default=55)
    ap.add_argument('--seed', type=int, default=19870311)
    ap.add_argument('--dry-run', action='store_true')
    args = ap.parse_args()

    rng = random.Random(args.seed)
    util_dir = os.path.join(args.root, 'BATCH', 'UTIL')
    jcl_dir = os.path.join(args.root, 'JCL')

    # allocate program names across the four archetypes
    split = {'RT': 0.25, 'EX': 0.28, 'XR': 0.23, 'CV': 0.24}
    names = []
    for k, frac in split.items():
        n = round(args.programs * frac)
        for i in range(n):
            names.append(('CABU%s%02d' % (k, i + 1), k))
    while len(names) > args.programs:
        names.pop()
    i = 1
    while len(names) < args.programs:
        names.append(('CABUZZ%02d' % i, rng.choice(list(ARCHETYPES))))
        i += 1
    rng.shuffle(names)

    sizes = size_plan(len(names), rng)

    # one unique working-storage tag per program - no two programs use the
    # same field names, which is the single biggest thing keeping clone
    # detection from grouping this tier together
    pool = list(TAG_POOL)
    rng.shuffle(pool)
    if len(pool) < len(names):
        raise SystemExit('TAG_POOL too small for %d programs' % len(names))
    tags = pool[:len(names)]

    # allocate programs to jobs - one to three steps per job
    jobs = []
    idx = 0
    jn = 7000
    while idx < len(names):
        remaining_jobs = args.jobs - len(jobs)
        remaining_pgms = len(names) - idx
        if remaining_jobs <= 1:
            take = remaining_pgms
        else:
            max_take = min(3, remaining_pgms - (remaining_jobs - 1) + 1)
            take = rng.randint(1, max(1, max_take))
        jobs.append(('CABU%04d' % jn, list(range(idx, idx + take))))
        idx += take
        jn += 10

    if not args.dry_run:
        os.makedirs(util_dir, exist_ok=True)
        os.makedirs(jcl_dir, exist_ok=True)

    metas = [None] * len(names)
    total_lines = 0
    for jobname, members in jobs:
        for si, pi in enumerate(members):
            pgm, arch = names[pi]
            step = 'STEP%03d' % ((si + 1) * 10)
            lo, hi = SIZE_BANDS[sizes[pi]]
            trng = random.Random(hashlib.sha256(
                ('T|%s|%d' % (pgm, args.seed)).encode()).hexdigest())
            target = trng.randint(lo, hi)
            text, meta = build_to_target(pgm, arch, sizes[pi], args.seed,
                                         jobname, step, target, tags[pi])
            metas[pi] = meta
            total_lines += meta['lines']
            if not args.dry_run:
                with open(os.path.join(util_dir, pgm + '.cbl'), 'w') as fh:
                    fh.write(text)

    jcl_lines = 0
    for jobname, members in jobs:
        jrng = random.Random(hashlib.sha256(jobname.encode()).hexdigest())
        text = build_job(jobname, [metas[p] for p in members], jrng)
        jcl_lines += len(text.splitlines())
        if not args.dry_run:
            with open(os.path.join(jcl_dir, jobname + '.jcl'), 'w') as fh:
                fh.write(text)

    # structural uniqueness check
    sigs = {}
    for m in metas:
        sigs.setdefault(m['para_signature'], []).append(m['program'])
    dupes = {k: v for k, v in sigs.items() if len(v) > 1}

    summary = {
        'generated': '%d COBOL programs, %d JCL members' %
                     (len(metas), len(jobs)),
        'cobol_lines': total_lines,
        'jcl_lines': jcl_lines,
        'size_distribution': {
            '200_500': sum(1 for m in metas if 200 <= m['lines'] < 500),
            '500_1000': sum(1 for m in metas if 500 <= m['lines'] < 1000),
            '1000_2000': sum(1 for m in metas if 1000 <= m['lines'] < 2000),
            'under_200': sum(1 for m in metas if m['lines'] < 200),
            'over_2000': sum(1 for m in metas if m['lines'] >= 2000),
        },
        'duplicate_paragraph_signatures': dupes,
        'seed': args.seed,
    }
    print(json.dumps(summary, indent=2))

    if not args.dry_run:
        write_manifest(here, metas, jobs, summary)
    return 0 if not dupes else 1


def write_manifest(here, metas, jobs, summary):
    lines = []
    lines.append('# BUILDER — UTILITY TIER MANIFEST (GENERATED)')
    lines.append('')
    lines.append('Produced by `BUILDER/build_families.py`. '
                 'Do not hand-edit — rerun the builder.')
    lines.append('')
    lines.append('**%s. %s COBOL lines, %s JCL lines.**'
                 % (summary['generated'],
                    '{:,}'.format(summary['cobol_lines']),
                    '{:,}'.format(summary['jcl_lines'])))
    lines.append('')
    lines.append('Seed `%d`. Rerunning with the same seed reproduces these '
                 'files byte for byte.' % summary['seed'])
    lines.append('')
    lines.append('## Size distribution')
    lines.append('')
    lines.append('| Band | Programs |')
    lines.append('|---|--:|')
    for k in ('under_200', '200_500', '500_1000', '1000_2000', 'over_2000'):
        lines.append('| %s | %d |' % (k.replace('_', '–'),
                                      summary['size_distribution'][k]))
    lines.append('')
    lines.append('## Archetype totals')
    lines.append('')
    lines.append('| Archetype | Programs | Lines |')
    lines.append('|---|--:|--:|')
    for k, v in ARCHETYPES.items():
        sel = [m for m in metas if m['archetype'] == k]
        lines.append('| %s — %s | %d | %s |'
                     % (k, v['title'], len(sel),
                        '{:,}'.format(sum(m['lines'] for m in sel))))
    lines.append('')
    lines.append('## Programs')
    lines.append('')
    lines.append('| Program | Arch | Lines | Paras | REDEFINES | OCCURS | '
                 'WS fields | Copybooks | Job.Step |')
    lines.append('|---|:--:|--:|--:|--:|--:|--:|---|---|')
    for m in sorted(metas, key=lambda x: x['program']):
        lines.append('| `%s` | %s | %d | %d | %d | %d | %d | %s | %s.%s |'
                     % (m['program'], m['archetype'], m['lines'],
                        m['paragraphs'], m['redefines'], m['occurs'],
                        m['ws_fields'], ' '.join(m['copybooks']),
                        m['job'], m['step']))
    lines.append('')
    lines.append('## Jobs')
    lines.append('')
    lines.append('| Member | Steps | Programs |')
    lines.append('|---|--:|---|')
    for jobname, members in jobs:
        lines.append('| `JCL/%s.jcl` | %d | %s |'
                     % (jobname, len(members),
                        ' '.join(metas[p]['program'] for p in members)))
    lines.append('')
    lines.append('## Structural uniqueness')
    lines.append('')
    if summary['duplicate_paragraph_signatures']:
        lines.append('**FAILED** — duplicate paragraph signatures: %s'
                     % summary['duplicate_paragraph_signatures'])
    else:
        lines.append('No two programs share a paragraph-name sequence. '
                     'Paragraph counts, field inventories, REDEFINES counts, '
                     'OCCURS limits, copybook usage and CALL patterns are '
                     'independently randomised per program, so clone '
                     'detection should not group these programs together.')
    lines.append('')
    with open(os.path.join(here, '_MANIFEST.md'), 'w') as fh:
        fh.write('\n'.join(lines) + '\n')


if __name__ == '__main__':
    sys.exit(main())
