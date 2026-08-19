# CABS Tier 5 — Wholesale Carrier Access Billing Reference Estate

A synthetic but production-standard IBM mainframe estate, built as a **test article for
modernization tooling and method**.

**527 mainframe source files. 194,990 lines of mainframe code (595 authored files and 317,767
lines in total, including the Python toolchain, the contract registers and the documentation).
27 complexity constructs placed by hand in named paragraphs. 12 seeded defects with a sealed
answer key. Two application boundaries that cannot be untangled without a business decision.**

---

## What this is, and why it exists

Every modernization assessment eventually runs into the same problem: **you cannot tell how good
the assessment was, because nobody knows the right answer.** A scanner reports 4,000 findings. A
transformation tool converts 92% of the code. A vendor deck claims 85% automation. None of those
numbers is checkable, because the estate they were measured against is somebody's production
system and its ground truth is unknown to everyone including the people who own it.

This estate has ground truth. Every construct that makes legacy code hard to reason about was
**placed deliberately, in a specific paragraph, and recorded**. Every defect was seeded on purpose,
with its business impact written down and sealed. So when a tool is pointed at it, three questions
become answerable:

1. **What did the tool find?** — score against `CONTRACTS/complexity_placement.json`.
2. **What did the tool miss?** — the difference, itemised.
3. **What could no tool have found?** — the list that determines the timeline.

The third answer is the one that matters, and it is the reason this estate was built the way it
was. The single largest body of business logic in it — which records enter a file, which are
discarded, how usage is aggregated, how fractional cents are treated — sits in twenty-seven
sort-exit modules that **appear in no call graph, no `EXEC PGM=` statement, and no `CALL` literal
anywhere in the estate**. See `SORTEXIT/_README.md`.

This is not a toy. The COBOL compiles. The control-record framework balances. The revision
histories span 1987–2019 and some of them are wrong, on purpose, the way real ones are.

---

## The domain — wholesale carrier access billing

CABS is what a local exchange carrier uses to bill **other carriers** for use of its network. Not
consumers. The counterparties are interexchange carriers, competitive local exchange carriers,
other incumbents and wireless carriers, and the amounts are large, the arrangements are negotiated
or tariffed, and the regulator is watching.

Six revenue streams are modelled, and each of them drives a specific family of programs:

**Switched access.** When a long-distance carrier originates or terminates a call on the local
network, it pays per minute, per rate element. This estate rates **five elements per call** —
originating access, terminating access, local transport, tandem switching, carrier common line —
in `BATCH/RATING/CABRAT03.cbl`, at 4,290 lines the second largest program here.

**Special access.** Dedicated circuits, billed as a fixed monthly rate with proration for partial
periods and term-commitment discounts. `CABRAT04`.

**Unbundled network elements (UNE).** Individual pieces of the network — loops, ports, transport —
leased to a competitor under the 1996 Act. `CABRAT05`.

**Meet-point billing.** When a call traverses two local carriers' networks to reach the
interexchange carrier, the two split the access revenue by a **billing percentage** that each files
independently. `BATCH/SETTLE/CABSET01.cbl`, 2,131 lines. Roughly **6% of the meet-point percentage
pairs in the generated reference data do not sum to 100.00000**, because the two carriers file
independently and sometimes disagree. That is not a data bug; it is the condition the settlement
programs exist to handle, and the settlement family is where several of the estate's
hardest reconciliation questions sit.

**Reciprocal compensation.** Carriers pay each other for terminating each other's local traffic.
The economics were distorted by ISP-bound traffic — dial-up internet calls terminate and never come
back — so agreements carry an **ISP-bound minute cap**. `BATCH/SETTLE/CABSET05.cbl`, 2,057 lines.

**CMDS / RAO settlement.** The industry's Centralized Message Distribution System exchanges billing
records between carriers by Revenue Accounting Office code, in a **180-byte fixed industry format
that is entirely zoned decimal** — no COMP-3, because the exchange format predates the internal
packed layouts and has never been renegotiated. `CABSET07` outbound, `CABSET08` inbound.

**PIU / PLU jurisdictional factors, and retroactive restatement.** This is the part that makes CABS
genuinely hard, and it is why `BATCH/JURIS/CABJUR07.cbl` is the largest program in the estate at
4,075 lines.

Access charges differ by jurisdiction — interstate traffic is priced under the federal tariff,
intrastate under the state tariff, local under the interconnection agreement. But on a trunk group
carrying mixed traffic **you cannot tell which call was which**. So the carrier files a **Percent
Interstate Use** factor: "treat 63.47% of the minutes on this trunk group as interstate." Below
that, a **Percent Local Use** factor splits the intrastate remainder into local and toll.

The factors are filed quarterly, per carrier, per state, per LATA — and **they are filed
retroactively**. A carrier files a corrected factor for a quarter that has already been billed. The
system must then reprice every already-billed record under both the old and the new factor, compute
the delta at five decimal places, apportion it across the billed rate elements, accumulate it per
carrier / state / jurisdiction, and raise adjustment records. `CABJUR07` does exactly that, and it
carries five separate Julian-date routines.

---

## Repository layout

File counts are actual, counted from the tree. `DATA/` (generator output) and `__pycache__` are
excluded — they are produced, not authored. In the tree below the first number after a folder is
**every file it contains**, including its `_MANIFEST.md`; the program count and line count that
follow are for the source members only.

```
CABS-MAINFRAME-DEMO-TIER5/            595 authored files   527 of them mainframe source, 194,990 ln
│
├── README.md                                          ← you are here
├── CONVENTIONS.md                                     ← build rules. Read first.
├── WITHHOLD_FOR_BLIND_RUN.md                          ← what to remove before a blind run
│
├── BATCH/                                    203 ── the COBOL estate, 194 pgms, 146,583 ln
│   ├── INGEST/       13 ── 12 pgms   7,825 ln  EMI usage edit, validate, dedup, split, enrich
│   ├── RATING/       15 ── 14 pgms  15,416 ln  rate table load, 5-element switched access, UNE,
│   │                                           special access, banded rates, min/max, overrides
│   ├── JURIS/        12 ── 11 pgms  13,485 ln  PIU/PLU engine + retroactive restatement
│   ├── SETTLE/       14 ── 13 pgms  14,403 ln  meet-point, reciprocal comp, CMDS/RAO, netting
│   ├── BILLCALC/     13 ── 12 pgms  13,815 ln  trigger, detail assembly, balance, tax, numbering
│   ├── FORMAT/       10 ──  9 pgms   8,081 ln  print, EDI 811, tape/media, carriage control
│   ├── REPORT/        9 ──  8 pgms   8,617 ln  daily balancing, revenue, suspense, month-end close
│   ├── COMMON/       13 ── 12 pgms   4,496 ln  called subprograms — the documented P8000 exception
│   ├── CONTROL/       9 ──  8 pgms   5,697 ln  IMS DL/I ×6 + MQ ×2      [REFERENCE-ONLY]
│   └── UTIL/         95 ── 95 pgms  54,748 ln  generated utility tier (see BUILDER/)
│
├── CTC/                                       26 ── multi-site divergence: 12 regional centres
│   ├── _MANIFEST.md, _SITE_INDEX.md            2      21 COBOL members 21,107 ln, 3 run JCL 236 ln
│   ├── SITE01/ ATL  3    SITE05/ DAL  3    SITE09/ CHI  3
│   ├── SITE02/ CLT  2    SITE06/ NSH  2    SITE10/ CLE  1
│   ├── SITE03/ HOU  2    SITE07/ PHX  1    SITE11/ NWK  2
│   └── SITE04/ STL  1    SITE08/ IND  2    SITE12/ SAC  2
│
├── JCL/                                      194 ── job control, 8,107 ln
│   ├── (131 job members)                     131      CABING01, CABRAT01, CABJ1*, CABS2*-CABS7*,
│   │                                6,266 ln          CABU7* ×54, CABGDGDF, CABCTLRP
│   ├── PROCS/        27 ── 18 .prc + 9 .jcl     944 ln  shared job fragments (complexity 11)
│   ├── CTLCARDS/     18 ── 18 .ctl              273 ln  **business rules live here** (cplx 14)
│   └── CTLCARDS/MVT/ 18 ── 17 .ctl + _README    624 ln  MVT-era rewrites of the same cards, for
│                                                        the sort TK4- actually ships
│
├── COPYBOOKS/                                 18 ── the frozen data architecture. Do not edit.
│                                       733 ln         CABSWRK nests CABSERR + CABSDATE + CABSCTL
│
├── SORTEXIT/                                  29 ── 27 E15/E35 exits, 8,186 ln  [REFERENCE-ONLY]
│                                                     **the largest blind spot. Read _README.md.**
│
├── VSAM/                                      21 ── 20 IDCAMS members, 1,351 ln
│                                                     11 datasets declared nowhere else
│
├── HLASM/                                      6 ── 5 members, 4 modules, 1,749 ln  [REF-ONLY]
│   └── EMERG/                                         CABDATCV exists TWICE, pivot 70 vs 68
│
├── IMS/                                       15 ── 4 DBDs + 2 index DBDs + 6 PSBs + gen job
│                                                     746 ln. 2 databases have NO DBD. [REF-ONLY]
│
├── DB2/                                       11 ── DDL, DCLGEN, BIND, GRANT, RUNSTATS
│                                                     1,193 ln  [REFERENCE-ONLY]
│
├── ONLINE/                                    15 ── CICS suite  [REFERENCE-ONLY]
│   ├── (6 programs)                    2,638 ln       CAB0-CAB5, pseudo-conversational
│   └── BMS/            8 ── 6 mapsets    699 ln       two vintages: 1991-93 and 2004
│
├── PLI/                                        5 ── 4 programs, 1,352 ln  [REFERENCE-ONLY]
├── REXX/                                       7 ── 6 execs, 872 ln  [REFERENCE-ONLY]
│
├── GENERATORS/                                 8 ── scale-parameterised EBCDIC data generators
│                                                     Python 3.9+, no dependencies, 58 unit tests
├── HARNESS/                                   10 ── L1-L5 bill-to-bill comparison harness
│                                                     three-way verdict, 39 unit tests,
│                                                     containment test suite
├── BUILDER/                                    4 ── the generator that produced BATCH/UTIL,
│                                                     plus verify_syntax.py, the build gate
├── CONTRACTS/                                  6 ── complexity placement register (authoritative),
│                                                     dataset contracts, canonical interchange spec
├── SEALED/                                     5 ── **the answer key. Withhold for a blind run.**
├── DOCS/                                       7 ── run book, CAST guide, traceability matrix,
│                                                     process contract register, audit report,
│                                                     2 assessment .docx
└── HUB/                                        2 ── build_hub.py + index.html, the interactive
                                                     knowledge graph. Regenerates byte-identical.
```

**Line counts by artefact type** — every mainframe extension in the tree, counted directly:

| Type | Files | Lines |
|---|---:|---:|
| COBOL (`.cbl`) | 248 | 178,514 |
| JCL (`.jcl`) | 167 | 8,627 |
| Assembler (`.asm`) | 5 | 1,749 |
| PL/I (`.pli`) | 4 | 1,352 |
| Copybooks (`.cpy`) | 23 | 1,061 |
| REXX (`.exec`) | 6 | 872 |
| Sort control cards (`.ctl`) | 35 | 724 |
| BMS mapsets (`.bms`) | 6 | 592 |
| PROCs (`.prc`) | 18 | 554 |
| DB2 DDL/SQL (`.ddl`, `.sql`) | 3 | 558 |
| IMS DBD/PSB (`.dbd`, `.psb`) | 12 | 387 |
| **Mainframe subtotal** | **527** | **194,990** |
| Python toolchain (`.py`) | 17 | 12,886 |
| Contracts and answer keys (`.json`, `.yaml`) | 11 | 100,583 |
| Documentation (`.md`) | 35 | 7,814 |
| Hub (`.html`) | 1 | 1,494 |
| Spreadsheets and documents (`.xlsx`, `.docx`) | 4 | — |
| **Total authored** | **595** | **317,767** |

Of the 248 COBOL programs, **204 (159,377 lines) are OS/VS COBOL and target Hercules TK4-**; the
other **44 (19,137 lines) are Enterprise COBOL** and are enumerated in the exemption table in
`CONVENTIONS.md`.

> **`HUB/index.html` counts on a different basis and will not match these figures.** The hub walks
> the whole tree *including* `DATA/`, which is generated output, and *excluding* `HUB/` itself, so
> it reports 623 files and 745,298 lines. Neither number is wrong; they answer different questions.
> The figures above are what was authored. Re-run `python3 HUB/build_hub.py` after any change to
> the tree and the hub will agree with itself again.

---

## Runnable versus reference-only

The estate is deliberately split. **159,377 lines of COBOL across 204 programs target Hercules
TK4- / MVS 3.8j and are written in OS/VS COBOL (1974 standard). The remaining 44 programs
(19,137 lines) are authored to production standard for static analysis and do not execute on
TK4-.**

`CONVENTIONS.md` fixes the rule: *"NO `EVALUATE` … NO reference modification … NO `INITIALIZE`, NO
inline `PERFORM`, NO `END-IF`/`END-PERFORM` scope terminators"* in the runnable layer. The
Enterprise COBOL exemption is not a general licence — it is an explicit list of folders and files
in `CONVENTIONS.md`, every exempt program carries `COMPILER    : ENTERPRISE COBOL` in its header
block, and `BUILDER/verify_syntax.py` check 5 reads that list and fails the build on any hit
outside it. **Two vintages, deliberate, and enumerated.**

| Folder | Files | Lines | Status | Why |
|---|---:|---:|---|---|
| `BATCH/INGEST` | 13 | 7,825 | **RUNNABLE** | OS/VS COBOL 1974 |
| `BATCH/RATING` | 15 | 15,416 | **RUNNABLE** | OS/VS COBOL 1974 |
| `BATCH/JURIS` | 12 | 13,485 | **RUNNABLE** except `CABJUR10` | `CABJUR10` is Enterprise COBOL + DB2 precompiler |
| `BATCH/SETTLE` | 14 | 14,403 | **RUNNABLE** except `CABSET12`, `CABSET13` | Both are Enterprise COBOL + DB2 precompiler |
| `BATCH/BILLCALC` | 13 | 13,815 | **RUNNABLE** | OS/VS COBOL 1974 |
| `BATCH/FORMAT` | 10 | 8,081 | **RUNNABLE** | OS/VS COBOL 1974 |
| `BATCH/REPORT` | 9 | 8,617 | **RUNNABLE** | OS/VS COBOL 1974 |
| `BATCH/COMMON` | 13 | 4,496 | **RUNNABLE** | OS/VS COBOL 1974, called subprograms; the documented `P8000-CONTROL` exception |
| `BATCH/UTIL` | 95 | 54,748 | **RUNNABLE** | OS/VS COBOL 1974, generated, verified conformant |
| `CTC/` | 26 | 21,585 | **RUNNABLE** | OS/VS COBOL 1974 ×12 sites |
| `JCL/` | 194 | 8,107 | **RUNNABLE** with edits | Symbolics are scheduler tokens — see below |
| `VSAM/` | 21 | 1,351 | **RUNNABLE** with two corrections | Target volumes must be created; `CABVS130` and `CABVS140` carry syntax that needs fixing |
| `COPYBOOKS/` | 18 | 733 | **RUNNABLE** | Frozen |
| `BATCH/CONTROL` | 9 | 5,697 | REFERENCE-ONLY | No IMS DB, no DL/I batch region, no MQ on MVS 3.8j. Enterprise COBOL. |
| `SORTEXIT/` | 29 | 8,455 | REFERENCE-ONLY | Enterprise COBOL with pointers; would need static save-area linkage; `CABSE15B` opens a QSAM file the control cards do not allocate |
| `IMS/` | 15 | 746 | REFERENCE-ONLY | MVS 3.8j has no IMS |
| `DB2/` | 11 | 1,193 | REFERENCE-ONLY | MVS 3.8j has no DB2 |
| `ONLINE/` incl. `BMS/` | 15 | 3,422 | REFERENCE-ONLY | No CICS region, no BMS macro library |
| `PLI/` | 5 | 1,413 | REFERENCE-ONLY | OS PL/I V2 syntax; `CABLGCNV` needs an `%INCLUDE` not in this repo |
| `REXX/` | 7 | 928 | REFERENCE-ONLY | MVS 3.8j predates the TSO/E REXX level used |
| `HLASM/` | 6 | 1,839 | REFERENCE-ONLY | Later macro library than TK4- ships |

### Three things that will surprise you when you try to run it

**1. No job member submits unedited.** `CABING01.jcl` says so in its own comment box: the `&CYCLE`,
`&BILLPER`, `&RUNID`, `&MODE` tokens are **plain-text placeholders the scheduler replaces before
the job reaches the internal reader**. The CABS team uses a single ampersand; the SETL team uses a
double percent (`%%CYCLDT`). Two conventions, one estate. `REXX/CABGENJC.exec` did the substitution
in period — from a control table that is not in this repository — and it is reference-only.

**2. Fifteen of the eighteen sort control cards use statements the available sort does not
support.** TK4- has no licensed DFSORT. The community OS/360 Sort/Merge is MVT-era and has **no
`INCLUDE`, no `OMIT`, no `SUM FIELDS`, no `INREC`/`OUTREC`, no `OUTFIL`, no `FIELDS=COPY`, and no
`OPTION EQUALS`**. Three cards (`CABSRT04`, `CABSRT12`, `CABSRT15`) carry only
statements it accepts, and every exit all three name is reference-only. This is not an oversight — it is *why* the business logic ended up in E15/E35
exits in the first place. `JCL/CTLCARDS/MVT/` holds an OS/360 Sort/Merge equivalent for each of
the seventeen, written with only `SORT`/`MERGE`, `RECORD`, `MODS` and `END`; the original cards
in `JCL/CTLCARDS/` are untouched because they are the historical record of what the rules are.

**3. The twelve most-called modules in the estate appear in no JCL at all.** `CABPARMR` is called
from **141 sites**; `CABHASH` — which accumulates the four hash totals the entire balancing
framework rests on — from **120 sites**. Both live in `BATCH/COMMON/`, which holds twelve called
subprograms covering **519 call sites** and carries no `EXEC PGM=` anywhere. A call graph built
from job steps contains none of them, and a build list driven off the JCL will not compile them.
The only literals still without source are `CBLTDLI` (36 sites) and the seven MQ verbs (15), both
confined to the reference-only `BATCH/CONTROL/`.

`DOCS/HERCULES_RUNBOOK.md` §11 is the complete, blunt accounting.

---

## The two applications, and the coupling that blocks everything

`CONVENTIONS.md`: *"Datasets: `TELCABS.<APP>.<NAME>` — apps are `CABS` (access billing) and `SETL`
(settlement)."*

| | **CABS** — access billing | **SETL** — settlement |
|---|---|---|
| Programs | 161 batch + 95 utility + 21 CTC | 13 batch |
| Owns | `TELCABS.CABS.*` | `TELCABS.SETL.*` |
| Does | Rate the usage. Determine jurisdiction. Assemble, tax, number and print the invoice. | Split meet-point revenue. Calculate reciprocal compensation. Exchange CMDS/RAO. Net and settle per counterparty. |
| Runs | Daily (ingest + rating), quarterly (jurisdiction), monthly (billing, format, report, close) | Monthly |

They are separate applications with separate load libraries, separate scheduling groups and
separate teams. **And they read each other's files directly.**

| Program | App | Reads | Owned by |
|---|---|---|---|
| `CABJUR09` | CABS | `TELCABS.SETL.SETTLE.MASTER` | SETL |
| `CABBIL06` | CABS | `TELCABS.SETL.NET` | SETL |
| `CABRPT05` | CABS | `TELCABS.SETL.SETTLE.ALL` | SETL |
| `CABSET01` | SETL | `TELCABS.CABS.BILLDTL` | CABS |
| `CABSET02` | SETL | `TELCABS.CABS.CDR.PLU` | CABS |
| `CABSET05` | SETL | `TELCABS.CABS.CDR.RECIP` | CABS |
| `CABSET09` | SETL | `TELCABS.CABS.CARRIER` | CABS |
| `CABSET11` | SETL | `TELCABS.CABS.BILLHDR` | CABS |
| `CABCTL05` | SETL | writes `TELCABS.SETL.MASTER` from a CABS-side IMS update | both |

### Why this is *the* modernization blocker

**There is no interface to preserve.** The coupling is a filename and a record layout. There is no
API, no message, no contract, no versioning, no owner. `CABSET01` opens `TELCABS.CABS.BILLDTL`,
reads a variable-length record with one to forty rate elements, walks the ODO, and takes what it
needs. If CABS changes that layout, SETL breaks — and nothing anywhere in the estate says so.

The practical consequence for sequencing:

- **You cannot migrate CABS first.** Move the bill detail to a new store and five SETL programs stop
  working, including the meet-point settlement that determines what nine counterparties are paid.
- **You cannot migrate SETL first.** Move the settlement master and three CABS programs stop
  working, including the invoice that nets the counterparty position onto the customer's bill.
- **You cannot migrate both at once** without a big-bang cutover across two applications, two
  teams, two schedules and a regulatory filing cycle.
- **The strangler-fig option requires building the interface that does not exist** — nine data
  contracts, from source, with no specification to work from, and with the record layouts carrying
  known self-inconsistencies (see `GENERATORS/_README.md` §8).

That interface-definition work is invisible in every static analysis report, because a scan finds
the *coupling* and cannot find the *contract*. It is the single largest schedule risk in the
programme, and it is the reason `DOCS/CAST_IMAGING_GUIDE.md` insists the estate be loaded as **two
applications, not one**.

---

## The 27 complexities

Every construct below is placed in a named paragraph, in a named file, and recorded in
`CONTRACTS/complexity_placement.json` — which `CONVENTIONS.md` makes authoritative: *"Do not invent
complexity placements. Implement ONLY what `CONTRACTS/complexity_placement.json` assigns to your
files."*

| # | Name | Plain English |
|---|---|---|
| 1 | Fall-through | a missing stop lets logic run on into the next rule |
| 2 | Dynamic call | the program to run is chosen at run time |
| 3 | Hidden error handler | failure logic sits where nobody looks for it |
| 4 | Redefines | one memory block means different things at different times |
| 5 | Compile-time renaming | the field's real name differs from the one in the code |
| 6 | Condition ranges | named value ranges quietly overlap |
| 7 | Variable-length records | the size of a record changes with its content |
| 8 | Nested copybooks | shared data definitions stack inside each other |
| 9 | Job parameters | job settings arrive at submission and stay invisible until then |
| 10 | Generation files | dated file versions are selected by position rather than by name |
| 11 | Shared job fragments | reusable job blocks change behaviour where they are included |
| 12 | Library search order | the search path decides which copy of a program runs |
| 13 | Job overrides | a step's settings are replaced at run time |
| 14 | Sort utility logic | business rules sit inside sort control cards |
| 15 | File definition jobs | a job creates files that are declared nowhere else |
| 16 | Cross-application access | one application reads another's data files directly |
| 17 | Two stores, one update | relational and indexed data fall out of step on a failure |
| 18 | Append writes | records are added with no key, so order carries meaning |
| 19 | Two-digit year | dates break once they pass a cutoff |
| 20 | String assembly | text is built piece by piece, so meaning spreads across lines |
| 21 | Rounding rules | the original rounds one way and the target must be told to match |
| 22 | Print control | layout characters also carry logic |
| 23 | Character scanning | data is read byte by byte with assumptions about position |
| 24 | Sort inside a program | an ordering step hides within business logic |
| 25 | Julian dates | year plus day numbering brings awkward arithmetic |
| 26 | Dead code | logic can never run and was never removed |
| 27 | Dormant feature | a capability was switched off years ago and still sits in the code |

**Traceability.** `DOCS/Complexity_Traceability_Matrix.xlsx` maps each construct to its exact file
and paragraph and is the scoring instrument for a tool assessment.
`DOCS/Process_Contract_Register.xlsx` carries the process-boundary contracts. Both are derived from
`CONTRACTS/complexity_placement.json`, which remains authoritative — if a workbook and the JSON
disagree, the JSON wins. Each family's `_MANIFEST.md` carries the same placements in prose as a
third, independent check.

**Four worth singling out, because they are where the exercise is won or lost:**

- **14 — sort utility logic.** Thirteen placements. The `SUM FIELDS` in `CABSRT03` is the estate's
  only dedup-by-summing rule and `CABING01.jcl` STEP070's own comment says *"NO CABING PROGRAM
  KNOWS ABOUT IT."* `CABSRT13`'s EDI partner list is four hardcoded OCNs in a control card.
  `OPTION EQUALS` appears on fifteen of eighteen cards and is what makes every accumulator in the
  estate correct.
- **12 — library search order.** `CABDATCV` exists twice with **century pivot 70 and 68** and
  different leap-year rules. The same `CALL 'CABDTCNV'` resolves a two-digit year of 68 to **1968 in
  the settlement jobs and 2068 in the rating jobs**, decided by STEPLIB order at load time.
- **21 — rounding rules.** `CONVENTIONS.md` is explicit: *"Deliberate inconsistency is required:
  some programs `COMPUTE … ROUNDED`, others truncate on the same field. Do not 'fix' this."*
  `CABJUR05`'s revision history records that V2.02 aligned them in 2000 and V2.03 backed it out in
  2002. And `RT-ROUND-RULE` is drawn **per rate record**, so sibling records for the same element in
  different states disagree.
- **27 — dormant feature.** `CABS2450` still runs every month and has produced an empty settlement
  file since July 2011. `CABFMT09` runs every cycle and produces an empty print file — last
  campaign 2009. **Dead code can be deleted. Dormant code is a business decision.**

---

## Seeded defects

**This estate contains twelve deliberately seeded defects, D1 through D12. They are not annotated
in the source.** No comment names a construct or admits a defect anywhere in the estate — that is
a build rule.

| Key file | Defects |
|---|---:|
| `SEALED/answer_key_ingest_rating.json` | 3 |
| `SEALED/answer_key_juris_settle.json` | 4 |
| `SEALED/answer_key_bill_format_report.json` | 4 |
| `SEALED/answer_key_reference_layer.json` | 1 |
| `SEALED/defect_placements.json` | the placement register for all 12 |

Each key carries the full detail: the file, the paragraph, the mechanism, the business impact and
the modernization response. Nothing outside `SEALED/` says which program carries which defect.

### The answer key must be withheld for a blind run

**So must `HARNESS/defect_signatures.json`.** It names each defect and describes its fingerprint,
and handing it to a blind run leaks the answer as surely as the key does. `HARNESS/verdict.py`
raises `BlindRunError` on either, by design.

The intended protocol, from `HARNESS/_README.md` §5:

```bash
# 1. Blind. Answer key and signature file both refused.
python3 run_compare.py --legacy L --candidate C --blind --out ../DATA/blind
#    -> every variance is DIVERGENT, no attribution, no defect list

# 2. Attributed. Same inputs, same comparison, key applied afterwards.
python3 run_compare.py --legacy L --candidate C --out ../DATA/attributed

# 3. The difference between the two reports is the score.
```

The variance lists are identical. **Only the verdicts change.** That is the proof the comparison
engine was not tuned to the answer.

A blind run always exits `1` if there is any variance at all, because without the key nothing can be
classified as by-design. **That is correct behaviour, not a bug: a blind run is a measurement, not
a gate.**

Three of the twelve are only observable under specific conditions. One needs the band-boundary
probes — tariff band boundaries are round numbers (50,000 / 100,000 / 250,000 / 500,000 minutes)
and a random sample will essentially never land on one, so the generator places probes
deliberately. Another needs the STRESS profile or larger, because it only manifests when the sort
spills across more than one work dataset. A third needs a bill detail line carrying more rate
elements than an ordinary account produces, which no profile below STRESS reaches on its own and
which the generator can be asked to place. And seven of the twelve are only observable **after the
batch estate has actually run**. Which defect is which is in `SEALED/`.

---

## Quick start

### Run the estate on Hercules
→ **`DOCS/HERCULES_RUNBOOK.md`**

Install-from-scratch complete: Hercules and TK4-, the OS/360 Sort/Merge, DASD allocation, uploading
source into PDS members, compile and link JCL, loading generated data, the full job execution order,
expected elapsed times, verifying a clean run through the control-record chain, restart and
recovery, and a complete statement of what will not run.

**Read §11 first if your goal is a working batch cycle.** It will change how you plan the work.

### Generate data
→ **`GENERATORS/_README.md`**

```bash
cd GENERATORS
python3 -m unittest test_gen_common -v          # 58 tests. COMP-3 correctness is load-bearing.
python3 generate.py --profile SMOKE --days 1 --seed 20260815 --outdir ../DATA/smoke
```

Everything is **EBCDIC cp037**, fixed-length, laid out from the frozen copybooks. **No layout is
restated in Python** — `gen_common.py` parses the `.cpy` members directly, so the generator cannot
drift from the data architecture. Where a copybook disagrees with itself, the generator **reports
and continues; it does not repair.**

Four volume profiles: SMOKE (50K CDRs/day), DAILY (500K), STRESS (2M), TARGET (100M — cloud side
only; the legacy estate under Hercules is not sized for it). Output is byte-identical for a given
seed and **independent of worker count**.

### Compare a candidate against the legacy
→ **`HARNESS/_README.md`**

```bash
cd HARNESS
python3 -m unittest test_canonical -v           # 39 tests
python3 run_compare.py --legacy ../DATA/legacy --candidate ../DATA/candidate \
                      --contracts contracts/compare_contract.json --out ../DATA/attributed_run
```

Five levels — **L1** record, **L2** field, **L3** control, **L4** bill, **L5** settlement — run at
every process boundary, against a canonical form **that neither side owns**. Comparing an EBCDIC
packed record to a UTF-8 JSON record byte by byte is meaningless; decoding the mainframe side into
whatever the target produces is worse, because it makes the target's format the definition of
correct. So both sides normalise into a third form, and nothing is compared before that point.

Three-way verdict: `MATCH`, `DIVERGENT-BY-DESIGN` (the variance traces to a known seeded defect —
scored **positively**, the transform found something real), and `DIVERGENT` (blocks). A two-way
pass/fail forces every real finding into the same bucket as every bug, and the standard response is
to widen a tolerance until the test goes quiet.

A level that did not run says **`NOT RUN`**, never `MATCH`.

### Load into CAST Imaging
→ **`DOCS/CAST_IMAGING_GUIDE.md`**

Application split, technology roots, copybook and PROC paths, Inference Engine configuration —
then the ground truth of what CAST should find, the itemised list of what it cannot, and how to
score the difference.

### Regenerate the utility tier
→ **`BUILDER/_README.md`**

```bash
python3 BUILDER/build_families.py
```

Deterministic — same seed, same bytes. The generator's overriding concern is that the 95 utility
programs **must not look like clones**, because a false duplication signal on 95 files would have to
be explained away in front of a client, which is worse than not having the tier at all. Measured
result: no two programs share a paragraph-name sequence; longest identical run between any two
programs is 35 lines; highest line-level similarity is 0.60. The residual is the mandatory control
boilerplate that `CONVENTIONS.md` requires of every program — **which is a true finding about the
estate, not an artefact of generation.**

---

## The control-record framework

One design decision runs through every program in the estate and is worth understanding before you
read any of them.

`CONVENTIONS.md`: **"`P8000-CONTROL` is NOT optional."**

Every program writes a `CABS-CONTROL-RECORD` (`COPYBOOKS/CABSCTL.cpy`, FB 180) to DD `CTLOUT`,
populating five counts and four hash totals, then evaluates:

```
CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED + CT-CARRIED-FWD
```

`BATCH/REPORT/CABRPT01.cbl` reads every control record in a cycle, reproves the equation for each,
walks a **63-entry process chain** proving that what one process wrote is what the next one read,
checks hash continuity and step sequence, and prints **the verdict line operations read before
releasing the print stream**. In the month-end stream it runs in line so its return code governs
the close.

**Four honest caveats, because a green report here is not the same as a correct run:**

1. **A sort step writes no control record at all.** When a sort exit suppresses 40,000 zero-value
   lines, the run still balances — the next program simply reads fewer records and reports the
   smaller number as its own `CT-READ`. There is no reconciliation point at which the difference
   becomes visible. `HARNESS/contracts/` records the `CABRAT09 → CABBIL01` edge as a **deliberate
   gap** rather than pretending the chain is continuous.
2. **Three programs satisfy the equation by moving zero into a count.** `CABCTL01` counts the rows
   that did not fit on its extract, then moves **zero** into `CT-SUMMARISED`. The run always
   balances; the loss appears only on a printed listing.
3. **A green balancing proof is not the same as a correct one.** The estate carries twelve
   deliberately seeded defects. Several of them are invisible to the estate's own control
   reports: every control total agrees, every register prints clean, and the money is still
   wrong. Which
   programs carry which defect, what each one does, what it costs and what the correct
   modernization response is are all recorded **only** in `SEALED/`. Nothing in this README,
   in the manifests, in `CONTRACTS/`, in `HUB/` or in the source comments names them.
4. **`CT-BAL-IND = 'O'` being routine in operations is not a reason to skip the check** — it is the
   reason to make it.

---

## Proposed target-state decomposition

**No Java is written here, and none will be. The transformation is what the test is for.** What
follows is the target shape the estate's own structure argues for — offered as a hypothesis to be
tested against, not a design to be implemented ahead of the exercise.

### The decomposition the data argues for

The seams are not where the JCL job boundaries are. They are where the **data ownership** is, and
the estate makes them visible once you stop reading it as a batch stream.

| Service | Owns | Absorbs | Why this seam |
|---|---|---|---|
| **Usage Ingestion** | Raw and edited usage, suspense | `CABING01`–`CABING09`, `CABING11` | Pure record-at-a-time transformation. No shared state, no cross-application read, restartable by key. **The easiest thing to move and the right thing to move first.** |
| **Reference Data** | Carrier, circuit, rate, tariff | `CABRAT01`, `PLI/CABRTMNT`, the CICS rate/factor maintenance transactions, `CABURT*` | Everything reads it, almost nothing writes it. Classic read-mostly master data. |
| **Jurisdiction & Factors** | PIU/PLU factors, factor history, restatement adjustments | `CABJUR01`–`CABJUR11`, `CABING10`, `PLI/CABFCVAL` | Its own quarterly lifecycle, its own regulatory driver, its own retroactive semantics. **The restatement engine is a service, not a batch step.** |
| **Rating** | Rated usage | `CABRAT02`–`CABRAT13` | The dynamic-call dispatcher is already a plugin architecture — it just resolves plugins from a table at run time instead of from a registry. |
| **Billing** | Bill detail, bill header, invoice register | `CABBIL01`–`CABBIL12` | Owns the invoice. |
| **Settlement** | Settlement master, disputes, netting, CMDS exchange | `CABSET01`–`CABSET13`, `CABCTL05`–`CABCTL08` | **The second application. Give it a real interface.** |
| **Document & Delivery** | Print stream, EDI, media | `CABFMT01`–`CABFMT09` | Presentation. Should never have been coupled to carriage-control semantics. |
| **Control & Assurance** | Control records, balancing, close | `CABRPT01`–`CABRPT08`, `CABCTLRP` | **Elevate this. It is the estate's best idea and it is buried in a report program.** |

### Data stores

| Store | Holds | From |
|---|---|---|
| **Relational (OLTP)** | Carrier, circuit, rate, factor, agreement, dispute, invoice register, settlement ledger | The four VSAM KSDS masters, the IMS CARRIER / CIRCUIT / SETTLEMENT / BILLHIST databases, the seven DB2 tables |
| **Object store, partitioned** | Raw and rated usage, bill detail, archive | The GDG chains and the VB 1651 bill detail |
| **Columnar / lakehouse** | Rated usage and bill detail for revenue assurance, rate element studies, separations filing | `CABRPT02`, `CABRPT06`, the tariff analysts' workstation extract |
| **Ledger / append-only** | The control-record chain and the audit log | `CABSCTL` and `TELCABS.CABS.AUDIT.LOG` — **already append-only with position-carrying semantics** |
| **Event log** | Inter-service handoffs and the CMDS exchange | Replaces the file handoffs and the MQ pair |

### Patterns, and what each one is replacing

| Pattern | Replaces | Note |
|---|---|---|
| **Managed batch orchestration with explicit DAG dependencies** | JCL `COND=`, the internal reader chain in `CABS7000`, the scheduler symbolics in `PARMTBL` | The dependencies exist today; they are just scattered across `COND=` parameters, submission order and a control table that is not in the repository. |
| **Object storage with lifecycle policy** | 20 GDG bases at `LIMIT(35) SCRATCH` | Same retention semantics, without silent rolloff destroying the `(-3)` a job expected. |
| **Managed streaming / queue** | The `CABCTL07`/`CABCTL08` MQ pair, and the CMDS exchange | **And close the NACK gap** — today `CABCTL07` sets `MQMD-REPLYTOQ` to a queue nothing ever reads. |
| **Serverless functions for the sort-exit rules** | E15/E35 exits, re-specified as explicit pipeline stages | **This is the critical-path work.** See below. |
| **Managed relational database with real transactions** | The six "two stores, one update" programs | The gap is not a technology problem; it is that nobody ever wrote a resync utility. `CABSRSYN` is referenced in source and does not exist. |
| **Data contract registry with schema versioning** | The nine cross-application file reads | **The thing that does not exist today and must be built before either application can move.** |
| **Distributed cache** | The in-storage OCN table in `CABING02`, the flattened rate table in `CABRAT01`, the 1,200-row carrier-type table in `CABSE15B` | Three different programs already solved the same problem three different ways. |
| **Idempotent, keyed processing** | `CT-RESTART-KEY`, the positional audit trail, the `EXTEND` carry-forward file | **Removes the rerun trap entirely.** Today `CABRAT12` derives its retry count from physical record position. |
| **Policy-as-configuration** | The 45-state territory table in `CABSE15C`, the four hardcoded EDI OCNs in `CABSRT13`, the zero-suppression thresholds in `CABSE15D`, `RT-ROUND-RULE` | Every one of these is a business rule that currently requires a recompile or a control-card edit. |
| **Observability on the control chain** | `CABRPT01`'s 63-edge proof | The best asset in the estate. Make it a live dashboard instead of a printed verdict line. |

### The sequencing this estate actually forces

1. **Extract the sort-exit and control-card rules first.** Twenty-seven modules and eighteen
   control cards hold filter, transform, grouping and rounding rules that exist nowhere else.
   `SORTEXIT/_README.md` §5 is unambiguous that they *"must be **re-specified**, not translated"* —
   the territory list, the string limit, the zero thresholds, the 2012 grouping change are all
   constants in source that were never written down as requirements. **Nothing else can start
   until this is done, because every pipeline downstream is defined by rules nobody has written
   out.**
2. **Define the nine cross-application data contracts.** From source. With owners. Before either
   application moves.
3. **Settle the four CTC questions.** `CTC/_MANIFEST.md` §5: *"reconcile the twelve copies into a
   single specification with the differences explicit (mechanical, and tooling helps a lot); take
   the four business decisions (human, and no tooling helps at all); then build one program. **The
   middle step is the one that determines the timeline, and it is the one that is invisible in a
   static analysis report.**"*
4. **Decide the rounding convention, explicitly, per money field.** Twenty-odd programs disagree
   with the program downstream of them, `RT-ROUND-RULE` varies per rate record, and `CABPKDEC`
   rounds half-away-from-zero at five places while its callers round half-up at two. **Every one of
   those disagreements changes an invoice.**
5. **Then move Usage Ingestion**, which is the only family with no cross-application read, no
   shared state and clean per-record restart semantics.

### The decisions that are not technical, and must not be deferred

`CTC/_MANIFEST.md` puts it best: *"Converting twelve copies faithfully preserves twelve behaviours.
Converting one copy and pointing all twelve centres at it silently changes the invoice at up to
eleven of them. **Both of those are decisions. Neither of them is a technical decision.**"*

The same is true of the seeded defects. `HARNESS/_README.md` §4: `DIVERGENT-BY-DESIGN` **is not a
pass** — it means the candidate has surfaced a defect the legacy has been carrying, and the correct
response is *"a business decision about the defect — revenue assurance, regulatory, restatement —
not a code change to silence the harness. Several of the seeded defects cannot be 'fixed' during a
migration without changing what carriers are billed, which is a rate change in substance."*

---

## Where to go next

| You want to | Read |
|---|---|
| Understand the build rules before reading any source | **`CONVENTIONS.md`** |
| Stand up Hercules and run the estate | **`DOCS/HERCULES_RUNBOOK.md`** |
| Load it into CAST Imaging and score the result | **`DOCS/CAST_IMAGING_GUIDE.md`** |
| Understand the single biggest blind spot | **`SORTEXIT/_README.md`** |
| Understand why twelve copies of one program is the hardest problem here | **`CTC/_MANIFEST.md`** |
| Generate input data | **`GENERATORS/_README.md`** |
| Compare a candidate against the legacy | **`HARNESS/_README.md`** |
| See what is deliberately missing from the IMS layer, and why | **`IMS/_README.md`** |
| Understand the library-search-order and rounding divergences | **`HLASM/_MANIFEST.md`** |
| Find where a specific complexity is placed | **`CONTRACTS/complexity_placement.json`** + each family's `_MANIFEST.md` |
| Score a tool against the placements | **`DOCS/Complexity_Traceability_Matrix.xlsx`**, **`DOCS/Process_Contract_Register.xlsx`** |
| See the seeded defects | **`SEALED/`** — *withhold for a blind run* |

---

*CABS Tier 5 · wholesale carrier access billing reference estate · OS/VS COBOL 1974 on Hercules
TK4- / MVS 3.8j, with an Enterprise COBOL, IMS, DB2, CICS, MQ, PL/I, HLASM and REXX reference layer
authored to production standard for static analysis.*
