# CABS Tier 5 — CAST Imaging Loading and Scoring Guide

**What this document is.** How to load this estate into CAST Imaging, what CAST should find, what
CAST will not find, and how to score the difference.

**Why the third section is the important one.** This estate was built so that a static analyser has
something real to find *and* something real to miss. The findings CAST surfaces are the easy half
of a modernization assessment. The half it cannot surface is where the schedule risk lives. Section
3 is the specific, itemised list — not a general caution about the limits of static analysis, but
the named paragraphs, control cards, load libraries and copybook fields in *this* estate that a
scan will not reach.

**Reference documents.** `CONVENTIONS.md` (build rules), `SORTEXIT/_README.md` (the sort-exit blind
spot — the most consequential one), `CTC/_MANIFEST.md` (multi-site divergence),
`IMS/_README.md` (the deliberately missing DBDs), `HLASM/_MANIFEST.md` (library search order and
the rounding divergence), `ONLINE/_MANIFEST.md` (COMMAREA state rules).

**A note on version specificity.** CAST Imaging's UI labels, menu paths and the exact name of the
dynamic-call inference setting change between releases. Where this guide names a setting, treat the
*behaviour* as the requirement and confirm the current label in the CAST documentation for your
release. **[unverified — I have not confirmed labels against a specific CAST Imaging version.]**

---

## 1. Loading instructions

### 1.1 Create two applications, not one

**Create `CABS-ACCESS-BILLING` and `CABS-SETTLEMENT` as separate CAST applications.**

This is not a cosmetic choice. `CONVENTIONS.md` defines two application codes — `CABS` (access
billing) and `SETL` (settlement) — and the dataset naming carries the boundary: `TELCABS.CABS.*`
versus `TELCABS.SETL.*`. If you load everything as one application, **every cross-application read
becomes an internal read and complexity 16 vanishes from the report.** The single most important
structural finding in this estate would disappear into the noise.

Loaded as two, CAST's inter-application analysis will surface the coupling as cross-application
links, which is exactly what you want on the screen when the modernization sequencing conversation
happens.

### 1.2 Folder-to-application mapping

| Folder | Application | Notes |
|---|---|---|
| `BATCH/INGEST/` | **CABS-ACCESS-BILLING** | 12 programs |
| `BATCH/RATING/` | **CABS-ACCESS-BILLING** | 14 programs |
| `BATCH/JURIS/` | **CABS-ACCESS-BILLING** | 11 programs. `CABJUR10` is DB2-precompiled. |
| `BATCH/BILLCALC/` | **CABS-ACCESS-BILLING** | 12 programs |
| `BATCH/FORMAT/` | **CABS-ACCESS-BILLING** | 9 programs |
| `BATCH/REPORT/` | **CABS-ACCESS-BILLING** | 8 programs |
| `BATCH/UTIL/` | **CABS-ACCESS-BILLING** | 95 generated utility programs |
| `BATCH/SETTLE/` | **CABS-SETTLEMENT** | 13 programs. `CABSET12`, `CABSET13` DB2-precompiled. |
| `BATCH/CONTROL/` | **split** | `CABCTL01`–`CABCTL04` → CABS. `CABCTL05`–`CABCTL08` → SETL. See note below. |
| `CTC/SITE01` … `SITE12` | **CABS-ACCESS-BILLING** | See §1.4 — load all twelve. |
| `SORTEXIT/` | **CABS-ACCESS-BILLING** | Load it, knowing CAST will find no inbound edge. That absence is the finding. |
| `HLASM/` + `HLASM/EMERG/` | **both** — shared | `TELCABS.COMMON.LOADLIB` is common to both applications. |
| `PLI/` | **CABS-ACCESS-BILLING** | |
| `ONLINE/` + `ONLINE/BMS/` | **CABS-ACCESS-BILLING** | CICS suite |
| `IMS/` | **split** | `CABCARDB`, `CABCIRDB`, `CABCIRIX`, `CABBHSDB` → CABS. `CABSETDB`, `CABSETSX` → SETL. |
| `DB2/` | **split** | `CABSADJ`, `CABSRTHS`, `CABSFCHS`, `CABSINVR`, `CABSAUDT` → CABS. `SETLTRAN`, `SETLPERIOD` → SETL. |
| `REXX/` | **CABS-ACCESS-BILLING** | |
| `VSAM/` | **split by DSN prefix** | `TELCABS.CABS.*` → CABS. `TELCABS.SETL.MASTER` (`CABVS100`) → SETL. |
| `COPYBOOKS/` | **shared library, both applications** | Do not duplicate — see §1.5. |
| `JCL/` | **split by job name** | `CABING*`, `CABIN*`, `CABRAT*`, `CABRT*`, `CABJ1*`, `CABJ2*`, `CABS4*`, `CABS5*`, `CABS6*`, `CABS7*`, `CABU7*`, `CABCTLRP`, `CABGDGDF` → CABS. `CABS2*`, `CABS3*` → SETL. |
| `JCL/PROCS/` | **shared** | `CABPDB2P` is used by `CABJ1900` (CABS), `CABS3100` and `CABS3200` (SETL) — a genuinely shared PROC. |
| `JCL/CTLCARDS/` | **shared** | See §1.3. |
| `GENERATORS/`, `HARNESS/`, `BUILDER/`, `CONTRACTS/`, `SEALED/`, `DOCS/`, `DATA/` | **exclude** | Tooling and answer key, not estate. |

> **`BATCH/CONTROL/` split rationale.** `CABCTL01`–`CABCTL04` maintain the carrier profile, factors,
> circuit inventory and bill history — CABS-owned data. `CABCTL05`–`CABCTL08` do settlement posting,
> the dispute enquiry and the MQ gateway pair — SETL-owned. If you would rather not split a folder,
> put all eight in CABS-ACCESS-BILLING and accept that `CABCTL05`'s write to
> `TELCABS.SETL.MASTER` will then read as an outbound cross-application write, which is also a true
> statement about the estate.

> **`SEALED/` must not be loaded and must not be in scope.** It is the answer key. Loading it does
> not help CAST and it destroys the blind-run property.

### 1.3 The `.ctl` extension problem — solve this first

The 35 members in `JCL/CTLCARDS/` and `JCL/CTLCARDS/MVT/` carry a `.ctl` extension. **CAST will classify them as
unclassified text unless you tell it otherwise, and they contain business rules.**

`SORTEXIT/_README.md` §2 is explicit about why this matters:

> *"The control card is **data**, not JCL and not code. Most parsers treat instream SYSIN as an
> opaque blob and PDS members with unknown suffixes as unclassified text. Even a parser that reads
> the card has to know Sort control-card grammar to recognise `MODS=` as a module reference."*

Do at least the first of these:

1. **Bring them into scope as content.** Even if CAST does not parse sort grammar, having them
   indexed means a text search for `MODS=` finds them and they appear in the object inventory
   rather than being invisible.
2. **Record the twenty-two `MODS` module references manually as an addendum to the CAST
   findings.** Full list in §3.1 — five in `JCL/CTLCARDS/` and seventeen in `JCL/CTLCARDS/MVT/`. There is no configuration that makes CAST derive these; they have to be added by hand
   if you want them on the dependency graph.

The same problem applies to the `//SYSIN DD *` instream cards in the JCL. `CABING01.jcl` STEP020
carries a `PARMIN DD *` instream parm card; `CABS7000.jcl` carries two positional SYSIN control
records that drive `CABRPT01` and `CABRPT08`.

### 1.4 The `CTC/` multi-site family — load all twelve copies

Twelve directories each contain a `CABCTC01.cbl`; nine also contain a `CABCTC02.cbl`. That is 21
COBOL members across 12 sites, 21,107 lines.

**Load all twelve as separate source paths.** Do not deduplicate, do not rename, do not pick one.

If your CAST configuration objects to duplicate program names within one application, model each
site as its own source location within `CABS-ACCESS-BILLING` — one path per site, mirroring the
real deployment where each centre promotes from its own source library into
`TELCABS.<centre>.CABS.LOADLIB`.

This family is the single richest CAST demonstration in the estate: **clone detection will
correctly find the near-duplicate group, correctly identify the paragraphs that differ, and be
unable to tell you which behaviour the business is supposed to have.** That gap — mechanical
detection succeeding, semantic decision remaining human — is the point.

### 1.5 Copybook and PROC library paths

**Copybooks — one shared library, referenced by both applications.**

```
COPYBOOKS/          →  COBOL copybook path (18 members)
DB2/DCL*.cpy        →  COBOL copybook path (5 DCLGEN members)
ONLINE/BMS/         →  symbolic map source (see note)
```

Two configuration facts that will otherwise cost you resolution:

1. **`CABSWRK` nests `CABSERR`, `CABSDATE` and `CABSCTL`.** `CONVENTIONS.md` requires every program
   to `COPY CABSWRK`. That is complexity 08 — nested copybooks — and it means the copybook path
   must resolve four members deep from a single `COPY` statement. All four are in `COPYBOOKS/`, so
   one path covers it, but confirm CAST is following the nesting and not stopping at the first
   level. **If `CABSCTL` does not appear as a dependency of all 173 runnable programs, the nesting
   is not resolving.** That is your check.

2. **The BMS symbolic maps do not exist as copybook members.** `ONLINE/CABONL01.cbl` through
   `CABONL06.cbl` each `COPY CABM0100` … `CABM0600`. Those members are *generated* by the
   `TYPE=DSECT` pass of `ONLINE/BMS/CABMAPS.jcl`, which **has never been submitted**
   (`ONLINE/BMS/_MANIFEST.md`). The `.bms` macro source is present; the generated copybooks are
   not. **CAST will report six unresolved `COPY` statements in the CICS suite.** That is correct
   and expected — record it, do not fabricate the members.

**PROCs.**

```
JCL/PROCS/          →  JCL PROCLIB path (27 members)
```

Note the extension inconsistency, which is real and in the source: 18 members are `.prc` and 9 are
`.jcl` (`CABPCMDS`, `CABPDB2P`, `CABPFCTL`, `CABPJURS`, `CABPMPBX`, `CABPNETT`, `CABPRECP`,
`CABPREST`, `CABPSETL`). Configure the PROCLIB path to accept both extensions or nine PROCs will
not resolve and roughly a third of the `EXEC procname` statements in the estate will dangle.

### 1.6 Technology roots

| Technology | Root path(s) | Extension(s) | Notes |
|---|---|---|---|
| **COBOL — OS/VS 1974** | `BATCH/INGEST`, `BATCH/RATING`, `BATCH/JURIS`, `BATCH/SETTLE`, `BATCH/BILLCALC`, `BATCH/FORMAT`, `BATCH/REPORT`, `BATCH/UTIL`, `CTC/SITE01..12` | `.cbl` | Fixed format, cols 1-6 sequence, 7 indicator, 8-11 Area A, 12-72 Area B |
| **COBOL — Enterprise** | `BATCH/CONTROL`, `ONLINE`, `SORTEXIT` | `.cbl` | Scope terminators, `EVALUATE`. **Two vintages in one estate is deliberate** (`CONVENTIONS.md`). |
| **COBOL — DB2 precompiled** | `BATCH/JURIS/CABJUR10.cbl`, `BATCH/SETTLE/CABSET12.cbl`, `BATCH/SETTLE/CABSET13.cbl` | `.cbl` | `EXEC SQL` blocks |
| **COBOL — CICS** | `ONLINE/` | `.cbl` | `EXEC CICS` blocks |
| **COBOL — IMS DL/I** | `BATCH/CONTROL/CABCTL01..06.cbl` | `.cbl` | `CALL 'CBLTDLI'` |
| **JCL** | `JCL/` | `.jcl` | 131 job members |
| **JCL — PROC** | `JCL/PROCS/` | `.prc` **and** `.jcl` | 27 members |
| **JCL — VSAM/IDCAMS** | `VSAM/` | `.jcl` | 20 IDCAMS members + manifest |
| **JCL — IMS gen** | `IMS/CABIMSGN.jcl` | `.jcl` | DBDGEN → PSBGEN → ACBGEN |
| **JCL — BMS assembly** | `ONLINE/BMS/CABMAPS.jcl` | `.jcl` | Two-pass, never submitted |
| **PL/I** | `PLI/` | `.pli` | OS PL/I Optimizing Compiler V2 era |
| **HLASM / Assembler** | `HLASM/`, `HLASM/EMERG/` | `.asm` | 5 members, 4 distinct modules |
| **REXX** | `REXX/` | `.exec` | 6 execs |
| **IMS DBD** | `IMS/` | `.dbd` | 6 members |
| **IMS PSB** | `IMS/` | `.psb` | 6 members |
| **BMS mapset** | `ONLINE/BMS/` | `.bms` | 6 mapsets |
| **DB2 DDL** | `DB2/` | `.ddl`, `.sql` | 3 members |
| **Copybook** | `COPYBOOKS/`, `DB2/` | `.cpy` | 23 members |
| **Sort control card** | `JCL/CTLCARDS/` | `.ctl` | **18 members — see §1.3** |

### 1.7 Analysis configuration

**Dynamic calls — the Inference Engine.**

This estate places complexity 02 (dynamic call) in nine programs with five distinct target-name
construction patterns. **Enable CAST's Inference Engine for dynamic COBOL calls and set it to
report unresolved dynamic call targets rather than dropping them.** The behaviour you want:

- Every `CALL <identifier>` is recorded as a call site even when the target cannot be resolved.
- The identifier's data flow is traced backwards where possible, so `R1-CALL-TARGET` traces back to
  `R1-CALL-PREFIX` + `R1-CALL-SUFFIX`.
- The result is a **named unresolved edge**, not a silent omission.

A dynamic call that CAST silently drops is worse than one it flags as unresolved, because the call
graph then looks complete when it is not.

**What the Inference Engine will and will not get here.** It will recover the *prefix* — `'CABRAT'`,
`'CABJX'`, `'CABRT'`, `'CABMP'`, `'CABRA'` — because those are literals moved into the name field.
It will **not** recover the suffix, because the suffix comes from `R2-EN-MODULE-SFX`, a field in a
**rate table record read from a VSAM cluster at run time**. `CONTRACTS/complexity_placement.json`
states it flatly: *"suffix_source: `R2-EN-MODULE-SFX` from the loaded rate table entry — **not
derivable statically**."* No inference engine can resolve a name that is data.

**Other settings.**

| Setting | Value | Why |
|---|---|---|
| **Clone / duplication detection** | **On** | You want it to find `CTC/`. See §2.5 for what it should and should not group. |
| **Dead code detection** | **On** | Nine placed instances (§2.4). |
| **`GO TO` in control flow** | **Must be modelled** | Six hidden error handlers are reached only by `GO TO` from 3–5 sites each. A tool that models `PERFORM` but not `GO TO` reports them as unreachable, and they are neither unreachable nor optional. |
| **`PERFORM … THRU` ranges** | **Must be modelled as ranges** | Eleven fall-through placements depend on `PERFORM A THRU B-EXIT` spanning paragraphs. Modelling `PERFORM` as single-paragraph invocation loses all of them. |
| **Copybook expansion depth** | ≥ 4 | `CABSWRK` → `CABSERR`/`CABSDATE`/`CABSCTL`, and `CABSRT01` → `02` → `03` → `04` in `CABRAT01`. |
| **JCL symbolic resolution** | Best effort, and **record what did not resolve** | See §3.3. Nothing resolves them; the values live outside the estate. |
| **Character encoding** | UTF-8 / ASCII for the repository as delivered | The source in this repository is ASCII. If you scan a PDS extract instead, it is EBCDIC cp037. |
| **Line length** | 80, with COBOL columns 73–80 ignored | `BUILDER/_README.md` records that zero lines exceed column 72. |

---

## 2. Expected findings — ground truth

Everything in this section is derived from the actual source and manifests. Use it as the answer
sheet for what CAST *should* surface. Where a count is exact it is stated exactly; where it is a
family it is described as one.

### 2.1 Call graph entry points

**JCL-driven entry points: 131 job members**, invoking **173 runnable COBOL programs** plus 8
reference-only Enterprise COBOL programs plus 6 CICS programs plus 4 PL/I programs.

The two main stream entry points CAST should identify as the roots of the deepest chains:

| Entry | Steps | Depth |
|---|---:|---|
| `JCL/CABING01.jcl` — daily ingest | 14 | 12 COBOL programs + 2 SORT steps + `CABPCTLR` PROC |
| `JCL/CABRAT01.jcl` — rating | 14 | 13 COBOL programs + 1 SORT step, 12 via the `CABPRATE` PROC |
| `JCL/CABS7000.jcl` — month-end close | 7 | Submits 21 jobs through the internal reader, runs 2 programs in line |

**CICS entry points: 6 transactions** — `CAB0` (`CABONL01`, the menu/dispatcher) `XCTL`s to
`CAB1`–`CAB5`. `CABONL05` `LINK`s to `CABONL06`. `CABONL06` `XCTL`s to `CAB4`.

**IMS entry points: 6 PSBs** — `CABCT01P` … `CABCT06P`, one per DL/I program.

**Programs with no inbound edge — CAST should report these as orphans:**

| Program | Status |
|---|---|
| `BATCH/INGEST/CABING12.cbl` | **Orphaned. Referenced by no JCL member in the estate.** 421 lines, EMI 42-XX legacy bridge. Recorded in `BATCH/INGEST/_MANIFEST.md` as complexity 26. |
| `SORTEXIT/CABSE15A` … `CABSE35D` (8 modules) | **No inbound edge from any COBOL or JCL.** Reached only via `MODS=` in a control card. See §3.1 — this is the deliberate blind spot. |
| `HLASM/CABPKDEC.asm` | **No inbound edge.** Called dynamically with the name built in working storage. |
| `HLASM/CABBITST.asm` | **No inbound edge — and the manifest is wrong about it.** `HLASM/_MANIFEST.md` claims static calls from `CABING01`, `CABING05`, `CABING09`, `CABING11`; the literal `CABBITST` appears in **no `.cbl` file in the estate**. CAST is right and the manifest is wrong. Record it. |
| `PLI/CABRTMNT.pli`, `CABSTREC.pli`, `CABFCVAL.pli` | No caller in the estate — they are batch-driven from JCL that is not present. |
| `VSAM/CABVDEF1..6`, `CABVS020/040/090/110/120` | Define 11 datasets with **no inbound edge from any program or JCL**. Complexity 15. |

**A COBOL→PL/I edge CAST will draw that is misleading, and it is meant to be:**
`BATCH/CONTROL/CABCTL04.cbl` `P7700-BRIDGE-OLD-FORMAT` calls `PLI/CABLGCNV.pli`.
CAST will draw the edge and report `CABLGCNV` as live. **It is not.** `P7700` is performed only
from `P2100-READ-HEADER`, guarded by `IF WS-BRIDGE-REQUIRED`, where `WS-BRIDGE-SW` is declared
`VALUE 'N'` and **is never moved to anywhere in the program**. The parm card carries a
`WP-BRIDGE-SW` field that looks as though it would set it; nothing transfers one to the other.
`BATCH/CONTROL/_MANIFEST.md` documents this in full.

### 2.2 Dynamic call targets CAST will show as unresolved

**This table is the ground truth for §3's scoring.** None of these targets exists as source in this
estate.

| Caller | Construction | Targets | Resolvable statically? |
|---|---|---|---|
| `CABRAT02` `P4100` (×4 sites) | `'CABRAT'` + `R2-EN-MODULE-SFX` from the loaded rate table | `CABRATOA`, `CABRATTA`, `CABRATLT`, `CABRATTS`, `CABRATCC`, `CABRATSP`, `CABRATUN`, `CABRATOS` | **No — suffix is table data** |
| `CABRAT03` `P7200` | same pattern | same set | **No** |
| `CABRAT08` `P3400-INVOKE-OVERRIDE-PGM` | override-record driven | varies | **No** |
| `CABRAT10` `P4500-CALL-FORMATTER` | `WS-FMT-MODULE-NAME` | varies | **No** |
| `CABRAT13` `P3300-CALL-OPR-MODULE` | operator-services module | `CABRATOS` | **No — and unreachable anyway** (dormant behind `R1-OPR-SVC-SW = 'N'`) |
| `CABJUR03` | `'CABJX'` + state suffix built at run time | `CABJXAL`, `CABJXCA`, … one per state | **No.** Note `CABJXCAL` appears in **two of the five STEPLIB libraries** on `CABJ1200` — a library-search-order interaction on top of a dynamic call. |
| `CABJUR04` | `'CABRT'` + jurisdiction suffix | `CABRTIS`, `CABRTIN`, `CABRTLO` | **No** |
| `CABSET01` | `'CABMP'` + region suffix (regional residual rules, 2013) | `CABMPSE`, `CABMPSW`, … | **No** |
| `CABSET07` | `'CABRA'` + region suffix — five regional formatters never harmonised | `CABRASE`, `CABRASW`, … | **No** |
| `HLASM/CABPKDEC` | name built in working storage in RATING and SETTLE | `CABPKDEC` | **No — the binder never resolves it either** |

**Dynamic call identifiers in the source**, for cross-checking CAST's inventory:
`WS-DISPATCH-TARGET` (4 sites), `WS-CALL-PGM` (4), `WS-OS-CALL-TARGET` (1),
`WS-FMT-MODULE-NAME` (1), `WS-DC-TARGET` (1).

**Static `CALL 'literal'` targets** — the twelve in `BATCH/COMMON/` account for 519 of the call
sites and **appear in no JCL at all**, so a call graph built from `EXEC PGM=` will not contain any
of them even though they are the most-called modules in the estate. Only `CBLTDLI` and the MQ
verbs are genuinely unresolved:

| Target | Call sites | Source? |
|---|---:|---|
| `CABPARMR` | 141 | ✅ `BATCH/COMMON/CABPARMR.cbl` |
| `CABHASH` | 120 | ✅ `BATCH/COMMON/CABHASH.cbl` |
| `CABERRWR` | 69 | ✅ `BATCH/COMMON/CABERRWR.cbl` |
| `CABEDITF` | 11 | ✅ `BATCH/COMMON/CABEDITF.cbl` |
| `CABSEQCK` | 10 | ✅ `BATCH/COMMON/CABSEQCK.cbl` |
| `CABOCNVL` | 10 | ✅ `BATCH/COMMON/CABOCNVL.cbl` |
| `CABCTLWR` | 9 | ✅ `BATCH/COMMON/CABCTLWR.cbl` |
| `CABFMTR` | 6 | ✅ `BATCH/COMMON/CABFMTR.cbl` |
| `CABTBLLU` | 3 | ✅ `BATCH/COMMON/CABTBLLU.cbl` |
| `CABRTFMT` | 3 | ✅ `BATCH/COMMON/CABRTFMT.cbl` |
| `CABCIRCL` | 2 | ✅ `BATCH/COMMON/CABCIRCL.cbl` |
| `CBLTDLI` | 36 | ❌ (IMS interface module, resolved at link-edit) |
| `MQCONN`/`MQOPEN`/`MQPUT`/`MQGET`/`MQCMIT`/`MQCLOSE`/`MQDISC` | 15 | ❌ (MQ stubs) |
| `CABABEND` | 200 | ✅ `HLASM/CABABEND.asm` |
| `CABDTCNV` | 136 | ✅ `BATCH/COMMON/CABDTCNV.cbl` **and** an alias entry of `HLASM/CABDATCV.asm` — two candidates for one literal, see §3.6 |
| `CABDATCV` | 68 | ✅ — **but see §3.6: there are two of them** |
| `CABSE15A` | 1 | ✅ `SORTEXIT/CABSE15A.cbl` — the one exit that is also named in a COBOL `CALL` |
| `CABLGCNV` | 1 | ✅ `PLI/CABLGCNV.pli` — but the call site never executes |

### 2.3 Cross-application dataset coupling — CABS ↔ SETL

**This is the finding the two-application load exists to produce.** Nine confirmed instances,
every one a direct dataset read across the application boundary with no interface, no contract and
no owner.

| # | Program | App | Reads / writes | Owned by | Recorded in |
|---|---|---|---|---|---|
| 1 | `CABJUR09` | CABS | `TELCABS.SETL.SETTLE.MASTER` | SETL | `BATCH/JURIS/_MANIFEST.md` |
| 2 | `CABBIL06` | CABS | `TELCABS.SETL.NET` | SETL | `BATCH/BILLCALC/_MANIFEST.md` |
| 3 | `CABRPT05` | CABS | `TELCABS.SETL.SETTLE.ALL` | SETL | `BATCH/REPORT/_MANIFEST.md` |
| 4 | `CABSET01` | SETL | `TELCABS.CABS.BILLDTL` | CABS | `BATCH/SETTLE/_MANIFEST.md` |
| 5 | `CABSET02` | SETL | `TELCABS.CABS.CDR.PLU` | CABS | `BATCH/SETTLE/_MANIFEST.md` |
| 6 | `CABSET05` | SETL | `TELCABS.CABS.CDR.RECIP` | CABS | `BATCH/SETTLE/_MANIFEST.md` |
| 7 | `CABSET09` | SETL | `TELCABS.CABS.CARRIER` | CABS | `BATCH/SETTLE/_MANIFEST.md` |
| 8 | `CABSET11` | SETL | `TELCABS.CABS.BILLHDR` | CABS | `BATCH/SETTLE/_MANIFEST.md` |
| 9 | `CABCTL05` | SETL | writes `TELCABS.SETL.MASTER` from a CABS-side IMS update | both | `BATCH/CONTROL/_MANIFEST.md` |
| 10 | `CABCTC01` | CABS/CTC | `TELCABS.CABS.CARRIER` (owned by ingest) | CABS | `CTC/_MANIFEST.md` §6 |

**The corresponding JCL DDs**, which is where CAST will actually see it:
`CABS2100` (`CDR.PLU`), `CABS2200` (`BILLDTL`), `CABS2500` (`CDR.RECIP`), `CABS2800` (`CARRIER`),
`CABS3000` (`BILLHDR`), `CABJ1800` (`SETTLE.MASTER`), `CABS4300` (`SETL.NET` at `(-1)`),
`CABS6300` (`SETTLE.ALL` at `(-1)`).

**Why this is the key modernization blocker, in one sentence:** there is no interface to preserve
— the coupling is a filename, and any target-state design that moves either application to a new
data store must simultaneously replace six-to-nine direct file reads with something that does not
exist yet.

**One more coupling CAST will not see at all**, listed here because it belongs in the same
conversation: `CABCTL07` `MQPUT`s to `CABS.SETTLE.OUT` and sets `MQMD-REPLYTOQ` to
`CABS.SETTLE.NACK`, while `CABCTL08` `MQGET`s from `CABS.SETTLE.ACK`. **Nothing in the estate ever
reads the NACK queue.** The record's fate leaves the estate at the `MQPUT` — no file, no control
record, no return path.

### 2.4 Dead code candidates

Nine placed instances of complexity 26, plus five of complexity 27 (dormant feature). CAST's dead
code detector should find most of the first group and **none of the second**, because a dormant
feature is reachable code behind a switch that happens never to be set.

**Complexity 26 — dead code (unreachable). CAST should find these:**

| Program | Paragraph(s) | Story |
|---|---|---|
| `CABING09` | `P6600-NEGATIVE-MOU-ADJ` | Behind an impossible guard |
| `CABING12` | *whole program* | Referenced by no JCL member |
| `CABRAT05` | `P6100-UNE-LOOP-REPRICE` | Behind `WS-LOOP-REPRICE-SW`, never moved to |
| `CABJUR02` | `P5000-PSU-BAND-CHECK` | Compiled, never `PERFORM`ed since V2.01 in 1999 |
| `CABBIL04` | `P6200-DEFERRED-PAYMENT-PLAN`, `P6210-PLAN-SCHEDULE` | Written for the 1995 settlement programme |
| `CABFMT07` | `P5400-MICROFICHE-BLOCK` | Bureau contract ended 2001 |
| `CABRPT05` | `P6100-INTEREST-PROJECTION`, `P6110-ONE-INTEREST` | 1999 tariff review |
| `CABSET09` | `P5000-INTEREST-CALC` | Suspended April 1999 pending a tariff review that never concluded |
| `CABCTL04` | `P7700-BRIDGE-OLD-FORMAT` | The COBOL→PL/I bridge. **CAST will report `CABLGCNV.pli` as live because of this.** |
| `PLI/CABLGCNV.pli` | *whole program* | 293 lines, reachable from exactly one place, and that place never executes |
| `CABONL03` | `DUPKEY` handler | Handles a condition the current key structure can no longer raise |
| `CABONL01` | `WS-RPT-MENU-SW` | Leftover field from the withdrawn 1998–2003 option 6 |

**Complexity 27 — dormant feature (reachable, switch never set). CAST will report these as LIVE:**

| Program | Feature | Switch | Since |
|---|---|---|---|
| `CABING06` | MPB auto-split | `WS-MPB-AUTO-SW = 'N'` | — |
| `CABRAT13` | *Entire rating path* — ~170 lines | `R1-OPR-SVC-SW = 'N'` | Tariff withdrawn 1998 |
| `CABJUR06` | `P5000-STUDY-DERIVED` — performed on every record, returns immediately | traffic-study feed | Decommissioned June 2011 |
| `CABBIL06` | `P4800-CMDS-RESIDUAL-NET` — 1994 CMDS residual pool share | `WS-PE-CMDS-RESID-SW`, JCL substitutes `N` | Agreement lapsed |
| `CABFMT09` | *Entire program* — bill message insert | insert switch + campaign code; `CABS5500` STEP020 substitutes `N` and a blank | Last campaign 2009 |
| `CABSET06` | *Entire program* — wireless termination | `P1400-FEATURE-CHECK` | July 2011, moved to bill-and-keep. **`CABS2450` still runs every month and produces an empty file.** |
| `CABONL05` | `P6000-AUTO-CREDIT` delegated-authority path | — | — |

**This distinction is the most useful single output of the exercise.** Dead code can be deleted.
Dormant code cannot — it is a business decision about whether the capability should come back, and
in `CABSET06`'s case a monthly job is still burning an initiator to produce an empty file.

### 2.5 The CTC near-duplicate family

**What CAST should report, and it will be right:**

| Program | Copies | Byte-identical group | Differ in one literal | Differ in logic |
|---|---:|---|---|---|
| `CABCTC01` | 12 | **7** (SITE01, 02, 04, 06, 08, 10, 12) — SHA-256 prefix `b99e5c41f50b`, 930 lines, `V1.09` | **3** (SITE03, SITE05, SITE07) | **2** (SITE09 = 951 lines / 23 lines different; SITE11 = 914 lines / 26 lines different) |
| `CABCTC02` | 9 | **5** (SITE01, 02, 06, 08, 12) — SHA-256 prefix `74badc8ad7d7`, 1,105 lines, `V2.14` | **2** (SITE03, SITE05) | **2** (SITE09 = 1,087 / 20 lines; SITE11 = 1,120 / 17 lines) |

**Three sites have no `CABCTC02` at all** — STL (SITE04), PHX (SITE07), CLE (SITE10). Nobody
currently in the department can say whether those three never took it, took it and backed it out,
or run it from another centre's library.

**The specific divergences CAST should surface:**

| Site | Program | What differs |
|---|---|---|
| SITE03 | `CABCTC01` line 160 | `WS-SUMM-STATE-DFLT VALUE 'TX'` vs `'NC'` |
| SITE05 | `CABCTC01` line 162 | `WS-MOU-THRESHOLD VALUE 499999.99` vs `999999.99` |
| SITE07 | `CABCTC01` line 163 | `WS-TARIFF-EFF-YYDDD VALUE 98213` vs `96182` |
| SITE09 | `CABCTC01` | Extra paragraph `P2250-VALIDATE-TRUNK-GROUP` + its `PERFORM` at the bottom of `P2200-VALIDATE-USAGE`. Suspends any voice record with a blank trunk group or zero CIC. |
| SITE11 | `CABCTC01` | **Missing** `P2650-EXCLUDE-ISP-MINUTES` and its `PERFORM` inside `P2600-ACCUMULATE`. **And** `P3100-APPLY-ROUNDING` reads `MOVE WS-RW-RAW-MINUTES TO WS-RW-OUT-MINUTES` (truncates) where eleven copies read `COMPUTE … ROUNDED`. |
| SITE03 | `CABCTC02` line 262 | `WS-VARIANCE-TOLERANCE VALUE 0.25` vs `0.05` — five times the other centres' |
| SITE05 | `CABCTC02` line 263 | `WS-RETRO-CUTOFF-YYDDD VALUE 12001` vs `09001` |
| SITE09 | `CABCTC02` | **Missing** `P3750-COLLAR-EXPECTED`. Chicago does not apply the tariff minimum and maximum before comparing. Every collared element reconciles as a variance; the centre has worked that report by hand since 2018. |
| SITE11 | `CABCTC02` | Extra paragraph `P3800-CHECK-CROSS-CENTRE`, performed from `P3500-BUILD-RECON-REC`. |

**The trap CAST will fall into if you build an inventory from `WS-PGM-VERSION`:** ten of the twelve
`CABCTC01` copies report `V1.09`, **including all three that differ in a literal**. The version
literal was not moved when the value changed. A version-based inventory sees ten identical copies
and two outliers, which is wrong in both directions.

**Also load `CTC/_SITE_INDEX.md` as context but do not build an inventory from it.** It lists
**thirteen** centres. There are twelve. SITE13 (Kansas City) was folded into SITE04 (St Louis) in
September 2014 and CHG-2014-0918 was never closed. Anyone counting from the directory tree gets
twelve and does not learn that St Louis has been carrying Kansas City's traffic under its own site
code since 2014 — which is why `CS-SITE-CD` on a St Louis summary record is not a reliable
indicator of where the traffic originated.

### 2.6 Dataset dependency hot spots

CAST should rank these highest by fan-in/fan-out:

| Dataset | Read by | Written by | Note |
|---|---|---|---|
| `TELCABS.CABS.CONTROL` (GDG) | `CABRPT01`, `CABRPT08`, `CABRAT14`, `CABCTLRP`, `CABPCTLR` | **Every one of the 173 runnable programs** via `CTLOUT` | **The highest-fan-in dataset in the estate.** `CONVENTIONS.md` makes it mandatory. |
| `TELCABS.CABS.CARRIER` | `CABING02`, `CABING10`, `CABRAT11`, `CABRPT02`, `CABRPT05`, `CABSET09`, `CABCTC01`, `CABONL02` | `CABVS030` / `CABGDGDF` define it; `CABCTL01` reads via IMS | Cross-application read by `CABSET09` |
| `TELCABS.CABS.USAGE.SUSPENSE` (GDG) | `CABING07`, `CABING11`, `CABRPT03`, `CABJUR11` | 8 ingest programs write `SUSOUT` | Fan-in 8, fan-out 4 |
| `TELCABS.CABS.BILLDTL` (VB 1651) | `CABBIL03`, `CABBIL09`, `CABBIL11`, `CABFMT01`, `CABFMT03`, `CABFMT06`, `CABFMT07`, `CABRPT02`, `CABRPT06`, **`CABSET01`** | `CABRAT10`, `CABBIL02` | **The cross-application read** |
| `TELCABS.CABS.RATE` | `CABRAT01`–`CABRAT08`, `CABRAT14`, `CABUR*` | `CABVS050` defines; `PLI/CABRTMNT` maintains | Defined twice with conflicting `RECORDSIZE` |
| `TELCABS.CABS.AUDIT.LOG` | positional read-back | `CABING07` (`EXTEND`), `CABRAT12` (`EXTEND`) | **Physical order is the data.** No key. |
| `TELCABS.CABS.USAGE.CFWD` | `CABING08` (`CFWIN`) | `CABING08` (`CFWOUT`, `DISP=MOD`) | Self-referential across cycles |
| `TELCABS.COMMON.LOADLIB` | Every `STEPLIB` | — | And `TELCABS.SETL.LOADLIB.EMERG` shadows it — §3.6 |

**Twenty GDG bases** are defined in `JCL/CABGDGDF.jcl`, `LIMIT(35) SCRATCH NOEMPTY`. CAST should
show the generation-relative references (`(0)`, `(+1)`, `(-1)`, `(-3)`) as complexity 10.
Multi-generation concatenations to look for: `CABS7000` STEP040 (four generations), STEP060
(five), `CABS6200` (four), `CABS6400` (five), `CABJ1600` (reads `(-1)` **and** `(-3)`).

**Eleven datasets whose only declaration anywhere is an IDCAMS job**, with no inbound edge from any
program or JCL — complexity 15:

`TELCABS.CABS.CDR.CLEAN.ES`, `TELCABS.CABS.CARRIER.AIX`, `TELCABS.CABS.CARRIER.PATH`,
`TELCABS.CABS.BILLDTL.ES`, `TELCABS.CABS.SUSPENSE.ES`, `TELCABS.CABS.RUNCTL.ES`, plus the six
`CABVDEF` allocations (`TAXRATE`, `INVCTL`, `HOLDRSN`, `CLOSEMST` and two more).

**A dependency graph built from programs and JCL alone will not contain them at all. A graph built
from programs, JCL and `VSAM/` will contain them with no inbound edge.** That difference is worth
demonstrating on screen.

### 2.7 Copybook fan-out from `CABSWRK`

`CONVENTIONS.md`: *"Every program `COPY`s `CABSWRK` (which nests `CABSERR`, `CABSDATE`,
`CABSCTL`)."*

**Expected fan-out: `CABSWRK` → all 173 runnable programs + 21 CTC members + 8 reference-layer
programs. It is the single most-referenced artefact in the estate.**

The nesting chain, which is complexity 08:

```
CABSWRK ──┬── CABSERR    (error / suspense codes)
          ├── CABSDATE   (date work area, DW-PIVOT-YY = 70)
          └── CABSCTL    (CABS-CONTROL-RECORD, LRECL 180)
```

**Your resolution check:** `CABSCTL` must appear as a transitive dependency of every program that
`COPY`s `CABSWRK`, even though no program names `CABSCTL` in a `COPY` statement. If CAST shows
`CABSCTL` with a fan-in of zero or a handful, the nesting is not being expanded and every
control-record dependency in the estate is missing from your graph.

**A second, deeper nesting** in `CABRAT01`: `CABSRT01` → `CABSRT02` → `CABSRT03` → `CABSRT04`, four
levels, flattening the rate table into `R2-RATE-TABLE` + `R3-BAND-POOL`.

**Copybook usage across the utility tier**, from `BUILDER/_MANIFEST.md`: `CABSWRK` in all 95,
plus 0–2 archetype-appropriate copybooks, plus `CABSPRNT` where the program prints — 1 to 4 per
program.

**Two record layouts that are NOT in `COPYBOOKS/` and therefore have no fan-out at all:**

1. **`CABS-CTCS-RECORD`** — the CTC summary layout — is defined **inline in both CTC programs**.
   Twelve copies of a record layout that no copybook governs.
2. **`CABCTC02` re-declares `CABS-RTBL-RECORD` and its two `REDEFINES`** from
   `BATCH/RATING/CABRAT01.cbl`. **A change to the flattened extract has to be made in `CABRAT01`
   and in nine copies of `CABCTC02`.** CAST will not link them, because there is no `COPY`.

Also: the four PL/I programs each **declare their own copy of the `CABSCTL` layout field by field**
rather than through a shared `%INCLUDE`, because no PL/I copy library for `CABSCTL` exists in this
build. Five independent definitions of one record.

### 2.8 Summary — what a good scan should report

| Finding class | Expected count |
|---|---:|
| Programs analysed | ~217 COBOL + 4 PL/I + 5 HLASM + 6 REXX |
| JCL jobs | 131 |
| PROCs | 27 |
| Copybooks | 23 |
| Unresolved dynamic call sites | ≥ 11 identifiers across 9 programs |
| Unresolved static `CALL` targets | 11 distinct COBOL/assembler names + `CBLTDLI` + 7 MQ stubs |
| Cross-application dataset links | 9–10 |
| Dead code paragraphs/programs | 12 (see §2.4) |
| Near-duplicate program group | 1 family, 21 members, 12 sites |
| Orphan datasets (no inbound edge) | 11 |
| Orphan programs (no inbound edge) | ≥ 12 |
| Unresolved `COPY` statements | 6 (the BMS symbolic maps) |
| GDG bases | 20 |
| Missing IMS DBD/PSB | 2 databases (`CABRATDB`, `CABBANDB`) |

---

## 3. What CAST will miss — and this is the point

Everything above is what a good static analyser gets right. This section is what it cannot get, in
this estate, specifically. **It is not a critique of CAST.** Most of these are not detectable by any
static tool, because the information is not in the artefacts being scanned.

### 3.1 Sort E15/E35 exit logic — the largest blind spot

**Twenty-seven programs, 8,159 lines, carrying production business rules that appear in no call
graph** — the eight `CABSE*` modules (2,253 lines) named by the DFSORT cards in `JCL/CTLCARDS/`,
and the nineteen `CABSX*` modules (5,906 lines) named by the MVT-era cards in
`JCL/CTLCARDS/MVT/`.

`SORTEXIT/_README.md` opens by calling this folder *"the single most consequential blind spot in
the estate"*, and states:

> *"Not one of them appears in any COBOL call graph, any `EXEC PGM=` statement, or any `CALL`
> literal anywhere in the estate. They are reached only through the `MODS=` operand of a Sort
> control card. A reverse-engineering exercise that follows programs and JCL EXEC statements will
> not find them, and a reader who reads only the COBOL will form a materially wrong picture of what
> the batch stream does."*

**The complete list of `MODS=` references in the estate — add these to the dependency graph by
hand:**

Twenty-two distinct exit modules are named across the thirty-five control-card members, and
**every one of them resolves to source in `SORTEXIT/`**.

| Control card | `MODS` operand | Exit(s) |
|---|---|---|
| `CABSRT04.ctl` | `MODS=(E15=(CABSE15A,4096),E35=(CABSE35A,4096))` | `CABSE15A`, `CABSE35A` |
| `CABSRT07.ctl` | `MODS=(E35=(CABSE35B,4096))` | `CABSE35B` |
| `CABSRT11.ctl` | `MODS E15=(CABSXJUR,2048,SORTEXIT,N)` | `CABSXJUR` |
| `CABSRT12.ctl` | `MODS E15=(CABSXZIP,4096,…),E35=(CABSXBST,2048,…)` | `CABSXZIP`, `CABSXBST` |
| `CABSRT15.ctl` | `MODS E35=(CABSXLST,2048,SORTEXIT,N)` | `CABSXLST` |
| `MVT/CABSRT01.ctl` | `MODS E15=(CABSXSRC,4096,…)` | `CABSXSRC` |
| `MVT/CABSRT02.ctl` | `MODS E15=(CABSXEDT,4096,…)` | `CABSXEDT` |
| `MVT/CABSRT03.ctl` | `MODS E35=(CABSXMIN,8192,…)` | `CABSXMIN` |
| `MVT/CABSRT05.ctl` | `MODS E35=(CABSXRTY,4096,…)` | `CABSXRTY` |
| `MVT/CABSRT06.ctl` | `MODS E35=(CABSXCYC,4096,…)` | `CABSXCYC` |
| `MVT/CABSRT07.ctl` | `MODS E35=(CABSXSUM,8192,…)` | `CABSXSUM` |
| `MVT/CABSRT08.ctl` | `MODS E15=(CABSXBAL,4096,…)` | `CABSXBAL` |
| `MVT/CABSRT09.ctl` | `MODS E15=(CABSXACC,4096,…)` | `CABSXACC` |
| `MVT/CABSRT10.ctl` | `MODS E35=(CABSXDTL,8192,…)` | `CABSXDTL` |
| `MVT/CABSRT11.ctl` | `MODS E15=(CABSXJUR,2048,…)` | `CABSXJUR` |
| `MVT/CABSRT12.ctl` | `MODS E15=(CABSXZIP,4096,…),E35=(CABSXBST,2048,…)` | `CABSXZIP`, `CABSXBST` |
| `MVT/CABSRT13.ctl` | `MODS E15=(CABSXEDI,4096,…)` | `CABSXEDI` |
| `MVT/CABSRT14.ctl` | `MODS E35=(CABSXTAP,8192,…)` | `CABSXTAP` |
| `MVT/CABSRT15.ctl` | `MODS E35=(CABSXLST,2048,…)` | `CABSXLST` |
| `MVT/CABSRT16.ctl` | `MODS E35=(CABSXDUP,16384,…)` | `CABSXDUP` |
| `MVT/CABSRT17.ctl` | `MODS E15=(CABSXLDG,4096,…),E35=(CABSXLDR,4096,…)` | `CABSXLDG`, `CABSXLDR` |
| `MVT/CABSRT18.ctl` | `MODS E15=(CABSXOOB,4096,…)` | `CABSXOOB` |

The graph is still broken, just in the other direction: `CABSE15B`, `CABSE15C`, `CABSE15D`,
`CABSE35C` and `CABSE35D` **exist as source but are named in no control card in this repository**
— their control cards live in the site libraries. Five of twenty-seven modules are therefore
unreachable from any artefact a scanner can see, and the other twenty-two are reachable only from
a `.ctl` member that a scanner reads as data.

**What is decided in the eight `CABSE*` modules and nowhere else** (the nineteen `CABSX*` modules
are tabulated the same way in `SORTEXIT/_MANIFEST.md`):

| Module | The rule that lives only here |
|---|---|
| `CABSE15A` | Reformats the 200-byte CDR into the rating work layout. **The sort key positions on `CABSRT04` only line up after this exit has run.** Drops any record whose rate element code is still spaces — those records are not suspended, they are gone. |
| `CABSE15B` | Carrier-type selection for settlement. Loads a 1,200-row table from DD `CARRTYPE` and drops every record for a non-settlement-party, unknown or out-of-term carrier. **The carrier master holds the same indicator and the aggregation step never reads it.** This is the only settlement-eligibility test in the aggregation path — `CABSET04` does not repeat it. |
| `CABSE15C` | Drops local traffic before the access split; **rewrites an indeterminate jurisdiction to interstate in place**; drops intrastate traffic outside a hardcoded **45-state territory list maintained in source**. |
| `CABSE15D` | Zero-usage suppression at **0.01 minutes / $0.005** — not zero. Exempts setup, credit and make-up lines. The suppressed value is accumulated and reported to SYSOUT only. |
| `CABSE35A` | Stamps a 12-byte "rating control prefix" into bytes 189–200. **No copybook describes this area and no COBOL program references it.** It carries the sort work-dataset ordinal that `CABSE35B` depends on. |
| `CABSE35B` | Applies `RT-ROUND-RULE` to the **summed** amount after `SUM FIELDS` collapses the keys, accumulates the fractional-cent residue, releases it as one adjustment at end of merge. **A reviewer reading `CABRAT09` alone will conclude the summary is unrounded. It is not.** |
| `CABSE35C` | Full control-break summarisation — deletes every detail record, emits one total per OCN/BAN/period/jurisdiction. **The rate element was deliberately removed from the control group in 2012** — a grouping change made in a sort exit, with no corresponding change in any COBOL module. |
| `CABSE35D` | Cuts the industry-format header and trailer per receiving RAO, including the detail count and amount hash the counterparty balances against. **`CABSET07` never sees these records and cannot reproduce them.** |

**Why no configuration change fixes this.** `SORTEXIT/_README.md` §2 sets it out as four
independent breaks in the chain:

| The link that exists | Why a scanner does not follow it |
|---|---|
| `MODS=(E15=(CABSE15A,4096))` inside `//SYSIN DD *` or a `.ctl` member | The control card is data. Even a parser that reads it must know sort control-card grammar to recognise `MODS=` as a module reference. |
| The exit module in `TELCABS.COMMON.LOADLIB` | Resolved by the **loader at run time** from `STEPLIB`. No `EXEC PGM=`, no `CALL` literal. |
| The exit's effect on the data | Expressed as a **return code in `RETURN-CODE`**, interpreted by the sort. Nothing downstream reads a flag saying a record was dropped. |
| The exit's own accounting | Written with `DISPLAY` to `SYSOUT`. **Not** written to a `CABS-CONTROL-RECORD`, so the estate's own balancing framework never sees it. |

**And the estate's control framework makes it worse rather than better.** Every COBOL program must
satisfy `CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED + CT-CARRIED-FWD`. **A sort step writes
no control record at all.** So when `CABSE15D` suppresses 40,000 zero-value lines, or `CABSE15B`
removes every IXC's minutes from the settlement aggregation, **the run still balances** — the
program on the far side reads fewer records and reports the smaller number as its own `CT-READ`.
There is no reconciliation point at which the difference becomes visible.

The E35 exits in `SORTEXIT/` carry accumulator state across calls, and the point at which that
state is reset is a business decision written in COBOL rather than in any specification. **No
static analysis reads reset points as business rules, and behaviour that only appears when the
merge spills across more than one work dataset cannot be observed below the STRESS profile at
all.**

### 3.2 Business rules inside sort control cards

Separate from the exits: the cards themselves carry rules that no program knows about.

| Card | The rule | Who else knows |
|---|---|---|
| `CABSRT03` | `SUM FIELDS=(120,7,PD)` — **the estate's only dedup-by-summing rule.** `CABING01.jcl` STEP070's own comment says: *"NO CABING PROGRAM KNOWS ABOUT IT."* | Nobody |
| `CABSRT02` | `OMIT COND=(30,1,CH,GE,C'6',AND,30,1,CH,LE,C'9')` — drops fatal edit-status records ahead of OCN/BAN validation | Nobody |
| `CABSRT01` | `INCLUDE` on record type 01–08 **and** rate element `'03'`/`'05'`/`'07'` | Nobody |
| `CABSRT06` | `INCLUDE COND=(180,6,CH,EQ,C'202608')` — **a hand-maintained bill-period literal in a control card.** `CABRT11R.jcl`'s comment records that it is maintained by hand. | Nobody |
| `CABSRT13` | `INCLUDE` on four hardcoded OCNs — `'0288'`, `'6006'`, `'9104'`, `'2214'`. **The EDI partner list is a sort card.** | Nobody |
| `CABSRT14` | `OUTFIL OMIT=(1,1,CH,EQ,C'D',AND,375,8,PD,EQ,0)` — **drops zero-value details that the trailer hash still includes.** | Nobody |
| `CABSRT16` | The duplicate test **deliberately excludes the run id**, so the reported suspense age is the age of the *first* suspension | Nobody |
| `CABSRT18` | `INCLUDE COND=(74,11,CH,EQ,C'OUT OF BAL ')` — **tests the result text, not the difference field**, so anything inside the program tolerance is invisible here too | Nobody |
| `CABSRT15` | The keep-the-last-rerun rule depends on `OPTION EQUALS` **and** an E35 exit | Nobody |

**`OPTION EQUALS` appears on 15 of the 18 cards.** It forces the sort to preserve the input order
of equal-key records. `CONVENTIONS.md` requires *"Accumulate in the order records arrive. Never
re-sequence before summing."* **The correctness of every accumulator in the estate depends on a
three-word operand in a data file.** Remove `EQUALS` and the money changes.

### 3.3 JCL symbolic parameters supplied at submission

**CAST will show `// EXEC CABPDB2P` with `&&CYCLDT` and `&&BILLPR` and nothing that resolves
them.** That is `REXX/_MANIFEST.md`'s own words.

`REXX/CABGENJC.exec` reads `TELCABS.CABS.CONTROL.PARMTBL` — an 80-byte fixed-card dataset holding
one row per job/symbolic combination with an effective-cycle window and an active switch — selects
every row that is active and in window, builds `// SET` statements and an `// EXEC proc,SYM=val`
statement, writes a **throwaway** temporary JCL member, submits it, and **deletes the member
immediately.**

> *"The JCL library member for a job like `CABS3100` never contains the cycle date, the bill
> period, the DB2 subsystem name, or any of the other symbolics that job runs with each cycle …
> The actual values a given run used are recoverable only by finding the `PARMTBL` row that was
> active for that run's date, or by reading the generated-and-discarded temporary JCL member out of
> the JES spool before it ages off."*

**`TELCABS.CABS.CONTROL.PARMTBL` is not in this repository.** It is a data file, and the values
that determine behaviour live in it.

Two token conventions, which will confuse any parser that assumes one:

| Convention | Team | Examples |
|---|---|---|
| `&NAME` | CABS ingest/rating/billing/format/report | `&CYCLE`, `&BILLPER`, `&RUNID`, `&MODE`, `&RERUN` |
| `%%NAME` | SETL settlement, jurisdiction DB2 posting, month-end | `%%CYCLDT`, `%%BILLPR`, `%%RSTEP`, `%%CLSPER`, `%%EXPPRC`, `%%SIGNOF` |

**`%%NAME` is not JCL syntax at all.** It is a scheduler placeholder. CAST will not treat it as a
symbolic parameter because it is not one.

**Symbolics with no default anywhere in the estate** — these are the ones where CAST can tell you a
value is missing but cannot tell you what it should be:

`%%RSTEP` (`CABS7000` job card — an unsubstituted restart is a JCL error, deliberately),
`RSTFRM`/`RSTWIN` (`CABJ1600`), `CAPOVR` (`CABS2500` — **zero means "use the agreement cap", not
"no cap"**), `EXCHDT` (`CABS2600`), `MAXELM` (`CABS4100`), `FORCSW`/`OCNFRM`/`OCNTHR` (`CABS4000`),
`PREFIX` (`CABS4600`), `EXPPRC` (`CABS6000` — differs by cycle type),
`CLSPER`/`LEDGCO`/`SIGNOF` (`CABS6400`), `LINPGE`/`COMPNY` (`CABS5000`),
`EDIVER`/`SENDID` (`CABS5300`), `TAXDT`/`FEDRAT` (`CABS4400`), `AGEBAS` (`CABS4200`),
`MINBIL`/`MAXBIL` (`CABS4350`), `TOLER` (`CABS4550`), `EDSTYL` (`CABS5100`),
`VOLSER` (`CABS5400`), `SUMSTL` (`CABS5200`).

`CABING01.jcl`'s own comment box ends: **`DO NOT SUBMIT THIS MEMBER UNEDITED.`**

### 3.4 PROC override resolution

Complexity 11 (shared job fragments) and 13 (job overrides) are placed throughout. **CAST can show
that `CABS4100` and `CABS4150` both `EXEC CABPBDTL`. It cannot show you that they get different
behaviour, because the difference is in the overrides.**

Examples from the manifests:

| Job pair | Same PROC | The difference |
|---|---|---|
| `CABS4100` / `CABS4150` | `CABPBDTL` | `CABS4150` carries **five DD overrides including three `DUMMY`** |
| `CABS4250` / `CABS4300` | `CABPBADJ` | `CABS4250` sends `SETLIN` to `DUMMY`; `CABS4300` points it at `SETL.NET` at `(-1)` — **the cross-application read exists in one and not the other** |
| `CABJ1600` / `CABJ1650` | `CABPREST` | `CABJ1650` sends `ADJOUT` to `DUMMY` — **that single override is the entire difference between a live restatement and a simulation** |
| `CABS2100` / `CABS2300` | `CABPMPBX` | Same PROC, different values, `DUMMY` overrides |
| `CABS2600` / `CABS2700` | `CABPCMDS` | **Same PROC, opposite direction** — outbound vs inbound exchange |
| `CABS4550` / `CABS4600` | `CABPBHDR` | `CABS4550` has **four `DUMMY` overrides** |
| `CABS6100` | `CABPRPTB` twice | **Six `DUMMY` overrides** |
| `CABS5500` | `CABPFMTP` twice | **Six `DUMMY` overrides**, and substitutes `N` and a blank campaign — which is what makes `CABFMT09` dormant |

**A DD overridden to `DUMMY` is a business rule.** `CABS4250` not reading settlement and `CABS4300`
reading it is the difference between an invoice that nets the counterparty position and one that
does not. In the model, both jobs run the same PROC.

**`CABPRPTB` is used by `CABS6000`, `CABS6100`, `CABS6200`, `CABS6300` and `CABS7000`** — five jobs,
one fragment, five override sets. `CABPDB2P` is used by `CABJ1900` (CABS), `CABS3100` and
`CABS3200` (SETL) — **a shared fragment spanning both applications.**

### 3.5 Dynamic call targets built from table data

Covered as ground truth in §2.2. The point restated as a limitation:

**The Inference Engine will recover `'CABRAT'`. It will never recover the suffix, because
`R2-EN-MODULE-SFX` is a field in a rate table record read from VSAM at run time.** The set of
programs that actually run on any given night is determined by the *contents of the rate table*,
not by anything in the source.

Consequences that matter for an estimate:

1. **You cannot enumerate the modules in scope from the source.** The eight `CABRAT**` targets
   listed in `CONTRACTS/complexity_placement.json` are what the *design* says; whether a ninth
   suffix has ever appeared in the rate table is a data question.
2. **`CABJUR03`'s `CABJX` + state suffix means up to 50 modules**, and `CABJXCAL` appears in **two
   of the five STEPLIB libraries** on `CABJ1200` — so even for a target you *can* name, which copy
   runs is a load-time decision.
3. `CABSET07` has **five regional formatters never harmonised**. Which one a settlement record gets
   depends on the counterparty's region.

### 3.6 Which CTC site copy is authoritative

**`CTC/_MANIFEST.md` §5 is unambiguous:**

> *"The estate does not answer this question. There is no artefact anywhere in it that identifies
> an authoritative copy of either program, and no rule that can be applied to derive one."*

And specifically about static analysis:

> *"`CAST Imaging` will not settle it. Static analysis will correctly report twelve `CABCTC01`
> programs with near-identical structure and will correctly identify the paragraphs that differ.
> It cannot tell you which behaviour the business is supposed to have. Clone detection reports the
> similarity; it does not report the intent."*

Everything that looks like it might settle it does not: the version literal (three divergent copies
carry the group's `V1.09`), the revision history (two histories stop at a release the others moved
past, and a history that stops is indistinguishable from one never updated), the highest version
(both outliers are local changes nobody else took), the most common copy (seven identical is a
majority, not a decision — and the 2016 ISP-minute exclusion that eleven copies carry is a
*settlement obligation*, so the one copy lacking it is arguably the wrong one, and majority
reasoning would have protected it if the vote had gone the other way), or file size and timestamps
(size tracks local edits, timestamps are the last library copy).

**Four questions a person with authority over wholesale settlement has to answer, that no tool can:**

1. Should ISP-bound minutes sit in the local bucket or the interstate bucket, and from what date?
   Eleven centres say local since 2016. Newark says interstate. **One of those positions is the
   settlement obligation and the other is exposure.**
2. Should the summary minute total round or truncate? The two rules produce different invoices.
   Both have been in production for a decade.
3. Should the Chicago trunk group edit apply everywhere, apply nowhere, or has it been suspending
   valid records since the study it fed was retired in 2019?
4. Should the reconciliation tolerance be 0.05 or 0.25, one-sided or two-sided, and should the
   three centres with no reconciliation program at all have one?

**Until those four are answered, any migration of this family is a migration of an undecided
question.** Converting twelve copies faithfully preserves twelve behaviours. Converting one and
pointing all twelve centres at it silently changes the invoice at up to eleven of them. Both are
decisions. Neither is a technical decision.

The realistic sequence: reconcile the twelve into one specification with the differences explicit
(mechanical — tooling helps a lot); take the four business decisions (human — no tooling helps at
all); then build one program. **The middle step determines the timeline, and it is invisible in a
static analysis report.**

### 3.7 Dataset ownership

CAST will show that `CABSET01` reads `TELCABS.CABS.BILLDTL`. **It will not tell you who owns it,
who may change it, or what happens if they do.**

The estate contains no ownership register. What it contains instead:

- A **naming convention** (`TELCABS.<APP>.<NAME>`) that is *evidence* of ownership, not a
  declaration of it. `CONVENTIONS.md` states the convention; nothing enforces it.
- **`TELCABS.CABS.FACTOR` is defined twice** — by `VSAM/CABVS060.jcl` and by `JCL/CABGDGDF.jcl` —
  with different attributes. **Neither has ever been treated as the system of record**
  (`VSAM/_MANIFEST.md`). Same for `CARRIER`, `CIRCUIT` and `RATE`. Most starkly, `CABGDGDF.jcl`
  still defines `RATE` at the pre-1996 fixed `RECORDSIZE(120 120)` while `CABVS050` sizes it to the
  full ODO band table at `RECORDSIZE(67 619)`.
- **`SHAREOPTIONS` drift with no recorded reason.** `(2 3)` is the estate default. `(3 3)` appears
  on `CABVS060`, `CABVS100` and `CABVS120` with **no comment explaining the departure**. `CABVS080`
  is the only `(4 3)` cluster and *does* carry an explanation (a CICS inquiry region concurrent
  with the nightly batch writer). **An analyst cannot tell from the source whether the `(3 3)`
  members were considered decisions or copy-paste drift.**
- **Volser `TELV09`.** `CABVS060`'s revision history claims the Factor table was moved off it in
  2009 ahead of a controller swap. `CABVS100` still allocates against `TELV09`, with a 2015 comment
  asserting the volser is "advisory only" because SMS ACS routines redirect it — **a claim
  `CABVS100`'s JCL has no way to verify.** Whether `TELV09` is live, decommissioned or
  SMS-overridden depends on which member's comment you read.

### 3.8 HLASM behavioural semantics — including the divergent rounding

**CAST can tell you `CABPKDEC` exists. It cannot tell you it rounds differently from every one of
its callers.**

From `HLASM/_MANIFEST.md`:

> `CABPKDEC`'s `MUL`, `DIV` and `RND` functions round **half away from zero at the fifth decimal
> place** … Its COBOL callers in RATING and SETTLE use `COMPUTE … ROUNDED`, which under OS/VS COBOL
> is **half-up at the receiving field's scale — normally two decimal places** — and which for a
> negative intermediate rounds toward zero in some compiler releases.

The two conventions disagree on **the place** at which rounding happens (5dp vs 2dp) and **the
direction** for negatives (away from zero vs toward zero). **Amounts that pass through `CABPKDEC`
and are then moved to a `PIC S9(nn)V9(02)` field are rounded twice, in two directions.**

The comment block in `CABPKDEC` presents its convention as the house standard (`CABS-STD-019`) and
does not mention the callers. **Both sources read as correct in isolation. That is the point.**

**And `CABPKDEC` is called dynamically** — the name is built in working storage. It appears in no
binder-derived call graph.

**The second HLASM problem is worse, because a graph will draw it wrong rather than not at all.**

`CABDATCV` exists **twice**, same name, same entry points, different behaviour:

| | `HLASM/CABDATCV.asm` | `HLASM/EMERG/CABDATCV.asm` |
|---|---|---|
| Version | V2.03 (2018) | V1.08E (1998) |
| Load library | `TELCABS.COMMON.LOADLIB` | `TELCABS.SETL.LOADLIB.EMERG` |
| **Century pivot** | **70** | **68** |
| Leap-year rule | ÷4, not ÷100 unless ÷400 | **÷4 only** |
| `ADDD` | Absolute day number (rewritten 2003) | Steps the day-of-year, rolls the year (1998 emergency fix) |
| Functions | `JTOG GTOJ JTOA ATOJ ADDD DIFF LEAP` | `JTOG GTOJ ADDD LEAP` only |

**Which one a program gets depends entirely on STEPLIB concatenation order:**

| Job | STEPLIB order | Binds |
|---|---|---|
| `CABS2900` dispute handler | `SETL.LOADLIB.EMERG`, `SETL.LOADLIB`, `COMMON.LOADLIB` | **1998, pivot 68** |
| `CABS3200` settlement posting | `SETL.LOADLIB.EMERG`, `SETL.LOADLIB`, `CABS.LOADLIB`, `COMMON.LOADLIB` | **1998, pivot 68** |
| `CABS2450` wireless settlement | `SETL.LOADLIB`, `SETL.LOADLIB.EMERG`, `COMMON.LOADLIB` | 1998, **if `SETL.LOADLIB` does not hold a copy** |
| `CABRAT01`, `CABJ1800`, `CABJ2000`, all RATING reruns | `CABS.LOADLIB`/`SETL.LOADLIB`, `COMMON.LOADLIB` | **2018, pivot 70** |

> *"So the same source-level call — `CALL 'CABDTCNV'` — resolves a two-digit year of 68 or 69 to
> 1968/1969 in the settlement jobs and to 2068/2069 in the rating jobs, and applies a different
> leap-year rule for century years. Nothing in the COBOL, and nothing in the JCL comments, says so.
> The only place the difference is visible is by comparing the two assembler sources, which are in
> different source libraries."*

**A single node labelled `CABDATCV` in any dependency graph is wrong. There are two
implementations and the edge is decided at load time.** `CABING01.jcl` STEP090 has the same shape
for a different module — `TELCABS.CABS.LOADLIB.TEST` is concatenated **first**, from a 2019 soak
(change CAB-2019-0447) that was never removed.

**Third HLASM item.** `CABBITST` is the only code in the estate that understands the edit-status
byte as a **bit map**. The COBOL 88-levels over the same `PIC X(01)` byte encode a different,
incompatible interpretation as a display digit. **A byte with more than one bit on satisfies a
different 88 from the one the intake set.** Neither source says so. (And, per §2.1, no COBOL
program actually names `CABBITST` — the manifest's claim of four static callers is not borne out by
the source, which is itself a finding.)

### 3.9 CICS COMMAREA state rules

`ONLINE/_MANIFEST.md` lists seven cross-program state dependencies carried through
`COPYBOOKS/CABSCOMM.cpy` and says of them:

> *"None of them is documented in a comment beyond the bland, generic notes already in the copybook
> and the source below (per house style, no comment names the rule or its consequence) — they exist
> only as behaviour, the way this kind of coupling actually survives in a legacy estate."*

**CAST will show that all six CICS programs `COPY CABSCOMM`. It cannot show you that a factor
update silently blocks a dispute.**

| Field | Set by | Tested by | Effect |
|---|---|---|---|
| `CM-FLAGS` byte 5 | `CABONL04` `P5000-PROCESS-CONFIRM` on a successful `REWRITE`/`WRITE` to `RATEFCTR` | `CABONL05` `P3000-RECEIVE-AND-VALIDATE` | **A factor update this cycle silently blocks a dispute against the same OCN/period. Nothing resets it** — it survives for the life of the COMMAREA chain. |
| `CM-BR-LAST-KEY` / `CM-BR-PAGE-NBR` / `CM-BR-EOF-SW` / `CM-BR-BOF-SW` | Whichever of `CABONL02`/`03`/`06` most recently did a lookup or page | The same program on the next PF7/PF8 | **PF7/PF8 silently do nothing useful unless a lookup already ran in this session.** The browse position is never independently re-derivable from the screen. |
| `CM-SAVED-KEY-1` | `CABONL02` (OCN) | **Nothing reads it back** | Slot 1 is understood estate-wide to mean "the OCN currently in view". **That convention is not written anywhere.** |
| `CM-SAVED-KEY-2` | `CABONL03` (BAN) | Nothing reads it back | Same shape. |
| `CM-SAVED-KEY-3` | `CABONL05` `P4000`; `CABONL06` `P2200`/`P3500`/`P3600`/`P8600` | `CABONL05` `P1800`/`P4000`; `CABONL06` `P6000` | **The same 20-byte slot is packed two different ways** depending which program last touched it. Bytes 1–4 are always an OCN, 5–10 a settlement period, and **nothing enforces that the two programs agree beyond both having been written by the same 2004 project.** |
| `CM-RETURN-TO` | `CABONL01` (always `'CAB0'`); `CABONL06` `P8600` (`'CAB5'`) | `CABONL05` `P1800`, `P3000`, `P5600` | **Entering `CABONL05` from `CABONL06` instead of the menu changes field requirements** (BAN not required) and prefills OCN/period — **a second, undocumented entry contract for the same transaction.** |
| `CM-FUNCTION-CD = 'SP'` | `CABONL05` `P4000`, immediately before `LINK`, cleared after | `CABONL06` `P0000-MAINLINE` | **The only signal distinguishing "attached as CAB5" from "linked as a subroutine."** Any other program linking to `CABONL06` without setting it gets full map-sending behaviour instead of a silent lookup. |
| `CM-FILLER` bytes 1–9, locally redefined as `LK-SETL-NET-DUE`/`LK-SETL-DIRECTION` in both programs | `CABONL06` `P6000` | `CABONL05` `P4000`, `P5600` | **The copybook calls this "general-purpose growth room". Nine of its twenty bytes are a private, offset-dependent contract between exactly these two programs.** Widening any earlier field in `CABSCOMM.cpy` without updating the hardcoded `PIC X(372)` filler in both local `REDEFINES` **would silently corrupt this handoff.** |

That last one is the sharpest: **a change to a copybook field length breaks a contract that is not
expressed anywhere except two hardcoded filler sizes.**

### 3.10 Everything else static analysis cannot reach here

Collected so nothing is left implicit.

| # | What CAST misses | Where |
|---|---|---|
| 1 | **Which IMS database a DL/I call actually touches, and how.** 36 `CALL 'CBLTDLI'` sites. CAST can recover segment and field names from SSA structures. It **cannot** recover the database organisation (HDAM vs HIDAM vs HISAM), the hierarchy (so it cannot know what a `GNP` returns), key structure and lengths, logical relationships and secondary indexes, or `PROCOPT` (**so it cannot tell a read-only program from an updater**). | `IMS/_README.md` |
| 2 | **Two IMS databases have no DBD or PSB anywhere in the estate** — `CABRATDB` (rate/tariff) and `CABBANDB` (account/BAN). Deliberate: the reference engagement found **69 programs issuing DL/I calls against databases whose DBD/PSB source was not in any library handed over.** | `IMS/_README.md` |
| 3 | **`PROCSEQ=CABSETSX` is in the PSB, not the program.** `CABCTL06` reads the settlement database in secondary-index sequence and gets the **index** key in the PCB key feedback area, not the root key — so it takes the root key out of the segment instead. **The PSB is the only place in the estate that says so.** | `IMS/_MANIFEST.md` |
| 4 | **Which of the two stores is authoritative when they diverge.** Six programs write the same fact to IMS-or-DB2 *and* VSAM with no coordination. `CABCTL05` prints `RUN CABSRSYN` on divergence and **`CABSRSYN` does not exist anywhere in the estate.** `CABSET13`'s resync utility was written in 2012 and has never been scheduled. | `BATCH/CONTROL/_MANIFEST.md`, `BATCH/SETTLE/_MANIFEST.md` |
| 5 | **That three programs balance by moving zero into a count.** `CABCTL01` counts rows that did not fit on its extract into `WS-SUMM-CNT`, then moves **zero** into `CT-SUMMARISED`. The equation holds; the loss appears only on a printed listing. | `BATCH/CONTROL/_MANIFEST.md` |
| 6 | **Rounding intent.** Complexity 21 is placed in ~20 programs. `CABJUR04` rounds what `CABJUR05` truncates; `CABBIL06` rounds what `CABBIL09` truncates; `CABSET09` rounds what `CABSET11` truncates; `CABRPT02` recomputes from the 5dp accumulator at every level while `CABRPT05` truncates. **`CABJUR05`'s history records that V2.02 aligned them in 2000 and V2.03 backed it out in 2002.** CAST sees `ROUNDED` and a `MOVE`. It cannot see that they are the same quantity. | Family manifests |
| 7 | **`RT-ROUND-RULE` is drawn per rate record.** `CONVENTIONS.md`: *"Rounding is taken from `RT-ROUND-RULE` on the rate record, NOT from a global convention."* **Sibling records for the same element in different states disagree about rounding.** The rule is data. | `CONVENTIONS.md`, `GENERATORS/_README.md` §5 |
| 8 | **Carriage control characters that carry business meaning.** `CABFMT01`: `'7'` is both skip-to-channel-7 **and** start-of-invoice; `'4'` is both skip-to-channel-4 **and** start-of-bill-section. `CABFMT04` finds section breaks **from the carriage control character, not from a data field.** The section count on the print control record is what the mailroom reconciles against. | `BATCH/FORMAT/_MANIFEST.md` |
| 9 | **Overlapping 88-levels resolved by test order.** `WS-SC-USAGE-SECTION` `'U1' THRU 'U3'` and `WS-SC-CHARGE-SECTION` `'U2' THRU 'C4'` are both true for U2 and U3, and `CABBIL03 P4200` **tests usage first**. `WS-TX-FEDERAL` `'FE' THRU 'FS'` and `WS-TX-SURCHARGE` `'FS' THRU 'SU'` are both true for FS, and `CABBIL07 P3400` tests federal first — **so an FS component is computed on the usage base rather than the charge base.** The behaviour is the *order of the tests*, not the declarations. | `BATCH/BILLCALC/_MANIFEST.md` |
| 10 | **Physical record order as data.** `CABRAT12` derives the retry attempt count from **physical position** in `AUDLOG`, not from a stored field. `CABING07` writes three records per run and **their order is the audit trail**, read back positionally. `CABING08`'s `CFWOUT` physical order is the cycle sequence and next run's release logic depends on reading it back in that order. | Family manifests, `CABING01.jcl` |
| 11 | **`CD-REC-TYPE` `'03'` and `'05'` are ambiguous in the frozen copybook.** `'03'` satisfies both `CD-VOICE-MOU` and `CD-DATA-SVC`; `'05'` satisfies both `CD-DATA-SVC` and `CD-SPECIAL-ACC`. **Which overlay a program reads decides what the record means.** | `COPYBOOKS/CABSCDR.cpy`, `HARNESS/_README.md` §6 |
| 12 | **Record length is not always what a field walk gives you.** One copybook self-disagreement remains: `CABSPRNT`'s `PC-BODY-A REDEFINES PC-BODY` is 118 bytes against 132. The bigger trap is `CABSBILL`: it declares LRECL **1651** against an ODO maximum of **1,647 data bytes**, and the 4-byte gap is the RDW that every RECFM VB record carries. **A tool that compares a copybook's computed length with the LRECL on the DCB will report a false 4-byte disagreement on every variable-length file in the estate, and a tool that sizes a VB buffer from the copybook alone will be 4 bytes short.** The COBOL `FD` clauses quote 1647, the JCL and the sort `RECORD TYPE=V` card quote 1651, and both are correct. | `GENERATORS/_README.md` §8 |
| 13 | **Silent truncation.** `CABRAT10 P5200` moves a VB 1647 group to `PIC X(500)`. `CABRPT06 P4000` moves the same group to `PIC X(400)` — **"the key fields are then restated over the top, which is why the record still looks correct."** | `BATCH/RATING/_MANIFEST.md`, `BATCH/REPORT/_MANIFEST.md` |
| 14 | **Revision histories that are wrong.** `CONVENTIONS.md` requires it: *"Some must be WRONG or describe behaviour that no longer exists — that is deliberate."* `CABCTL02`'s 2007 "two phase commit added" note only added a `CHKP`. `CABLGCNV`'s "retained for the remaining sites on the 1987 format" is true of no site. **A tool that mines comments for intent will be confidently wrong.** | `CONVENTIONS.md` and throughout |
| 15 | **The E15/E35 return-code protocol as business logic.** `SORTEXIT/_README.md` §4: *"The business rule is the return code, not the code around it. A paragraph that computes carefully and then falls through to `MOVE 4 TO RETURN-CODE` has thrown the record away."* | `SORTEXIT/_README.md` |
| 16 | **`OPTION EQUALS` on 17 of 18 sort cards** is what makes every accumulator in the estate correct. See §3.2. | `JCL/CTLCARDS/` |
| 17 | **The twelve seeded defects.** None is annotated in source. **All twelve are semantic, and none is detectable as a code-quality violation.** They are tolerance values, comparison operators, the placement of a reset and a fixed field one declaration narrower than the record it holds — each individually valid COBOL that a linter has no reason to flag. Which defect sits where is in `SEALED/`. | `SEALED/` |

---

## 4. How to score CAST's recall

### 4.1 The answer sheet

**`DOCS/Complexity_Traceability_Matrix.xlsx`** is the scoring instrument. It maps every one of the
27 complexity constructs to the specific file and paragraph where it is placed, and it is produced
by a separate workstream from the authoritative source of record,
`CONTRACTS/complexity_placement.json`.

`DOCS/Process_Contract_Register.xlsx`, from the same workstream, carries the process-boundary
contracts and is the companion input when you are scoring dataset coupling and the control chain
rather than code constructs.

> **Cross-check the matrix against the JSON before you score against it.**
> `CONTRACTS/complexity_placement.json` is the authoritative register — the matrix is derived from
> it. If the two disagree, the JSON wins and the matrix needs regenerating. The family
> `_MANIFEST.md` files carry the same placements in prose and are a third, independent check.

**`CONTRACTS/complexity_placement.json` is authoritative.** `CONVENTIONS.md`: *"Do not invent
complexity placements. Implement ONLY what `CONTRACTS/complexity_placement.json` assigns to your
files. If you need to add one, add it to that file so the traceability matrix stays
authoritative."*

### 4.2 The 27 constructs

Use this wording — it is the canonical plain-English form, deliberately free of mainframe
vocabulary.

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

### 4.3 Scoring method

**Step 1 — export CAST's findings** at object-and-property granularity: object name, type, source
file, line, and the rule or property that fired.

**Step 2 — build the placement list** from the traceability matrix (or, for now, from
`CONTRACTS/complexity_placement.json` plus the family manifests). One row per placement: construct
number, file, paragraph or line, one-line description.

**Step 3 — join on file + paragraph** and classify each placement:

| Outcome | Definition |
|---|---|
| **FOUND** | CAST reported an object or violation at that file and paragraph that corresponds to the construct |
| **PARTIAL** | CAST reported the object but not the property that matters (e.g. drew the `CABDATCV` edge but as one node) |
| **MISSED** | No CAST finding at that location |
| **NOT DETECTABLE** | The information is not in the artefacts scanned — §3 |

**Step 4 — score by construct, not by count.** A per-construct recall table is far more useful than
a single percentage, because the constructs are not equally hard and are not equally expensive to
get wrong.

```
Construct    Placed   Found  Partial  Missed  Not-detectable   Recall
01 Fall-through   11      ?       ?       ?          0             ?
02 Dynamic call    9      ?       ?       ?          9*            ?
…
14 Sort logic     13      0       0       0         13            0%
```

\* Detectable *as a dynamic call site*; **not detectable as a resolved target**. Score those two
things separately or you will flatter the tool in one direction and punish it unfairly in the
other.

**Step 5 — score the seeded defects separately, and expect zero.**

Eleven defects, keys in `SEALED/`. **None is annotated in source and none is a code-quality
violation.** A static analyser is not the instrument for these; `HARNESS/` is. Report the defect
recall as a separate line and expect **0 of 11 from CAST**. That is not a failure — it is the
finding, and it is precisely why the harness exists alongside the scan.

**Step 6 — publish the not-detectable list as a deliverable in its own right.**

The §3 items are not a footnote to the CAST report. They are the modernization risk register.
Each one is work that must be done by a person reading source and talking to the business, and none
of it appears on a scan dashboard. **The estimate that omits §3 is the estimate that overruns.**

### 4.4 What a good result looks like

| Class | Realistic expectation |
|---|---|
| Structural constructs (4, 5, 6, 7, 8, 23, 24) | **High recall.** These are visible in the source text. |
| Control-flow constructs (1, 3, 26) | **High recall — if** `GO TO` and `PERFORM … THRU` ranges are modelled (§1.7). Low if not. |
| JCL constructs (10, 11, 12, 13, 15) | **Moderate.** CAST sees the structure; it does not see which override changes behaviour. |
| Data-driven constructs (2, 9) | **Call site found, target not resolved.** Correct behaviour, and it must be reported as unresolved rather than dropped. |
| Cross-application (16) | **High — but only if you loaded two applications.** Zero if you loaded one. |
| Semantic constructs (19, 21, 22, 25, 27) | **Low.** These are about *meaning*, not structure. |
| Sort logic (14) | **Zero.** §3.1. |
| Two stores one update (17) | **Low.** CAST sees a DB2 `INSERT` and a VSAM `REWRITE`; the absence of coordination is not an artefact. |
| Append writes (18) | **Low.** `DISP=MOD` is visible; "physical order is the audit trail" is not. |
| The twelve seeded defects | **Zero, by design.** |

**The headline number is not "what percentage did CAST find". It is: *of the work this
modernization requires, what proportion is visible to a scan?*** Answer that with the four columns
in §4.3 Step 4 and the §3 list, and you have an honest assessment instead of a tool score.

---

## 5. Sources within the estate

| Topic | Read |
|---|---|
| Build rules, compiler targets, the control-record contract | `CONVENTIONS.md` |
| **The sort-exit blind spot — read this first** | `SORTEXIT/_README.md`, `SORTEXIT/_MANIFEST.md` |
| Multi-site divergence and the authoritative-copy problem | `CTC/_MANIFEST.md`, `CTC/_SITE_INDEX.md` |
| Missing IMS DBDs and what a scan cannot recover from SSAs | `IMS/_README.md`, `IMS/_MANIFEST.md` |
| Library search order, the two `CABDATCV`s, the rounding divergence | `HLASM/_MANIFEST.md` |
| COMMAREA state rules that exist nowhere in writing | `ONLINE/_MANIFEST.md` |
| Two stores one update, hidden error handlers, the PL/I dead-code bridge | `BATCH/CONTROL/_MANIFEST.md` |
| Orphan datasets, attribute drift, the `CABVS060` classification caveat | `VSAM/_MANIFEST.md` |
| Why the utility tier must not trigger clone detection | `BUILDER/_README.md` |
| Copybook self-disagreements, data conditions, band-boundary probes | `GENERATORS/_README.md` |
| The L1–L5 comparison and the three-way verdict | `HARNESS/_README.md` |
| Authoritative complexity placements | `CONTRACTS/complexity_placement.json` |
| Per-family placements in prose | `BATCH/*/_MANIFEST.md` |
| Seeded defect answer key — **withhold for a blind run** | `SEALED/*.json` |

---

*`DOCS/CAST_IMAGING_GUIDE.md` · CABS Tier 5 wholesale access billing reference estate.
Companion documents: `DOCS/HERCULES_RUNBOOK.md`, `README.md`, `CONVENTIONS.md`.*
