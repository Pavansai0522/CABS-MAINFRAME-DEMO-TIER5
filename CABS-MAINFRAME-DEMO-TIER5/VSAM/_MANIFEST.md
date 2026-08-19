# VSAM / IDCAMS Definition Library - Manifest

Scope: `BASE/VSAM/` - 14 IDCAMS job members (`CABVSnnn.jcl`) that define the VSAM clusters,
alternate index/path, and the loader/rebuild utilities behind the CABS Tier 5 estate. This
manifest is analyst-facing and states complexities plainly, unlike the JCL member comments
themselves, which stay in period-authentic house style (see `CONVENTIONS.md`).

## File index

| File | Lines | Purpose | Dataset(s) defined | Complexities carried | RUNNABLE on TK4- / REFERENCE-ONLY |
|---|---|---|---|---|---|
| CABVS010.jcl | 69 | Define the CDR input master KSDS | `TELCABS.CABS.CDR.KSDS` (+ `.DATA`/`.INDEX`) | Estate's largest cluster; IMBED/REPLICATE retained from the 1987 original | REFERENCE-ONLY |
| CABVS020.jcl | 71 | Define the CDR clean staging ESDS | `TELCABS.CABS.CDR.CLEAN.ES` | **Complexity 15** - undeclared elsewhere | REFERENCE-ONLY |
| CABVS030.jcl | 65 | Define the Carrier (OCN) master KSDS - library-standard define, parallel to the bulk provisioning copy in `CABGDGDF.jcl` | `TELCABS.CABS.CARRIER` (+ `.DATA`/`.INDEX`) | Dataset attribute drift vs. `CABGDGDF.jcl` (see below) | REFERENCE-ONLY |
| CABVS040.jcl | 71 | Define the Carrier ACNA alternate index and path | `TELCABS.CABS.CARRIER.AIX`, `TELCABS.CABS.CARRIER.PATH` | **Complexity 15** - AIX/PATH undeclared elsewhere | REFERENCE-ONLY |
| CABVS050.jcl | 62 | Define the Rate table KSDS, variable-length record to carry the full `RT-BAND-TABLE` ODO area | `TELCABS.CABS.RATE` (+ `.DATA`/`.INDEX`) | Complexity 7-adjacent (variable RECORDSIZE for an ODO area); dataset attribute drift vs. `CABGDGDF.jcl` | REFERENCE-ONLY |
| CABVS060.jcl | 70 | Define the PIU/PLU Factor table KSDS | `TELCABS.CABS.FACTOR` (+ `.DATA`/`.INDEX`) | **Complexity 15 as briefed - see caveat below** | REFERENCE-ONLY |
| CABVS070.jcl | 60 | Define the Circuit/trunk group inventory KSDS | `TELCABS.CABS.CIRCUIT` (+ `.DATA`/`.INDEX`) | none | REFERENCE-ONLY |
| CABVS080.jcl | 63 | Define the Bill header/invoice summary KSDS | `TELCABS.CABS.BILLHDR` (+ `.DATA`/`.INDEX`) | Dataset attribute drift - only `SHAREOPTIONS(4 3)` member in the estate | REFERENCE-ONLY |
| CABVS090.jcl | 71 | Define the Bill detail retention ESDS, sized for the 40-element `CABSBILL` variable record | `TELCABS.CABS.BILLDTL.ES` | **Complexity 15** - undeclared elsewhere; Complexity 7-adjacent (variable RECORDSIZE) | REFERENCE-ONLY |
| CABVS100.jcl | 62 | Define the inter-carrier Settlement master KSDS (SETL app, not CABS) | `TELCABS.SETL.MASTER` (+ `.DATA`/`.INDEX`) | Dataset attribute drift - stale/decommissioned volser retained | REFERENCE-ONLY |
| CABVS110.jcl | 69 | Define the Suspense record ESDS | `TELCABS.CABS.SUSPENSE.ES` | **Complexity 15** - undeclared elsewhere | REFERENCE-ONLY |
| CABVS120.jcl | 60 | Define the Run control/balancing record ESDS | `TELCABS.CABS.RUNCTL.ES` | **Complexity 15** - undeclared elsewhere | REFERENCE-ONLY |
| CABVS130.jcl | 66 | BLDINDEX the Carrier AIX from the base cluster, plus LISTCAT/EXAMINE verification | (populates `TELCABS.CABS.CARRIER.AIX`; defines no new cluster) | Companion utility to the Complexity 15 AIX in CABVS040 | REFERENCE-ONLY |
| CABVS140.jcl | 75 | REPRO sequential-to-KSDS initial load of Carrier and Rate, with a SKIP/COUNT restart facility and a VERIFY pass | (loads `TELCABS.CABS.CARRIER`, `TELCABS.CABS.RATE`; defines no new cluster) | Scheduler-symbolic restart pattern (`%%SKIPCT`/`%%LOADCT`), consistent with the `%%CYCLDT`/`%%RUNID` convention used estate-wide | REFERENCE-ONLY |

**Total: 14 job members, 934 lines, plus this manifest.**

## RUNNABLE vs. REFERENCE-ONLY

All 14 members are marked REFERENCE-ONLY because none of the target VSAM volumes (`TELV01`-
`TELV12`) exist on any real or emulated DASD pool, and several members carry syntax an operator
would need to correct before first execution on a genuine MVS 3.8j (Hercules TK4-) IDCAMS:

- **CABVS130 (BLDINDEX)** codes `WORKFILES(3)` as a BLDINDEX parameter. Real IDCAMS BLDINDEX has
  no such keyword - the three `SORTWKnn` DDs alone drive the external sort. This would need to be
  dropped before the job would assemble/execute cleanly.
- **CABVS140** carries `PARM='%%SKIPCT,%%LOADCT'` on the `EXEC PGM=IDCAMS` statement. IDCAMS does
  not read a step PARM this way; the SKIP/COUNT values are actually substituted directly into the
  SYSIN REPRO cards by the job scheduler before submission. The step PARM is vestigial decoration,
  copied from the pattern used on the real batch programs elsewhere in the estate (e.g.
  `CABS3100.jcl` `PARM=%%CYCLDT`) - harmless, but it does nothing here.
- **CABVS050 and CABVS090** use a variable `RECORDSIZE(min max)` to carry the ODO band area and
  the 40-element bill detail respectively. This is ordinary VSAM (no `SPANNED` keyword needed
  since both maxima fit inside a single control interval) and executes identically under any
  DFP/VSAM release including 3.8j - flagged here only because it is the one place in this library
  where record size is not a fixed value, which is easy to misread as a spanned-record cluster.
- **IMBED and REPLICATE** (CABVS010, CABVS030) parse and execute without error under MVS 3.8j
  IDCAMS - both are original-VSAM options, not later DFSMS additions - but they were already
  performance-neutral by the time of the estate's newest members, since FBA-emulated DASD under
  Hercules (and, in period, later 3390 controllers) does not benefit from the CI-splitting
  behaviour they influence. Their presence on the older members and absence on the newer ones is
  intentional era texture, not an error.
- All other syntax (`DEFINE CLUSTER`/`AIX`/`PATH`, `REPRO`, `BLDINDEX`, `LISTCAT`, `EXAMINE`,
  `PRINT`, `VERIFY`, `IF LASTCC`/`SET MAXCC`) is standard 1970s-vintage IDCAMS and would execute
  as written if the target volumes were mounted.

## Complexity 15 - file definition jobs undeclared elsewhere

Per brief, six members carry Complexity 15 (a VSAM cluster that no COBOL `SELECT`, no JCL `DD`,
and no copybook anywhere else in the estate references - programs and JCL simply assume the
dataset exists because a one-off allocation job, ostensibly from the 1994 conversion under
CR-4471, put it there):

- `CABVS020.jcl` - `TELCABS.CABS.CDR.CLEAN.ES`
- `CABVS040.jcl` - the alternate index/path over the Carrier master (`TELCABS.CABS.CARRIER.AIX` /
  `.PATH`) - confirmed: no COBOL program in `BATCH/` references `CR-ACNA`, so nothing could open
  this path
- `CABVS060.jcl` - `TELCABS.CABS.FACTOR`
- `CABVS090.jcl` - `TELCABS.CABS.BILLDTL.ES`
- `CABVS110.jcl` - `TELCABS.CABS.SUSPENSE.ES`
- `CABVS120.jcl` - `TELCABS.CABS.RUNCTL.ES`

Four of the six are confirmed orphans by direct search of `BATCH/` and `JCL/`: their dataset
names appear nowhere else in the built estate. Note that three of them (`CDR.CLEAN.ES`,
`SUSPENSE.ES`, `RUNCTL.ES`) share a base name with a live, heavily-referenced GDG chain
(`USAGE.CLEAN`, `SUSPENSE`, `RUNCTL` respectively) used by the INGEST/RATING/JURIS families as
`CLNOUT`/`SUSOUT`/`SUSPOUT`/`CTLOUT` - the naming similarity is deliberate estate texture; the
`.ES` qualifier is what actually keeps them distinct.

**Caveat on CABVS060 (`TELCABS.CABS.FACTOR`):** this member was built to the brief exactly as
specified, but a search of the already-built INGEST family turns up genuine, current references
to this same dataset name that the brief's orphan claim does not hold up against:
`BATCH/INGEST/CABING10.cbl` has `SELECT FCTRIN ASSIGN TO FCTRIN, ORGANIZATION IS INDEXED, ACCESS
MODE IS RANDOM, RECORD KEY IS FC-KEY` (a live COBOL SELECT), and both `JCL/CABIN10R.jcl` and
`JCL/CABING01.jcl` carry `//FCTRIN DD DSN=TELCABS.CABS.FACTOR,DISP=SHR` (a live JCL DD). The
bulk provisioning job `JCL/CABGDGDF.jcl` also already defines this exact cluster (`KEYS(14 0)
RECORDSIZE(80 80)`), separately from this member. In other words: TELCABS.CABS.FACTOR is not
actually an undeclared/assumed-to-exist dataset in the estate as built - it is the live rating
factor master, read by name every quarterly cycle. This member is retained per the explicit
build instruction and its comment box is internally consistent either way (it only claims a
1994 one-off origin, which does not conflict with the dataset later being wired into
production), but the Complexity 15 classification for CABVS060 specifically should be treated
as **superseded by the INGEST family build** and flagged for reconciliation against the
estate-wide complexity placement register rather than relied upon as a true CAST-detectable
orphan-file example. Use one of the other five instances (or the AIX/PATH) as the clean teaching
example of this complexity.

## Dataset attribute drift

No two DBA hands sized or shared these clusters the same way, and it shows:

- **SHAREOPTIONS.** `(2 3)` is the estate default (CABVS010, 020, 030, 050, 070, 090, and the
  bulk-provisioning copies in `CABGDGDF.jcl`). `(3 3)` appears on CABVS060 (Factor), CABVS100
  (Settlement) and CABVS120 (Control ESDS) with no comment explaining the departure from the
  default. CABVS080 (Bill header) is the estate's only `(4 3)` cluster, called out in its own
  comment box as deliberate (CICS inquiry region concurrent with the nightly batch writer) - but
  no equivalent comment exists on the `(3 3)` members, so an analyst cannot tell from the source
  alone whether those were considered decisions or copy-paste drift.
- **CARRIER/CIRCUIT/RATE/FACTOR defined twice.** `CABGDGDF.jcl` (bulk region provisioning) and
  the individual `CABVS030`/`CABVS050`/`CABVS060`/`CABVS070` members both carry `DEFINE CLUSTER`
  statements for the same four dataset names, with different `RECORDSIZE`, `CISZ`, `FREESPACE`
  and `CYLINDERS` values in most cases (most notably RATE: `CABGDGDF.jcl` still defines it at the
  pre-1996 fixed `RECORDSIZE(120 120)`, while CABVS050 sizes it to the full ODO band table at
  `RECORDSIZE(67 619)`). Neither job has been treated as the system of record.
- **Volser TELV09.** CABVS060's revision history claims the Factor table was moved off `TELV09`
  in 2009 ahead of a DASD controller swap. CABVS100 (Settlement master) still allocates against
  `TELV09` as of its latest revision, with a 2015 comment asserting the volser is "advisory only"
  because SMS ACS routines redirect the allocation - a claim CABVS100's JCL itself has no way to
  verify. Whether `TELV09` is live, decommissioned, or SMS-overridden depends on which member's
  comment an analyst reads.
- **IMBED/REPLICATE.** Present only on the two oldest clusters (CABVS010 CDR master, 1987;
  CABVS030 Carrier master, 1988). Every member defined or last touched after the mid-1990s
  omits them - not because anyone made a documented decision to drop them, but because they
  simply stopped being copied into newer DEFINE statements.

---

## Addendum — the `CABVDEF` allocation members

Six further IDCAMS members sit in this folder. They were cut by the earlier BILLCALC / REPORT
scope owner rather than as part of the CABVSnnn set above, and they are recorded here so the
folder manifest is complete. Every one of them defines a dataset that **no other member of the
estate declares** — no COBOL `SELECT`, no JCL `DD` allocation, no copybook. The programs that read
them simply assume they exist.

| File | Lines | Dataset(s) defined | Read by | Complexities carried | Runnable? |
|---|---:|---|---|---|---|
| `CABVDEF1.jcl` | 44 | `TELCABS.CABS.TAXRATE` | `CABBIL07` | **15** — allocation exists only here; maintained by the tax department through a File-AID panel | REFERENCE-ONLY |
| `CABVDEF2.jcl` | 38 | region / reference allocation | BILLCALC family | **15** | REFERENCE-ONLY |
| `CABVDEF3.jcl` | 46 | region / reference allocation | REPORT family | **15** | REFERENCE-ONLY |
| `CABVDEF4.jcl` | 40 | region / reference allocation | REPORT family | **15** | REFERENCE-ONLY |
| `CABVDEF5.jcl` | 58 | factor card file + document-control alternate index | `CABRPT04`; the AIX was built for the 1999 mailroom system replaced in 2006 | **15**, plus an alternate index retained past the retirement of its only consumer | REFERENCE-ONLY |
| `CABVDEF6.jcl` | 37 | region / reference allocation | REPORT family | **15** | REFERENCE-ONLY |

Taken together with `CABVS020`, the `CABVS040` AIX/PATH, `CABVS090`, `CABVS110` and `CABVS120`,
the estate carries **eleven** datasets whose only declaration anywhere is an IDCAMS job that has
not been run since the last DASD migration. A dependency graph built from programs and JCL alone
will not contain them at all; a graph built from programs, JCL and this folder will contain them
with no inbound edge.

Style note: these six members originally carried a comment line naming their own construct. That
line has been replaced with a period-authentic reference to `CABS-STD-058`, in line with the
estate-wide rule that no source comment names its own construct.
