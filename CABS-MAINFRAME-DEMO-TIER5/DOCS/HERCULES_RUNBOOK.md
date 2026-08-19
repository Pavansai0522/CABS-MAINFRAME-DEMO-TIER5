# CABS Tier 5 — Hercules Run Book

**Scope.** How to stand up a Hercules / TK4- (MVS 3.8j) system from nothing, install the sort
utility this estate depends on, allocate DASD, upload the source, compile it, load generated data,
run the daily and month-end streams, prove the run balanced, and recover it when it does not.

**Audience.** A competent engineer who has never used Hercules. Every command you need is here.

**Read `CONVENTIONS.md` at the estate root before you start.** The compiler target, the naming
rules and the mandatory control-record contract are defined there and are assumed throughout.

**A warning you should read before you spend a weekend on this.** Only part of this estate runs.
Section 11 is a complete and blunt statement of what does not and why. Read it first if your goal
is a working batch cycle rather than a reverse-engineering exercise — it will change how you plan
the work.

**Verification status of external facts.** Where this run book states a download location, a
version number or a product behaviour that comes from outside the estate, it is marked
**[verified]** with its source, or **[unverified]** where I could not confirm it. Nothing external
is asserted without one of those two marks. Everything about *this estate* is derived from the
source in this repository and is not marked.

---

## Table of contents

1. [Hercules and TK4- installation](#1-hercules-and-tk4--installation)
2. [OS/360 Sort/Merge installation](#2-os360-sortmerge-installation)
3. [DASD allocation](#3-dasd-allocation)
4. [Uploading the estate](#4-uploading-the-estate)
5. [Compiling](#5-compiling)
6. [Loading data](#6-loading-data)
7. [Job execution order](#7-job-execution-order)
8. [Expected elapsed times](#8-expected-elapsed-times)
9. [Verifying a clean run](#9-verifying-a-clean-run--the-control-record-chain)
10. [Restart and recovery](#10-restart-and-recovery)
11. [Known limitations](#11-known-limitations--what-will-not-run-and-why)

---

## 1. Hercules and TK4- installation

### 1.1 What you are installing

| Layer | What it is |
|---|---|
| **Hercules** | An emulator for the IBM System/370, ESA/390 and z/Architecture instruction sets. It emulates the CPU, channels, DASD, tape and terminals. It is not an operating system. |
| **TK4-** | A "turnkey" distribution: pre-built DASD volume images carrying a fully generated **MVS 3.8j** system, plus a Hercules configuration file that describes the machine those volumes are mounted on, plus compilers and utilities. MVS 3.8j is public-domain IBM software; TK4- is the community packaging of it. |
| **A TN3270 client** | MVS talks to terminals over the 3270 protocol. You need a client to log on. |

TK4- was assembled by Jürgen Winkelmann from the earlier Turnkey 3 system. **[verified — see
Sources]** The primary distribution page is <http://wotho.ethz.ch/tk4-/>, and the TK4- User's
Manual is at <https://wotho.ethz.ch/tk4-/MVS_TK4-_v1.00_Users_Manual.pdf>. **[verified]**

> **Note on newer distributions.** TK5 (Rob Prins, 2023) supersedes TK4- and, unlike TK4-, is built
> with modern DASD support already applied. Everything in this run book that concerns MVS itself
> applies to TK5 as well; the DASD discussion in §3 is where the two diverge, and TK5 makes that
> section easier. This run book targets TK4- because that is what the estate was specified against
> in `CONVENTIONS.md`. **[verified that TK5 exists and uses 3390 DASD; unverified whether every
> step below works unchanged on TK5 — I have not tested it.]**

### 1.2 Route A — native install

**Step 1. Get TK4-.**

```bash
mkdir -p ~/mainframe && cd ~/mainframe
# Download tk4-_v1.00_current.zip from http://wotho.ethz.ch/tk4-/
unzip tk4-_v1.00_current.zip -d tk4-
cd tk4-
```

**Step 2. Understand the directory layout.** After unpacking you will have roughly this:

| Directory | Contents |
|---|---|
| `conf/` | Hercules configuration. `tk4-.cnf` is the machine definition — CPU model, memory size, and every device address with the file backing it. **This is the file you edit to add DASD.** |
| `dasd/` | The emulated disk volume files (`.cckd` compressed CKD images). `mvsres.3350`, `mvs000.3350` … These are the system. |
| `prt/` | Printer output. JES2 SYSOUT written to a printer device lands here as flat files. |
| `pch/` | Card punch output. |
| `jcl/` | Sample job streams supplied with TK4-. |
| `log/` | Hercules and MVS logs. |
| `unattended/` | Scripts for automated start/stop. |
| `mvs-tk4-.sh` / `mvs.bat` | The start script. Run this. |

**Step 3. Start the emulator.**

```bash
./mvs-tk4-.sh          # Linux / macOS
mvs.bat                # Windows
```

Hercules starts, reads `conf/tk4-.cnf`, mounts the DASD, and **IPLs** (initial program load — the
mainframe word for boot) from the volume named in the config. You will see several hundred lines of
MVS initialisation scroll past. The system is up when the console goes quiet and you see the JES2
initialisation messages complete.

**Step 4. The operator console.** Two things are called "the console" and they are different:

| Console | What it is | How to reach it |
|---|---|---|
| **Hercules console** | Controls the *emulator*. Commands like `devlist`, `attach`, `detach`, `quit`. Nothing to do with MVS. | The terminal window Hercules is running in, or the web console on **port 8038** (`http://localhost:8038`). |
| **MVS operator console (MCS)** | Controls the *operating system*. This is where MVS writes `IEF403I JOB STARTED`, where you reply to WTORs, and where you enter `$D` JES2 commands. | TK4- presents it as a 3270 session, and also echoes it in the Hercules log. |

The single most useful skill here: **MVS console commands are prefixed differently depending on
where you type them.** From the Hercules console you must prefix an MVS command with `/`. So
`/$DA` from the Hercules console is the JES2 "display active jobs" command.

**Step 5. Log on to TSO.** Point a TN3270 client at **port 3270**:

```bash
c3270 localhost:3270          # Linux
x3270 localhost:3270          # Linux/macOS with X
# On Windows, Vista tn3270 or wc3270 both work
```

TK4- presents a logon screen. Enter:

```
LOGON HERC01
```

and when prompted, the password **`CUL8TR`**. **[verified — TK4- User's Manual]** `HERC01` is the
fully-authorised user; `HERC02` through `HERC04` exist with lower authority, and `IBMUSER` is the
system-programmer id. RAKF is the security manager.

You will land at the `READY` prompt (raw TSO) or in ISPF depending on the TK4- logon procedure you
picked. Type `ISPF` to get the panel interface if you are not already there.

**Step 6. Shut down cleanly. Do not just kill Hercules — you will corrupt a volume.**

From TSO at the `READY` prompt:

```
SHUTDOWN
LOGOFF
```

Wait about 30 seconds. TK4-'s automated shutdown procedure drains JES2, halts MVS, and quits
Hercules. **[verified — TK4- User's Manual]** Only `HERC01` and `HERC02` have the authority to do
this by default (it is gated on read access to the `DIAG8CMD` profile in the RAKF `FACILITY`
class). **[verified]**

If TSO is unavailable, the equivalent from the Hercules console is to enter the MVS shutdown
command with the `/` prefix, and only if that fails, `quit` at the Hercules console.

### 1.3 Route B — Docker

Faster to get running, and the right choice if you only want to try a SMOKE-profile run.

```bash
docker run -d \
  --name tk4-hercules \
  -p 3270:3270 \
  -p 8038:8038 \
  ghcr.io/skunklabz/tk4-hercules:latest
```

On Apple Silicon add `--platform linux/amd64`. **[verified — skunklabz/tk4-hercules README]**

| Port | Service |
|---|---|
| 3270 | TN3270 terminal |
| 8038 | Hercules web console |

Three named volumes persist state: `conf` (configuration), `dasd` (disk images), `log` (output).
**[verified]**

**The trade-off you are accepting.** You now have to get files in and out of a container, and you
have to edit `conf/tk4-.cnf` inside a Docker volume to add the DASD this estate needs (§3). For a
DAILY-profile run, the native install is less friction. For a first look, Docker is fine.

---

## 2. OS/360 Sort/Merge installation

### 2.1 Why this section exists

**TK4- does not ship a licensed DFSORT.** DFSORT is IBM program product code and is not in the
public domain. What is available is the **OS/360 Sort/Merge**, the MVT-era sort that predates
DFSORT entirely.

Tom Armstrong maintains an updated OS/360 MVT Sort/Merge, distributed as **CBT File #1036**, with
an Installation/Customization Guide and an Application Programming Guide. **[verified]** Jay
Moseley hosts the packaged distribution and the documentation:

- Distribution archive: <https://www.jaymoseley.com/hercules/downloads/archives/mvtsort.tgz>
- Installation guide: <https://www.jaymoseley.com/hercules/downloads/pdf/Tom.Armstrong.OS.360.Sort.Merge.v01.1.Installation.Guide.pdf>
- Application guide: <https://www.jaymoseley.com/hercules/downloads/pdf/Tom.Armstrong.OS.360.Sort.Merge.v01.1.Application.Guide.pdf>
- Instructions page: <https://www.jaymoseley.com/hercules/compilers/sort.htm>

**[all verified]**

> **[unverified] Whether TK4- ships a sort at all, and if so which one.** I found references
> suggesting a sort is present in TK4- but could not confirm from the TK4- manual which sort, at
> which level, or whether `MODS=` is supported in it. **Check `SYS2.LINKLIB` and `SYS1.SORTLIB` on
> your system before installing anything.** Jay Moseley's SYSCPK volume already carries Armstrong
> v1.01, so if you are running his build you may already have it. **[verified for SYSCPK; not for
> TK4-.]**

### 2.2 Installing it

The archive contains a HET tape image (`mvtsort.het`), an installation jobstream (`mvtsort.jcl`),
and a verification jobstream (`sorttest.jcl`). **[verified]**

```bash
cd ~/mainframe/tk4-
tar xvzf mvtsort.tgz
cp mvtsort.het tapes/
cp mvtsort.jcl sorttest.jcl jcl/
```

1. **Attach the tape.** At the Hercules console (or in `conf/tk4-.cnf`), attach `mvtsort.het` to a
   tape device address — `480` is the conventional first tape address on TK4-.
   ```
   attach 480 3420 tapes/mvtsort.het
   ```
2. **Submit `mvtsort.jcl`.** It runs IEBCOPY to restore two unloaded libraries from the tape:
   - `MVT.SORT.UNLOAD` → the sort load modules → **`SYS2.LINKLIB`**
   - `MVT.SORTLIB.UNLOAD` → the runtime library modules → **`SYS1.SORTLIB`**

   **[verified — target dataset names per Moseley's instructions. You may need to change them for
   your system; the instructions say so explicitly.]**
3. **Submit `sorttest.jcl`** to verify. It sorts an instream card deck and prints the result.
4. Confirm `SYS2.LINKLIB` is in your `LNKLST` (it is, on TK4-) so `EXEC PGM=SORT` resolves.

### 2.3 The part that determines how this estate behaves — read this carefully

OS/360 Sort/Merge is **MVT-era**. It is roughly fifteen years older than DFSORT and it does not
have DFSORT's control-statement vocabulary. Concretely:

| DFSORT / SyncSort statement | OS/360 Sort/Merge |
|---|---|
| `INCLUDE COND=` | **Not supported** |
| `OMIT COND=` | **Not supported** |
| `SUM FIELDS=` | **Not supported** |
| `INREC FIELDS=` | **Not supported** |
| `OUTREC FIELDS=` | **Not supported** |
| `OUTFIL` | **Not supported** |
| `JOINKEYS` | **Not supported** |
| `SORT FIELDS=COPY` | **Not supported** |
| `SORT FIELDS=(...)` | Supported |
| `MERGE FIELDS=(...)` | Supported |
| `RECORD TYPE=/LENGTH=` | Supported |
| `OPTION` (subset) | Supported |
| `MODS=(E15=…,E35=…)` | **Supported** — the exit mechanism is original to OS/360 Sort/Merge |

**This is the single most important architectural fact about this estate.** Because the sort had
no filtering, no summing and no reformatting, everything a modern shop would write as an `INCLUDE`
or a `SUM` had to be written as a **program**, hooked into the sort at exit point E15 (every record
on the way in) or E35 (every record on the way out).

That is why `SORTEXIT/` exists, and why `SORTEXIT/_README.md` describes that folder as *the single
most consequential blind spot in the estate*. Twenty-seven modules there carry production business rules
— which records enter a file, which are discarded, how usage is aggregated, how fractional cents
are treated — and **not one of them appears in any COBOL call graph, any `EXEC PGM=` statement or
any `CALL` literal anywhere in the estate.** They are reached only through the `MODS=` operand of a
sort control card, which is *data*, not code.

**Read `SORTEXIT/_README.md` in full before you attempt to run or reason about any sort step in
this estate.** It documents the register-1 parameter list, the return-code protocol (RC 04 = delete
the record; RC 08 = insert and re-enter with the same input), and the fact that WORKING-STORAGE
persists across entries because the module is loaded once for the whole sort step.

### 2.4 Work datasets — you must code SORTWK DDs

The Armstrong OS/360 Sort/Merge has an `IGNWKDD` customization option that controls dynamic work
dataset allocation. **[verified — the Installation Guide documents `IGNWKDD` and states that when
the dynamic allocation facility is not used, the job step must provide suitable `SORTWKdd` DD
statements in the input stream.]**

**Do not rely on dynamic allocation.** Code the DDs explicitly. Reasons:

1. Whether the installed build has dynamic allocation enabled is a *customization decision made at
   install time*, not a property of the product. You do not know what yours has until you check.
2. This estate's existing JCL already codes them. `JCL/CABING01.jcl` STEP030 and STEP070 each carry
   `SORTWK01` and `SORTWK02`. Every sort step in the estate follows that pattern.
3. **How many work datasets the sort spills across is a variable, not a detail.** Parts of the
   sort-exit layer behave differently when the merge runs over more than one `SORTWK`, and the
   number of work datasets is the only thing that decides whether that path is exercised at all.
   If you let the sort allocate its own work datasets you have given up control of it, and two
   runs of the same job are no longer comparable.

The classic build reserves six 2314 volumes named `SORTW1`–`SORTW6` under the esoteric unit name
`SORTDA`. **[verified — this is the layout on a standard Moseley-built MVS 3.8j.]** You may use
that, or point `SORTWKnn` at `UNIT=SYSDA` as this estate's JCL does. Up to **`SORTWK32`** is
addressable.

Sizing rule of thumb for this estate:

```
total SORTWK space  ≈  2 × the size of SORTIN
```

At the DAILY profile, `SORTIN` for the rating sort is roughly 100 MB, so provision about 200 MB
across the `SORTWK` DDs. Split it across at least two DDs on different volumes so the sort can
overlap I/O.

---

## 3. DASD allocation

### 3.1 The device-type problem — read before you plan volumes

**Original MVS 3.8j has no support for any DASD device type later than the 3350.** The 3375, 3380
and 3390 did not exist when MVS 3.8j shipped. **[verified — Jay Moseley,
`modernDASD.htm`.]**

Jim Morrison prepared usermods that may be applied to the MVS 3.8j distribution libraries to add
3375 / 3380 / 3390 support, and Rob Prins shipped updated versions of those usermods with Turnkey
5. **[verified.]** TK5 itself resides mostly on 3390 volumes. **[verified.]**

So you have three options, and you must pick one before you allocate anything:

| Option | Volume geometry | Per-volume capacity | Effort |
|---|---|---|---|
| **A — stock TK4-, 3350 only** | 555 cyl × 30 trk × 19,069 bytes/trk | **≈ 317 MB** | None. Works out of the box. |
| **B — TK4- + Morrison 3380 usermods** | 885 cyl × 15 trk × 47,476 bytes/trk | **≈ 630 MB** | Apply usermods to the DLIBs and re-IPL. Non-trivial. |
| **C — TK5** | 3390 | ≈ 946 MB (mod 1) | Use a different distribution. |

> The 630 MB figure for a 3380 is the single-density model (3380-A/D/E family). The 3380-K is
> roughly three times that. Hercules can emulate all of them; **MVS 3.8j is the constraint, not
> Hercules.** **[verified — Hercules supports 2311, 2314, 3330, 3340, 3350, 3375, 3380, 3390.]**

**Recommendation for this estate: Option A (3350) at SMOKE profile, Option B or C at DAILY.** A
DAILY cycle does not fit comfortably on 3350s once you add SORTWK, and you will spend more time
fighting B37 abends than analysing the estate.

### 3.2 How many volumes the DAILY profile needs

Derived from `GENERATORS/_README.md` §2 and the DD allocations in `JCL/`.

DAILY profile = **500,000 CDRs/day**, 2 shards, **≈ 100 MB of raw input per day**.

| Purpose | Content | Size | Volumes at 3380 (630 MB) | Volumes at 3350 (317 MB) |
|---|---|---|---|---|
| **Raw + GDG chain** | `USAGE.RAW` through `USAGE.CONSOL` — 15 GDG chains in `CABGDGDF.jcl`, each holding intermediate copies of the day's 100 MB, `LIMIT(35)` | Peak working set for one cycle ≈ 450 MB; the full 35-generation retention is far larger and you should reduce `LIMIT` for a test run | **1** (with `LIMIT` cut to 3) | **2** |
| **VSAM masters** | `CARRIER` (138×450), `RATE` (619×~1,200), `FACTOR` (76×~3,000), `CIRCUIT` (136× up to 60,000) | ≈ 15 MB | shares a volume | shares a volume |
| **Rated / bill detail** | `RATED`, `BILLDTL` (VB 1651), `SUMMARY`, `BILLHDR` | ≈ 250 MB | **1** | **1** |
| **SORTWK** | 2 × the largest `SORTIN` | **≈ 200 MB** | **1 dedicated** | **1 dedicated** |
| **Print / archive** | `PRTOUT`, `EDIOUT`, `MEDOUT`, `ARCHIVE.*` | ≈ 80 MB | shares | shares |
| **Source PDSs and load libraries** | 530 members, ~184,000 lines, plus load modules | ≈ 60 MB | shares | shares |

**Minimum workable DAILY configuration:**

- **3380 route:** 4 volumes — `TELV01` (masters + source), `TELV02` (GDG chain), `TELV03` (rated
  and bill detail), `SORTW1` (dedicated sort work).
- **3350 route:** 6 volumes — same split, with the GDG chain and the rated/detail sets each taking
  two.

**SMOKE profile** (50,000 CDRs/day, ≈ 10 MB/day) fits on **two 3350s** including sort work. Start
here.

### 3.3 Volume naming

Follow the estate's own convention so the VSAM jobs in `VSAM/` need no editing:

| Volser | Purpose | Referenced by |
|---|---|---|
| `TELV01` … `TELV12` | Application data volumes | The `VOLUMES()` operand throughout `VSAM/CABVS*.jcl` |
| `PUB001` | The volume named in the sample IDCAMS in `GENERATORS/_README.md` §9.3 | Generator documentation |
| `SORTW1` … `SORTW6` | Sort work volumes, esoteric unit `SORTDA` | Standard MVS 3.8j build **[verified]** |

**Note for honesty:** `VSAM/_MANIFEST.md` records that none of `TELV01`–`TELV12` exists on any real
or emulated DASD pool, which is one of the reasons the whole `VSAM/` folder is marked
REFERENCE-ONLY. You are creating them now. `VSAM/CABVS100.jcl` still allocates against `TELV09`,
which `VSAM/CABVS060.jcl`'s revision history claims was decommissioned in 2009 — that contradiction
is deliberate estate texture, not a bug to fix. Define `TELV09` anyway or the settlement master
define will fail.

### 3.4 Creating the volumes

Hercules ships `dasdinit`. Create compressed CKD images:

```bash
cd ~/mainframe/tk4-/dasd

# 3350 (works on stock TK4-)
dasdinit -z -linux telv01.3350 3350 TELV01
dasdinit -z -linux telv02.3350 3350 TELV02
dasdinit -z -linux sortw1.3350 3350 SORTW1

# 3380 (only after the Morrison usermods are applied to MVS)
dasdinit -z -linux telv01.3380 3380 TELV01
```

`-z` produces a compressed image that grows on demand — a 630 MB 3380 costs you a few megabytes of
host disk until you fill it.

Then add each to `conf/tk4-.cnf` at a free device address:

```
0250    3350    dasd/telv01.3350
0251    3350    dasd/telv02.3350
0252    3350    dasd/telv03.3350
0260    3350    dasd/sortw1.3350
```

Restart Hercules. Then, from MVS, initialise each volume with `IEHDASDR` (or `ICKDSF` if your
system has it) to write a VTOC and a volume label, and catalog the volumes so `UNIT=SYSDA`
allocation can reach them. The TK4- User's Manual documents the local procedure for adding a
volume to the system's unit-name groups. **[unverified — I have not confirmed the exact TK4-
procedure for extending `SYSDA`; consult the TK4- manual and Moseley's `addingDasdV8.htm`.]**

---

## 4. Uploading the estate

### 4.1 The three constraints

1. **Member names are 8 characters, uppercase, alphanumeric, first character alphabetic or
   national.** Every source file in this estate already conforms — `CABING01`, `CABSE35B`,
   `CABPBHDR`. The **file extension is not part of the member name**: `CABING01.cbl` becomes member
   `CABING01`. Strip extensions on the way in.
   - The one place to be careful is `CTC/`. Twelve directories each contain a member called
     `CABCTC01`. They cannot all live in one PDS. See §4.4.
2. **ASCII → EBCDIC.** Your source files are ASCII. MVS is EBCDIC (codepage **cp037**). Source must
   be translated. **Generated data must NOT be translated** — it is already EBCDIC. Getting this
   backwards is the most common failure in the whole exercise.
3. **Fixed-length 80-byte records.** PDS source members are `RECFM=FB,LRECL=80`. Lines shorter than
   80 are padded with blanks; anything past column 72 in COBOL is a sequence-number area and is
   ignored by the compiler. `BUILDER/_README.md` records that the generated tier was verified to
   have zero lines beyond column 72; the hand-authored families follow the same rule.

### 4.2 Suggested PDS layout

```
TELCABS.SOURCE.COBOL        FB 80    // all .cbl members
TELCABS.SOURCE.JCL          FB 80    // all .jcl job members
TELCABS.SOURCE.PROCLIB      FB 80    // all .prc / PROCS members
TELCABS.SOURCE.COPYLIB      FB 80    // all .cpy members
TELCABS.SOURCE.CTLCARDS     FB 80    // all .ctl sort control cards
TELCABS.SOURCE.ASM          FB 80    // HLASM (reference only)
```

Allocate them:

```jcl
//CABALLOC JOB (CABS,SETUP),'ALLOCATE SOURCE PDS',CLASS=A,MSGCLASS=X
//STEP010  EXEC PGM=IEFBR14
//COBOL    DD DSN=TELCABS.SOURCE.COBOL,DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,VOL=SER=TELV01,SPACE=(CYL,(60,20,300)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=6160)
//JCLLIB   DD DSN=TELCABS.SOURCE.JCL,DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,VOL=SER=TELV01,SPACE=(CYL,(10,5,200)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=6160)
//PROCLIB  DD DSN=TELCABS.SOURCE.PROCLIB,DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,VOL=SER=TELV01,SPACE=(CYL,(2,1,50)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=6160)
//COPYLIB  DD DSN=TELCABS.SOURCE.COPYLIB,DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,VOL=SER=TELV01,SPACE=(CYL,(2,1,50)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=6160)
//CTLCARDS DD DSN=TELCABS.SOURCE.CTLCARDS,DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,VOL=SER=TELV01,SPACE=(TRK,(10,5,30)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=6160)
//ASM      DD DSN=TELCABS.SOURCE.ASM,DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,VOL=SER=TELV01,SPACE=(TRK,(30,10,20)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=6160)
```

Directory-block sizing: **one directory block holds about five members.** `TELCABS.SOURCE.COBOL`
takes 248 members, so 300 blocks is generous but cheap insurance — running out of directory blocks
gives you an `IEC030I B14` abend on the *next* member you add, and the fix is a full reallocate and
copy.

Sizing note: 248 COBOL members averaging ~720 lines each at 80 bytes/line is roughly **14 MB**.
At 3350 geometry (19,069 bytes/track, 30 tracks/cylinder = 572 KB/cylinder) that is ~25 cylinders;
60 with secondary extents is comfortable.

### 4.3 Transferring — three routes

**Route 1 — FTP (simplest if your Hercules has the TCP/IP stack configured).**

```bash
ftp localhost 2121
> user HERC01
> quote site lrecl=80 recfm=fb blksize=6160
> ascii                                   # ← TRANSLATE. Source only.
> put BATCH/INGEST/CABING01.cbl 'TELCABS.SOURCE.COBOL(CABING01)'
```

**[unverified] Whether TK4- ships a working FTP server on port 2121 out of the box.** The
`GENERATORS/_README.md` §9.1 sample assumes one. Check before you plan around it; if it is not
there, use Route 2 or 3.

**Route 2 — card reader (best for source; this is the reliable route).**

Hercules emulates a 3505 card reader. Concatenate everything into one card deck with `//` job
control around it and feed it to the reader. TK4- reads ASCII card decks and translates them.

```bash
# Build one deck containing IEBUPDTE control statements for every member
python3 - <<'PY'
import pathlib, sys
out = open('cobol_deck.jcl', 'w')
out.write("""//CABLOAD  JOB (CABS,SETUP),'LOAD COBOL SOURCE',CLASS=A,MSGCLASS=X
//STEP010  EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD SYSOUT=*
//SYSUT2   DD DSN=TELCABS.SOURCE.COBOL,DISP=OLD
//SYSIN    DD DATA,DLM='@@'
""")
for p in sorted(pathlib.Path('BATCH').rglob('*.cbl')):
    name = p.stem.upper()[:8]
    out.write("./ ADD NAME=%s\n" % name)
    for line in p.read_text().splitlines():
        out.write(line[:80].ljust(80).rstrip() + "\n")
out.write("@@\n")
out.close()
PY

cp cobol_deck.jcl ~/mainframe/tk4-/jcl/
```

Then at the Hercules console:
```
devinit 00C jcl/cobol_deck.jcl ascii
```
Address `00C` is the conventional card reader. The `ascii` keyword tells Hercules to translate on
the way in — **this is where ASCII→EBCDIC happens for source.**

`IEBUPDTE` with `PARM=NEW` writes each `./ ADD NAME=` block as a member. Do the same for the JCL,
PROCLIB, COPYLIB and CTLCARDS libraries. Break the deck up if it exceeds a few thousand cards —
a failure halfway through a 168,000-line deck is painful.

**Route 3 — AWSTAPE (best for large volumes, and mandatory for the generated data in §6).**

Build a tape image on the host and read it with `IEBGENER` or `IEBCOPY`. `GENERATORS/_README.md`
§9.1 recommends this route for anything over a few megabytes.

### 4.4 The `CTC/` problem

Twelve site directories each contain `CABCTC01.cbl`, and nine also contain `CABCTC02.cbl`. In a
real estate each centre promotes its own copy into **its own** load library from **its own** source
library — `TELCABS.<centre>.CABS.LOADLIB`. There is no central source of record. Reproduce that:

```
TELCABS.SITE01.SOURCE.COBOL
TELCABS.SITE02.SOURCE.COBOL
…
TELCABS.SITE12.SOURCE.COBOL
```

Do **not** flatten them into one library with mangled names like `CABCTC1A`. The whole point of
`CTC/` — see `CTC/_MANIFEST.md` §5 — is that the estate contains no artefact identifying an
authoritative copy, and renaming them destroys exactly the property you are trying to study.

If you only intend to *run* rather than analyse, load `SITE01` only and ignore the rest.

---

## 5. Compiling

### 5.1 The compiler

TK4- ships the **OS/VS COBOL** compiler, program name **`IKFCBL00`** (often invoked through the
supplied `COBUC` / `COBUCL` / `COBUCLG` cataloged procedures). `CONVENTIONS.md` fixes the runnable
estate at **OS/VS COBOL, 1974 standard** precisely so it compiles here: no `EVALUATE`, no reference
modification, no `INITIALIZE`, no inline `PERFORM`, no scope terminators.

**[unverified] The exact TK4- procedure names and their PARM defaults.** Check `SYS1.PROCLIB` on
your system. The compile JCL below invokes `IKFCBL00` directly so it does not depend on a
particular procedure being present.

### 5.2 Compile and link JCL

Write this as a member `CABPCOBL` in `TELCABS.SOURCE.PROCLIB` and drive it from a job per program.

```jcl
//CABPCOBL PROC MEMBER=,LOADLIB='TELCABS.CABS.LOADLIB'
//*****************************************************************
//* CABPCOBL - OS/VS COBOL COMPILE AND LINK-EDIT                  *
//* SYMBOLIC MEMBER = THE SOURCE MEMBER AND THE LOAD MODULE NAME  *
//*****************************************************************
//COB      EXEC PGM=IKFCBL00,REGION=2048K,
//             PARM='LOAD,NODECK,SOURCE,DMAP,PMAP,XREF,APOST,SXREF'
//SYSPRINT DD  SYSOUT=*
//SYSPUNCH DD  DUMMY
//SYSIN    DD  DSN=TELCABS.SOURCE.COBOL(&MEMBER),DISP=SHR
//*
//* SYSLIB IS THE COPYBOOK CONCATENATION.  ORDER MATTERS - THE
//* FIRST LIBRARY HOLDING A MEMBER WINS.  CABSWRK NESTS CABSERR,
//* CABSDATE AND CABSCTL, SO ALL FOUR MUST BE REACHABLE HERE.
//*
//SYSLIB   DD  DSN=TELCABS.SOURCE.COPYLIB,DISP=SHR
//         DD  DSN=TELCABS.SOURCE.DCLGEN,DISP=SHR
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(3,2))
//SYSUT2   DD  UNIT=SYSDA,SPACE=(CYL,(3,2))
//SYSUT3   DD  UNIT=SYSDA,SPACE=(CYL,(3,2))
//SYSUT4   DD  UNIT=SYSDA,SPACE=(CYL,(3,2))
//SYSLIN   DD  DSN=&&OBJ,DISP=(MOD,PASS),UNIT=SYSDA,
//             SPACE=(TRK,(10,10)),DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//*
//LKED     EXEC PGM=IEWL,REGION=1024K,COND=(5,LT,COB),
//             PARM='LIST,MAP,XREF,LET,NCAL'
//SYSPRINT DD  SYSOUT=*
//SYSLIN   DD  DSN=&&OBJ,DISP=(OLD,DELETE)
//SYSLMOD  DD  DSN=&LOADLIB(&MEMBER),DISP=SHR
//SYSLIB   DD  DSN=SYS1.COBLIB,DISP=SHR
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(2,1))
```

Drive it:

```jcl
//CABCOMP1 JOB (CABS,BUILD),'COMPILE INGEST',CLASS=A,MSGCLASS=X
//JOBLIB   DD DSN=SYS2.LINKLIB,DISP=SHR
//STEP010  EXEC CABPCOBL,MEMBER=CABING01
//STEP020  EXEC CABPCOBL,MEMBER=CABING02
//STEP030  EXEC CABPCOBL,MEMBER=CABING03
//* … one step per member
```

### 5.3 The `NCAL` decision, and why it matters here

The link-edit PARM above carries **`NCAL`** (no automatic call). That suppresses the automatic
library call that would otherwise try to resolve every external reference at link time.

**You need `NCAL` because the two system interface modules this estate calls by literal are not
available on MVS 3.8j.** Every *estate* subprogram called by literal now has source: the twelve in
`BATCH/COMMON/` cover 519 of the call sites and the assembler modules in `HLASM/` cover the rest.
Static `CALL 'literal'` targets found in the COBOL:

| Called subprogram | Call sites | Source in estate? |
|---|---|---|
| `CABABEND` | 200 | ✅ `HLASM/CABABEND.asm` |
| `CABDTCNV` | 136 | ✅ `BATCH/COMMON/CABDTCNV.cbl`, **and** an alias entry point of `HLASM/CABDATCV.asm` — which one you get is a `STEPLIB` question, see §11.6 |
| `CABDATCV` | 68 | ✅ `HLASM/CABDATCV.asm` (and `HLASM/EMERG/CABDATCV.asm`) |
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
| `CBLTDLI` | 36 | ❌ IMS interface module — not on MVS 3.8j |
| `MQCONN`/`MQOPEN`/`MQPUT`/`MQGET`/`MQCMIT`/`MQCLOSE`/`MQDISC` | 15 | ❌ MQ stubs — not on MVS 3.8j |

**Consequence, stated plainly: the batch estate now links against real source, but the IMS and MQ
call sites still will not resolve on MVS 3.8j.** `CABPARMR` (the SYSIN parm-card reader) is called
from 141 sites and every program's `P1000-INIT` depends on it; `CABHASH` accumulates the four hash
totals the entire balancing framework is built on. Both are in `BATCH/COMMON/`, and both must be
compiled and link-edited into `TELCABS.COMMON.LOADLIB` **before** any job step is submitted — they
are subprograms, they appear in no JCL, and it is easy to miss them in a build list.

**The only stubs still needed are `CBLTDLI` (36 sites) and the seven MQ verbs (15 sites).** Those
sites are confined to `BATCH/CONTROL/`, which is reference-only on TK4- for other reasons as well
(§11.2), so in practice you exclude that family rather than stub it.

`NCAL` lets you get a *clean compile and link* immediately, with the unresolved references left as
weak external references, and defer the stubs. It also means the module will fail at run time with
an **S806** (module not found) the first time it makes an unresolved call — see §10.5.

### 5.4 The `SYSLIB` copybook concatenation

`CONVENTIONS.md`: *"Every program COPYs `CABSWRK` (which nests `CABSERR`, `CABSDATE`, `CABSCTL`)."*

That is complexity 08 — nested copybooks — and it means `SYSLIB` must reach all four members, not
just the one named in the `COPY` statement. `COPYBOOKS/` holds all 18 members in one library, so a
single `SYSLIB` DD covers the runnable estate.

Two members need care:

- `COPYBOOKS/CABSCOMM.cpy` is the **CICS COMMAREA** layout. It is only used by `ONLINE/`, which is
  reference-only. Leaving it in `SYSLIB` is harmless.
- `DB2/DCL*.cpy` are **DCLGEN** host structures for the DB2 precompiler. The three DB2-precompiled
  programs (`CABJUR10`, `CABSET12`, `CABSET13`) will not compile on TK4- at all — there is no DB2
  precompiler. The second `SYSLIB` DD above (`TELCABS.SOURCE.DCLGEN`) is there for completeness;
  you can omit it.

Also note `PLI/CABLGCNV.pli` references `%INCLUDE CABCDRLY`, and **that include member is not in
this repository** — `PLI/_MANIFEST.md` states this explicitly. `CABLGCNV` is not independently
compilable as delivered.

### 5.5 What a clean compile looks like, and what warnings to expect

**Return codes.**

| RC | Meaning | Action |
|---|---|---|
| **0** | Clean. No diagnostics. | Proceed. |
| **4** | Warnings (`W`-level). | **Expected across this estate. Read them, do not chase them to zero.** |
| **8** | Errors (`C`-level, conditional). Code was generated but the compiler made an assumption. | Investigate. |
| **12** | Severe (`E`-level). | Must fix. |
| **16** | Terminated (`D`-level). | Must fix. |

**A clean compile of this estate is RC=4, not RC=0.** Expect these warnings, which are properties
of the estate and are documented in `CONVENTIONS.md` and the family manifests as deliberate:

| Warning family | Cause in this estate | Do not "fix" |
|---|---|---|
| **Truncation on MOVE / high-order digits may be lost** | Complexity 21. 5-decimal accumulators MOVEd into 2-decimal header fields (`CABBIL09`, `CABBIL08`, `CABFMT05`, `CABRPT05`, `CABJUR05`). Also complexity 07 silent truncation: `CABRAT10 P5200` moves a VB 1647 group into `PIC X(500)`; `CABRPT06 P4000` moves the same group into `PIC X(400)`. | Correct — this is the seeded behaviour. |
| **REDEFINES length differs from redefined item** | The frozen copybooks disagree with themselves by design. `GENERATORS/_README.md` §8 lists them: `CD-VOICE-DETAIL` is 95 bytes over a 96-byte area, `CD-DATA-DETAIL` is 99, `CD-SPCL-DETAIL` is 97; `PC-BODY-A` is 114 over 132. | Correct. `COPYBOOKS/*` is frozen — report, do not change. |
| **Overlapping condition-name values (88-level)** | Complexity 06, placed deliberately. `CD-VOICE-MOU`/`CD-DATA-SVC` both true for `'03'`; `WS-SC-USAGE-SECTION` `'U1' THRU 'U3'` overlaps `WS-SC-CHARGE-SECTION` `'U2' THRU 'C4'`; `WS-TX-FEDERAL` `'FE' THRU 'FS'` overlaps `WS-TX-SURCHARGE` `'FS' THRU 'SU'`. | Correct. |
| **Paragraph is not referenced / cannot be reached** | Complexity 26 (dead code) and 27 (dormant feature). `P6200-DEFERRED-PAYMENT-PLAN`, `P5400-MICROFICHE-BLOCK`, `P6100-INTEREST-PROJECTION`, `P5000-INTEREST-CALC`, `P5000-PSU-BAND-CHECK`, `P6100-UNE-LOOP-REPRICE`, `P6600-NEGATIVE-MOU-ADJ`. | Correct — these are placed complexities. |
| **`GO TO` into / out of a `PERFORM` range** | Complexity 03, hidden error handlers. `P9990-DETAIL-FAILURE`, `P9980-PRINT-FAILURE`, `P9970-CONTROL-FAILURE`, `P9990-RATE-FAILURE`, `P9950-SUSPENSE-FAILURE`, `P9900-FATAL-EXIT`. | Correct. |
| **Unresolved external reference** at link | `CBLTDLI` / the MQ verbs, and any `BATCH/COMMON/` module you have not link-edited yet (§5.3). | Expected under `NCAL`. |

**What is NOT expected and means you have a real problem:**

- Any `E`-level or `D`-level diagnostic.
- `INVALID CHARACTER` or `ILLEGAL COLUMN` — you did not pad to 80 bytes, or a line ran past column
  72, or the ASCII→EBCDIC translation put something odd in.
- Anything naming `EVALUATE`, `END-IF`, `END-PERFORM`, `INITIALIZE`, or a reference-modification
  `(pos:len)` — that means you have fed the compiler an **Enterprise COBOL** member from the
  reference layer (`BATCH/CONTROL/`, `ONLINE/`, `SORTEXIT/`, `CABJUR10`, `CABSET12`, `CABSET13`).
  Those are not meant to compile here. See §11.

**Sanity checks before you accept a build:**

```
Programs to compile (runnable layer only):
  BATCH/INGEST     12
  BATCH/RATING     14
  BATCH/JURIS      10   (CABJUR10 excluded - DB2 precompiler)
  BATCH/SETTLE     11   (CABSET12, CABSET13 excluded - DB2 precompiler)
  BATCH/BILLCALC   12
  BATCH/FORMAT      9
  BATCH/REPORT      8
  BATCH/UTIL       95
  BATCH/COMMON     12   (called subprograms - in no JCL, easy to miss)
  CTC (SITE01)      2
  ----------------------
  TOTAL           185
```

Every one of the 173 job-step programs should produce a load module and a listing containing
`P0000-MAINLINE`, `P8000-CONTROL` and `COPY CABSWRK`. The twelve `BATCH/COMMON/` subprograms are
exempt from the `P8000-CONTROL` / `CTLOUT` rule under CABS-STD-041 — see
`BATCH/COMMON/_MANIFEST.md`. `BUILDER/_README.md` records that this was verified for all 95
generated members after the last builder run.

---

## 6. Loading data

### 6.1 Generate at SMOKE

Run the generators on the host, not the mainframe. Python 3.9+, no third-party dependencies.

```bash
cd GENERATORS

# COMP-3 correctness is load-bearing. Run the tests first.
python3 -m unittest test_gen_common -v      # 58 tests

# SMOKE: 50,000 CDRs/day, 1 shard, ~10 MB, ~7 s
python3 generate.py --profile SMOKE --days 1 --seed 20260815 \
        --outdir ../DATA/smoke --cycle-end 2024-09-30
```

Output lands in `../DATA/smoke/` under `REFERENCE/`, `USAGE/`, `SETTLEMENT/`, `CONTROL/`, plus
`run_manifest.json`.

**Everything the generator writes is already EBCDIC cp037 and already fixed-length.** No layout is
restated in Python — `gen_common.py` parses the frozen `.cpy` members directly. Transfer
**binary**.

The generator will report copybook diagnostics on every run and will not correct them. That is
correct behaviour; see `GENERATORS/_README.md` §8.

### 6.2 Transfer

**Binary. No translation. This is the opposite of §4.**

```bash
ftp localhost 2121
> quote site fix 200            # or the LRECL of the file being sent
> binary
> put TELCABS.CABS.USAGE.RAW.G0001V00.dat 'TELCABS.CABS.XFER.USAGE'
```

Or stage onto an AWSTAPE volume, which is the reliable route for anything over a few megabytes.

Blocking:

| LRECL | Recommended BLKSIZE | Why |
|---|---|---|
| 200 | **27800** | 139 × 200 — largest multiple under 32760 |
| 180 | **27540** | 153 × 180 — control and CMDS files |
| 300 | **27600** | 92 × 300 — suspense |
| 138 / 619 / 76 / 136 | n/a | VSAM; blocking is a cluster attribute |

### 6.3 Define the GDG bases — first

`JCL/CABGDGDF.jcl` already defines the full set. Submit it before anything else. It is safe to
resubmit; STEP010 ignores "already exists".

It defines **20 GDG bases**:

```
TELCABS.CABS.USAGE.RAW        TELCABS.CABS.USAGE.EDITED
TELCABS.CABS.USAGE.VALID      TELCABS.CABS.USAGE.DEDUP
TELCABS.CABS.USAGE.DATEVAL    TELCABS.CABS.USAGE.VOICE
TELCABS.CABS.USAGE.DATA       TELCABS.CABS.USAGE.SPCL
TELCABS.CABS.USAGE.CLEAN      TELCABS.CABS.USAGE.SUSPENSE
TELCABS.CABS.USAGE.CFWD       TELCABS.CABS.USAGE.ENRICH
TELCABS.CABS.USAGE.JURIS      TELCABS.CABS.USAGE.RECYCLE
TELCABS.CABS.USAGE.CONSOL     TELCABS.CABS.RATED
TELCABS.CABS.BILLDTL          TELCABS.CABS.SUMMARY
TELCABS.CABS.CONTROL          TELCABS.CABS.RATE.LOAD
```

and, in the same job, `DEFINE CLUSTER` for `CARRIER`, `CIRCUIT`, `RATE` and `FACTOR`.

> **A real conflict you will hit.** `CABGDGDF.jcl` and the individual `VSAM/CABVS030`, `CABVS050`,
> `CABVS060`, `CABVS070` members **both** define those same four clusters, with different
> `RECORDSIZE`, `CISZ`, `FREESPACE` and `CYLINDERS`. Most notably `CABGDGDF.jcl` still defines
> `RATE` at the pre-1996 fixed `RECORDSIZE(120 120)` while `CABVS050` sizes it to the full ODO band
> table at `RECORDSIZE(67 619)`. **Neither job has ever been treated as the system of record**
> (`VSAM/_MANIFEST.md`, "Dataset attribute drift").
>
> **For a working run, use the `CABVSnnn` sizes** — `RECORDSIZE(619 619)` fixed, per
> `GENERATORS/_README.md` §9.3. The generator writes every rate record at the maximum length with
> unused band slots binary zero and `RT-BAND-CNT` governing. A `(120 120)` cluster will reject
> every record.

For a test run, **reduce `LIMIT(35)` to `LIMIT(3)`** on the usage GDGs or you will fill your
volumes.

### 6.4 IDCAMS DEFINE sequence

Submit in this order. Sources: `VSAM/_MANIFEST.md` for the member list,
`GENERATORS/_README.md` §9.3 for the key and record sizes.

| Order | Member | Defines | Needed to run? |
|---|---|---|---|
| 1 | `JCL/CABGDGDF.jcl` | 20 GDG bases + 4 clusters | **Yes** |
| 2 | `VSAM/CABVS030.jcl` | `TELCABS.CABS.CARRIER` (+ DATA/INDEX) | **Yes** |
| 3 | `VSAM/CABVS050.jcl` | `TELCABS.CABS.RATE` (+ DATA/INDEX) | **Yes** |
| 4 | `VSAM/CABVS060.jcl` | `TELCABS.CABS.FACTOR` (+ DATA/INDEX) | **Yes** |
| 5 | `VSAM/CABVS070.jcl` | `TELCABS.CABS.CIRCUIT` (+ DATA/INDEX) | **Yes** |
| 6 | `VSAM/CABVS080.jcl` | `TELCABS.CABS.BILLHDR` — the only `SHAREOPTIONS(4 3)` cluster in the estate | **Yes** (BILLCALC) |
| 7 | `VSAM/CABVS100.jcl` | `TELCABS.SETL.MASTER` — still allocates on `TELV09` | Yes (SETTLE) |
| 8 | `VSAM/CABVS010.jcl` | `TELCABS.CABS.CDR.KSDS` | No — no live reader |
| 9 | `VSAM/CABVS040.jcl` | `CARRIER.AIX` + `CARRIER.PATH` | No — nothing opens the path |
| 10 | `VSAM/CABVS020/090/110/120` | `.ES` ESDS clusters | No — confirmed orphans |
| 11 | `VSAM/CABVDEF1.jcl` | `TELCABS.CABS.TAXRATE` | **Yes** — `CABBIL07` reads it |
| 12 | `VSAM/CABVDEF2.jcl` | `TELCABS.CABS.INVCTL` | **Yes** — `CABBIL12` reads it |
| 13 | `VSAM/CABVDEF3.jcl` | `TELCABS.CABS.HOLDRSN` | **Yes** — `CABBIL10` reads it |
| 14 | `VSAM/CABVDEF6.jcl` | `TELCABS.CABS.CLOSEMST` | **Yes** — `CABRPT08` reads it |
| 15 | `VSAM/CABVDEF4.jcl` | `TELCABS.CABS.BILLMSG` + `TELCABS.CABS.CONTRACT.MMX` | `BILLMSG` for `CABFMT09` (dormant), `CONTRACT.MMX` for `CABBIL08` |
| 16 | `VSAM/CABVDEF5.jcl` | `TELCABS.CABS.FACTOR.CARDS` + `TELCABS.CABS.PRTCTL.DOC.KSDS` | `FACTOR.CARDS` for `CABRPT04`. **The `PRTCTL.DOC` alternate index was built for the 1999 mailroom system replaced in 2006 and has had no consumer since.** |

**Items 11–15 are the trap.** They are the estate's complexity-15 instances: datasets that **no
COBOL `SELECT`, no JCL `DD` and no copybook anywhere else declares**. The programs simply assume
they exist. If you skip them, `CABBIL07`, `CABBIL10`, `CABBIL12` and `CABRPT08` will fail on OPEN
and nothing in the estate will have told you why.

Key and record sizes, from `GENERATORS/_README.md` §9.3:

| Cluster | `KEYS(len off)` | `RECORDSIZE` | Key fields |
|---|---|---|---|
| `TELCABS.CABS.CARRIER` | `KEYS(4 0)` | `(138 138)` | `CR-OCN` |
| `TELCABS.CABS.RATE` | `KEYS(18 0)` | `(619 619)` | `RT-TARIFF-CD` + `RT-RATE-ELEM` + `RT-JURIS-CD` + `RT-STATE-CD` + `RT-EFF-YYDDD` |
| `TELCABS.CABS.FACTOR` | `KEYS(14 0)` | `(76 76)` | `FC-OCN` + `FC-STATE-CD` + `FC-LATA` + `FC-EFF-YYDDD` |
| `TELCABS.CABS.CIRCUIT` | `KEYS(20 0)` | `(136 136)` | `CI-CIRCUIT-ID` |

### 6.5 REPRO loads

```jcl
//CABLOAD2 JOB (CABS,LOAD),'LOAD CARRIER MASTER',CLASS=A,MSGCLASS=X
//STEP010  EXEC PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//INFILE   DD  DSN=TELCABS.CABS.XFER.CARRIER,DISP=SHR
//OUTFILE  DD  DSN=TELCABS.CABS.CARRIER,DISP=SHR
//SYSIN    DD  *
  REPRO INFILE(INFILE) OUTFILE(OUTFILE)
/*
```

**`REPRO` into a KSDS requires the input to be in key sequence.** The generator writes the
carrier, rate and factor files sorted by key. **The circuit file is written in generation order and
must be sorted first:**

```jcl
//STEP015  EXEC PGM=SORT
//SORTIN   DD  DSN=TELCABS.CABS.XFER.CIRCUIT,DISP=SHR
//SORTOUT  DD  DSN=TELCABS.CABS.XFER.CIRCUIT.SORTED,DISP=(NEW,PASS),
//             UNIT=SYSDA,SPACE=(CYL,(10,5)),
//             DCB=(RECFM=FB,LRECL=136,BLKSIZE=27200)
//SORTWK01 DD  UNIT=SYSDA,SPACE=(CYL,(10,10))
//SORTWK02 DD  UNIT=SYSDA,SPACE=(CYL,(10,10))
//SYSIN    DD  *
  SORT FIELDS=(1,20,CH,A)
  RECORD TYPE=F,LENGTH=136
/*
```

That control card uses only `SORT FIELDS` and `RECORD` — both supported by OS/360 Sort/Merge.

Load the sequential files (raw usage, CMDS inbound, control) with `IEBGENER` into the GDG
generations, per the pattern in `GENERATORS/_README.md` §9.2.

### 6.6 BLDINDEX for the alternate index

`VSAM/CABVS040.jcl` defines `TELCABS.CABS.CARRIER.AIX` over `CR-ACNA` and a `PATH` over it.
`VSAM/CABVS130.jcl` builds it.

**`CABVS130.jcl` will not run as written.** `VSAM/_MANIFEST.md` records the reason: it codes
`WORKFILES(3)` as a `BLDINDEX` parameter, and real IDCAMS `BLDINDEX` has no such keyword — the
three `SORTWKnn`/`IDCUT` DDs alone drive the external sort. **Remove the `WORKFILES(3)` operand
before submitting.**

Corrected form:

```jcl
//STEP010  EXEC PGM=IDCAMS,REGION=2048K
//SYSPRINT DD  SYSOUT=*
//IDCUT1   DD  DSN=&&WORK1,DISP=(NEW,DELETE),UNIT=SYSDA,SPACE=(CYL,(5,2))
//IDCUT2   DD  DSN=&&WORK2,DISP=(NEW,DELETE),UNIT=SYSDA,SPACE=(CYL,(5,2))
//SYSIN    DD  *
  BLDINDEX INDATASET(TELCABS.CABS.CARRIER)  -
           OUTDATASET(TELCABS.CABS.CARRIER.AIX)
  LISTCAT ENTRIES(TELCABS.CABS.CARRIER.AIX) ALL
/*
```

**This index has no consumer.** `VSAM/_MANIFEST.md` confirms by direct search that **no COBOL
program in `BATCH/` references `CR-ACNA`**, so nothing could open the path. Building it is
optional; building it and then finding nothing uses it is the point of complexity 15.

Note also that `VSAM/CABVS140.jcl` carries `PARM='%%SKIPCT,%%LOADCT'` on `EXEC PGM=IDCAMS`. IDCAMS
does not read a step PARM that way. The values are substituted into the SYSIN `REPRO` cards by the
scheduler before submission; the step PARM is vestigial decoration. Harmless — leave it or delete
it, it does nothing either way.

### 6.7 Verify the load before you run anything

Reconcile against `run_manifest.json`, which records for every file: record count, byte count,
SHA-256, and the four hash totals (`hash_minutes`, `hash_amount`, `hash_seq`, `hash_ocn`) computed
in arrival order using the same accumulation rule the COBOL uses.

- **Record count** — `IDCAMS PRINT COUNT(0)` on a sequential file, or `LISTCAT ENT(…) ALL` and read
  `REC-TOTAL` on a cluster.
- **Hash totals** — submit `JCL/CABCTLRP.jcl`. It reads `TELCABS.CABS.CONTROL` and prints `CT-READ`
  and the four hash totals.

They must match. Two diagnostic rules from `GENERATORS/_README.md` §9.5 that will save you an
afternoon:

| Symptom | Almost certainly |
|---|---|
| `hash_ocn` mismatch alone | The transfer translated the **alphanumeric** OCNs. ~28% of the 450 OCNs are alphanumeric on purpose, because `CABHASH` treats the two forms differently. |
| `hash_minutes` mismatch alone | The transfer translated the **COMP-3** fields. |
| Both | The transfer was not binary. |

---

## 7. Job execution order

Derived from the actual JCL and the family manifests. Job names are members in
`TELCABS.SOURCE.JCL`.

### 7.0 Before you submit anything — the symbolics problem

**No job member in this estate will submit unedited.** `JCL/CABING01.jcl` says so in its own
comment box:

> `&CYCLE, &BILLPER, &RUNID, &MODE AND &RERUN ARE PLAIN-TEXT TOKENS IN THIS SOURCE MEMBER. THE
> SCHEDULER PACKAGE REPLACES THEM WITH LITERAL VALUES BEFORE THE JOB IS HANDED TO THE INTERNAL
> READER — THIS TEAM USES A SINGLE AMPERSAND, THE SETL TEAM'S JCL USES A DOUBLE PERCENT (%%) FOR
> THE SAME PURPOSE. DO NOT SUBMIT THIS MEMBER UNEDITED.`

Two token conventions, two teams, one estate:

| Convention | Used by | Example |
|---|---|---|
| `&NAME` | CABS ingest / rating / billing / format / report | `&CYCLE`, `&BILLPER`, `&RUNID`, `&MODE`, `&RERUN` |
| `%%NAME` | SETL settlement, jurisdiction DB2 posting, month-end close | `%%CYCLDT`, `%%BILLPR`, `%%RUNID`, `%%RSTEP`, `%%CLSPER`, `%%EXPPRC` |

`REXX/CABGENJC.exec` is the exec that did the substitution in period: it reads
`TELCABS.CABS.CONTROL.PARMTBL`, selects rows whose active switch is `Y` and whose effective window
covers today, builds `// SET` statements, writes a throwaway member and submits it — **then deletes
the member**. See `REXX/_MANIFEST.md`. That exec is reference-only (MVS 3.8j predates the TSO/E
REXX level it uses), so **you must do the substitution yourself.**

Practical approach: a host-side `sed` pass before upload.

```bash
CYCLE=24274; BILLPER=202409; RUNID=R240930001; MODE=LIVE
sed -e "s/&CYCLE/$CYCLE/g"   -e "s/%%CYCLDT/$CYCLE/g" \
    -e "s/&BILLPER/$BILLPER/g" -e "s/%%BILLPR/$BILLPER/g" \
    -e "s/&RUNID/$RUNID/g"  -e "s/%%RUNID/$RUNID/g" \
    -e "s/&MODE/$MODE/g" \
    JCL/CABING01.jcl > /tmp/CABING01.jcl
```

Symbolics with **no default anywhere in the estate** — these are complexity 09 placements and you
must supply a value or the job is a JCL error:

| Symbolic | Job | What it is |
|---|---|---|
| `%%RSTEP` | `CABS7000` | Restart step on the job card. A normal run substitutes `STEP010`. |
| `RSTFRM` / `RSTWIN` | `CABJ1600` | Restatement window start and width |
| `CAPOVR` | `CABS2500` | ISP cap override. **Zero means "use the agreement cap"** — not "no cap". |
| `EXCHDT` | `CABS2600` | CMDS exchange date |
| `MAXELM` | `CABS4100` | Maximum rate elements per bill detail record |
| `FORCSW`, `OCNFRM`, `OCNTHR` | `CABS4000` | Bill trigger force switch and OCN range |
| `PREFIX` | `CABS4600` | Invoice number prefix |
| `EXPPRC` | `CABS6000` | Expected process count — differs by cycle type |
| `CLSPER`, `LEDGCO`, `SIGNOF` | `CABS6400` | Close period, ledger company, sign-off initials |
| `LINPGE` | `CABS5000` | Lines per page |
| `COMPNY` | `CABS5000` | Company name on the bill heading |
| `EDIVER`, `SENDID` | `CABS5300` | EDI 811 version and sender id |

### 7.1 One-time setup

| # | Job | Produces |
|---|---|---|
| 1 | `JCL/CABGDGDF.jcl` | 20 GDG bases + the four VSAM masters |
| 2 | `VSAM/CABVS030/050/060/070/080/100.jcl` | The remaining clusters (see §6.4 for the sizing conflict) |
| 3 | `VSAM/CABVDEF1/2/3/6.jcl` | `TAXRATE`, `INVCTL`, `HOLDRSN`, `CLOSEMST` — required, declared nowhere else |
| 4 | Load jobs (§6.5) | Populated masters |
| 5 | `VSAM/CABVS130.jcl` (corrected) | Carrier AIX — optional, no consumer |

### 7.2 The daily stream

**Job 1 — `CABING01` (ingest). 14 steps.**

| Step | Runs | Reads | Writes |
|---|---|---|---|
| `STEP010` | `IEFBR14` | — | Scratches `TELCABS.CABS.WORK.SCRATCH` |
| `STEP020` | **`CABING01`** raw EMI edit / format validation | `RAWIN` = `USAGE.RAW(0)`, `PARMIN` instream | `EDTOUT` = `USAGE.EDITED(+1)`, `SUSOUT`, `CTLOUT` |
| `STEP030` | `SORT`, card `CABSRT02` | `USAGE.EDITED(0)` | `USAGE.EDITED(+1)` — OCN/BAN/SEQ, `OMIT` drops fatal edit-status 6–9 |
| `STEP040` | **`CABING02`** OCN/BAN validation | `EDTIN`, `CARRMST` | `VLDOUT` = `USAGE.VALID(+1)` |
| `STEP050` | **`CABING03`** duplicate / out-of-sequence | `VLDIN` | `DUPOUT` = `USAGE.DEDUP(+1)` |
| `STEP060` | **`CABING04`** YYDDD connect/disconnect validation | `DUPIN` | `DTVOUT` = `USAGE.DATEVAL(+1)` |
| `STEP070` | `SORT`, card `CABSRT03` | `USAGE.DATEVAL(0)` | `USAGE.DATEVAL(+1)` — **`SUM FIELDS=(120,7,PD)`. The estate's only dedup-by-summing rule, and it lives only in the control card. No `CABING` program knows about it.** |
| `STEP080` | **`CABING05`** usage type split | `DTVIN` | `VOCOUT`, `DATOUT` (**VB 204**), `SPCOUT`, `CLNOUT`, `SUSOUT`. |
| `STEP090` | **`CABING06`** circuit / trunk group resolution | `CLNIN`, `CIRCMST` | `ENROUT` = `USAGE.ENRICH(+1)`. **STEPLIB has `LOADLIB.TEST` first** — a soak from change CAB-2019-0447. |
| `STEP100` | **`CABING10`** jurisdictional pre-edit | `ENRIN`, `FCTRIN`, `CARRMST` | `JUROUT` = `USAGE.JURIS(+1)` |
| `STEP110` | **`CABING08`** cycle-boundary carry-forward | `CLNIN`, `CFWIN` = `USAGE.CFWD(0)` | `CFWOUT` = `USAGE.CFWD(+1)` **`DISP=MOD`** — physical order is the cycle sequence |
| `STEP120` | **`CABING09`** daily consolidation | `VOCIN`, `DATIN`, `SPCIN` | `CONOUT` = `USAGE.CONSOL(+1)`. **Merges the STEP080 streams straight — it does not see the STEP090/STEP100 enrichment.** |
| `STEP130` | **`CABING07`** suspense consolidation | `SUSIN` | `SUSOUT`, `AUDLOG` (`DISP=MOD`). **No `COND` — always runs, even after a STEP020 failure.** |
| `STEP140` | `CABPCTLR` control report | every `CTLOUT` from this run | Printed balancing summary. **Always runs.** |

Every step from STEP030 onward carries `COND=(4,LT,STEP020)` except STEP130 and STEP140. A failure
in the edit step bypasses everything downstream except suspense consolidation and the control
report.

`CABING11` (suspense recycle) and `CABING12` (EMI 42-XX bridge) are **not in this stream**.
`CABING11` runs standalone via `CABIN11R.jcl`. `CABING12` is **referenced by no JCL member anywhere
in the estate** — an orphaned program, complexity 26, recorded in `BATCH/INGEST/_MANIFEST.md`.

**Job 2 — `CABRAT01` (rating). 14 steps.**

| Step | Runs | Note |
|---|---|---|
| `STEP010` | **`CABRAT01`** rate table load | `RATEMST` → `RTBLOUT`. Builds the flattened `R2`/`R3` table with ODO. |
| `STEP020` | `SORT`, card **`CABSRT04`** | **`MODS=(E15=(CABSE15A,4096),E35=(CABSE35A,4096))`.** The sort key positions only line up *after* `CABSE15A` has reformatted the record. `CABSE35A` stamps the 12-byte rating control prefix into bytes 189–200 that `CABSE35B` later depends on. |
| `STEP030` | **`CABRAT02`** dispatcher | **Dynamic calls ×4** — target built from `R2-EN-MODULE-SFX` in the loaded rate table. Statically unresolvable. |
| `STEP040` | **`CABRAT03`** switched access, five rate elements | 4,290 lines. The single most expensive program in the estate. |
| `STEP050` | **`CABRAT04`** special access, proration | |
| `STEP060` | **`CABRAT05`** UNE rating | |
| `STEP070` | **`CABRAT11`** reciprocal compensation | |
| `STEP080` | **`CABRAT06`** banded / volume rate selection | Band selection walks the `R3` pool with an `OCCURS DEPENDING ON`. |
| `STEP090` | **`CABRAT07`** minimum / maximum charge | |
| `STEP100` | **`CABRAT08`** rate override handler | `OVRIN` = the override deck |
| `STEP110` | **`CABRAT13`** operator services | **Dormant.** Behind `R1-OPR-SVC-SW = 'N'`, tariff withdrawn 1998. Runs, does nothing. |
| `STEP120` | **`CABRAT12`** retry + positional audit trail | `AUDLOG` extend; attempt count derived from **physical position**, not a stored field. |
| `STEP130` | **`CABRAT10`** bill detail line construction | **Complexity 07 silent truncation** — `P5200` moves a VB 1647 group into `PIC X(500)`. |
| `STEP140` | **`CABRAT09`** rating summary | Internal `SORT` with `INPUT`/`OUTPUT PROCEDURE`; the eligibility and suppression rules live in the procedures. |

`CABRAT14` (rate table audit / effective-date sweep) is **not part of either main stream** — it is
a period-end utility, run via `CABRT14R.jcl`.

**Job 3 — `CABCTLRP`.** Run after both `CABING01` and `CABRAT01` complete. Selects `CTLOUT` records
where `CT-BAL-IND` is not `'B'` and prints them for the morning operations review. **A clean night
produces an empty report.**

### 7.3 Quarterly — the jurisdiction stream

Runs when a new PIU/PLU filing arrives. Not part of the daily cycle.

| Order | Job | Produces |
|---|---|---|
| 1 | `CABJ1000` | Quarterly factor load — `CABJUR01` adds/replaces the factor on the KSDS and carries the superseded one into `FC-PRIOR-PIU` |
| 2 | `CABJ1100` | Factor validation and dispute quarantine (`CABJUR02`) |
| 3 | `CABJ1200` | Jurisdiction determination (`CABJUR03`) — **dynamic call `CABJX` + state suffix**; STEPLIB has `CABJXCAL` in two of five libraries |
| 4 | `CABJ1300` | PIU application (`CABJUR04`) — interstate MOU = MOU × PIU / 100, intrastate by subtraction |
| 5 | `CABJ1400` | PLU application (`CABJUR05`) — **truncates what `CABJUR04` rounded** |
| 6 | `CABJ1500` | Default factor fallback (`CABJUR06`) |
| 7 | `CABJ1600` | **Retroactive factor restatement (`CABJUR07`, 4,075 lines).** Reads GDG `(-1)` and `(-3)`. |
| 7a | `CABJ1650` | Restatement *simulation* — same PROC as `CABJ1600` with `ADJOUT` to `DUMMY` |
| 8 | `CABJ1700` | Restatement reversal (`CABJUR08`) |
| 9 | `CABJ1800` | Jurisdictional revenue summary (`CABJUR09`) — **reads `TELCABS.SETL.SETTLE.MASTER`, owned by SETL** |
| 10 | `CABJ1900` | Restatement posting to DB2 (`CABJUR10`) — **reference-only on TK4-** |
| 11 | `CABJ2000` | Jurisdiction exception and audit report (`CABJUR11`) |

### 7.4 Monthly — the settlement stream (application `SETL`)

| Order | Job | Produces |
|---|---|---|
| 1 | `CABS2100` | Meet-point circuit extract (`CABSET02`) — reads `TELCABS.CABS.CDR.PLU` |
| 2 | `CABS2200` | **Meet-point settlement (`CABSET01`, 2,131 lines)** — reads `TELCABS.CABS.BILLDTL`. |
| 3 | `CABS2300` | Meet-point percentage validation and variance (`CABSET03`) |
| 4 | `CABS2400` | Reciprocal comp MOU aggregation (`CABSET04`) — the sort here uses `CABSE15B`, **the only settlement-eligibility test in the path** |
| 5 | `CABS2450` | Wireless termination (`CABSET06`) — **dormant since July 2011.** Runs monthly, produces an empty file. |
| 6 | `CABS2500` | **Reciprocal comp with ISP cap (`CABSET05`, 2,057 lines).** |
| 7 | `CABS2600` | CMDS/RAO outbound (`CABSET07`) — sort uses `CABSE35D` for the industry header/trailer **that `CABSET07` never sees** |
| 8 | `CABS2700` | CMDS/RAO inbound (`CABSET08`) |
| 9 | `CABS2800` | Settlement netting (`CABSET09`) — reads `TELCABS.CABS.CARRIER` |
| 10 | `CABS2900` | Dispute handler (`CABSET10`) — **STEPLIB puts `SETL.LOADLIB.EMERG` first, so this binds the 1998 `CABDATCV` with pivot 68** |
| 11 | `CABS3000` | Settlement statements (`CABSET11`) — reads `TELCABS.CABS.BILLHDR` |
| 12 | `CABS3100` | Period close (`CABSET12`) — **DB2, reference-only** |
| 13 | `CABS3200` | Settlement posting (`CABSET13`) — **DB2, reference-only.** Also binds the 1998 `CABDATCV`. |

### 7.5 Month-end close — `CABS7000` orchestrates everything

`CABS7000` is the stream that chains the whole month end together. It submits jobs through the
internal reader, runs the balancing report **in line** so its return code governs the rest, then
runs the close and the archive.

| Step | Action | Submits / runs |
|---|---|---|
| `STEP010` | `IEBGENER` → `INTRDR` | **Bill calculation, 12 jobs.** In this order: `CABS4000`, `CABS4100`, `CABS4150`, `CABS4200`, `CABS4250`, `CABS4300`, **`CABS4450`, `CABS4400`, `CABS4350`**, `CABS4500`, `CABS4550`, `CABS4600` |
| `STEP020` | `IEFBR14`, `COND=(0,NE)` | Wait — the scheduler holds this until `TELCABS.CABS.BILLHDR.FIN(0)` is cataloged |
| `STEP030` | `IEBGENER` → `INTRDR`, `COND=(4,LT)` | **Format and print, 6 jobs:** `CABS5000`, `CABS5100`, `CABS5200`, `CABS5300`, `CABS5400`, `CABS5500` |
| `STEP040` | `EXEC CABPRPTB` **in line** | **`CABRPT01` daily balancing report.** `CTLIN` is a four-generation concatenation `(0)(-1)(-2)(-3)`. `HALT=Y`, `CHAIN=Y`. **Sets RC 12 when the cycle does not balance, which stops the close and the print release.** |
| `STEP050` | `IEBGENER` → `INTRDR`, `COND=(4,LT)` | Reporting: `CABS6100`, `CABS6200`, `CABS6300` |
| `STEP060` | `EXEC CABPCLOS` **in line**, `COND=(4,LT)` | **`CABRPT08` month-end close and ledger posting.** `CTLIN` is a **five**-generation concatenation. Will not close the period if any process in the month was out of balance or any invoice is still held. |
| `STEP070` | `IDCAMS`, `COND=(4,LT)` | Archive: `REPRO` `BILLHDR.FIN(0)` → `ARCHIVE.BILLHDR(+1)`, then `BILLDTL.SEQ(0)` → `ARCHIVE.BILLDTL(+1)` |

> **Note the order in STEP010.** `CABS4450` (invoice header creation) is submitted **before**
> `CABS4400` (tax) and `CABS4350` (minimum/maximum). Read `BATCH/BILLCALC/_MANIFEST.md`: the
> logical data flow is trigger → detail → sequence → balance/payments → adjustments → settlement
> netting → **tax** → **min/max** → header → audit → balance → number. The submission order in
> `CABS7000` does not match that. Whether the internal reader's submission order actually
> determines execution order depends on JES2 job class and initiator configuration, which is
> exactly the kind of dependency that is invisible in the source. **Treat this as a real ordering
> hazard for a first run** — if the header job runs before tax, `BH-TAX-AMT` is zero.

`CABS6000` (the standalone daily balancing report job) is **not** in the `CABS7000` chain —
`STEP040` runs `CABRPT01` directly through `CABPRPTB` instead.

### 7.6 Ad-hoc — the utility tier

54 jobs, `CABU7000` through `CABU7530`, running 95 programs across four archetypes (rate-table
maintenance, dataset extract, cross-reference report, file conversion). Full job/step/program
mapping in `BUILDER/_MANIFEST.md`. **None of them is on the critical path.** They carry volume, not
complexity — that is the explicit design intent recorded in `BUILDER/_README.md`.

---

## 8. Expected elapsed times

> **These are estimates. No benchmark has been run on any target host.** They are derived from the
> generator's measured host-side throughput (`GENERATORS/_README.md` §2), the step counts in the
> JCL, and the general observation that Hercules on modern hardware executes System/370 code
> roughly one to two orders of magnitude faster than the 1980s hardware MVS 3.8j was written for,
> while I/O to emulated CKD on a host SSD is fast but not free. **Benchmark before you commit to a
> schedule.**

| Profile | CDRs/day | Input size | `CABING01` | `CABRAT01` | Daily total | Month-end close |
|---|---:|---:|---|---|---|---|
| **SMOKE** | 50,000 | 10 MB | ~1–2 min | ~2–3 min | **~5 min** | ~10–15 min |
| **DAILY** | 500,000 | 100 MB | ~6–10 min | ~8–14 min | **~15–25 min** | ~45–90 min |
| **STRESS** | 2,000,000 | 400 MB | ~25–40 min | ~35–60 min | **~1–2 h** | ~4–6 h |
| **TARGET** | 100,000,000 | 20 GB | — | — | **Do not attempt** | — |

`GENERATORS/_README.md` §2 is explicit: *"`TARGET` is intended for the cloud target side only. The
legacy estate under Hercules is not sized for it; use `STRESS` for anything that has to run on MVS
3.8j."*

**What actually costs the time:**

1. **The sorts.** Six sort steps across the two daily jobs, each moving the full day's volume.
   Under-provisioned `SORTWK` is the number one cause of a run taking three times as long as it
   should.
2. **VSAM random reads.** `CABING02` reads `CARRMST` per record; `CABING06` reads `CIRCMST` per
   record; `CABING10` reads `FCTRMST` per record. The in-storage OCN cache added to `CABING02` in
   1998 (V1.30) is why STEP030 sorts by OCN first.
3. **`CABRAT03`.** 4,290 lines, five rate elements per call, with an internal `SORT` inside it. It
   is the single most expensive program in the estate.
4. **`CABJUR07`.** 4,075 lines. Only runs quarterly, but when it does it reads prior-period priced
   usage *and* the billed detail history and reprices under two factors.
5. **The `REGION` sizes in the JCL.** Most steps carry `REGION=4M`; `CABING01`'s job card says
   `REGION=6M`; `CABS7000` says `REGION=0M`. MVS 3.8j has a 16 MB address space and a real
   below-the-line constraint. If you raise `REGION` past what the initiator allows you get an
   allocation failure, not better performance.

**One profile-specific caveat that is not about speed.** `GENERATORS/_README.md` §2:

> *"`STRESS` is the smallest profile that forces the summary sort to spill across more than one
> `SORTWK`. Some behaviour in the sort-exit layer only appears once that happens."*

A `SMOKE` or `DAILY` run therefore exercises a strictly smaller part of the estate than the
profile name suggests, and the harness will correctly report the untouched behaviour as not
observed. Size the profile against what you intend to exercise.

---

## 9. Verifying a clean run — the control record chain

### 9.1 The contract

`CONVENTIONS.md` makes this non-negotiable:

> **`P8000-CONTROL` is NOT optional.** Every program writes a `CABS-CONTROL-RECORD` to DD `CTLOUT`
> populating `CT-READ` / `CT-WRITTEN` / `CT-REJECTED` / `CT-SUMMARISED` / `CT-CARRIED-FWD` and the
> four hash totals, then evaluates the balancing equation and sets `CT-BAL-IND`.

The record is `COPYBOOKS/CABSCTL.cpy`, **LRECL 180 FB**:

```cobol
       01  CABS-CONTROL-RECORD.
           05  CT-KEY.
               10  CT-RUN-ID               PIC X(12).
               10  CT-PROCESS-ID           PIC X(08).
               10  CT-STEP-SEQ             PIC 9(03).
           05  CT-RUN-CONTEXT.
               10  CT-CYCLE-YYDDD          PIC 9(05).
               10  CT-BILL-PERIOD          PIC 9(06).
               10  CT-RERUN-NBR            PIC 9(02).
               10  CT-JOBNAME              PIC X(08).
               10  CT-STEPNAME             PIC X(08).
           05  CT-COUNTS.
               10  CT-READ                 PIC S9(11) COMP-3.
               10  CT-WRITTEN              PIC S9(11) COMP-3.
               10  CT-REJECTED             PIC S9(11) COMP-3.
               10  CT-SUMMARISED           PIC S9(11) COMP-3.
               10  CT-CARRIED-FWD          PIC S9(11) COMP-3.
           05  CT-HASH-TOTALS.
               10  CT-HASH-MINUTES         PIC S9(15)V9(02) COMP-3.
               10  CT-HASH-AMOUNT          PIC S9(13)V9(05) COMP-3.
               10  CT-HASH-SEQ             PIC S9(17)       COMP-3.
               10  CT-HASH-OCN             PIC S9(15)       COMP-3.
           05  CT-STATUS.
               10  CT-BAL-IND              PIC X(01).
                   88  CT-IN-BALANCE       VALUE 'B'.
                   88  CT-OUT-OF-BAL       VALUE 'O'.
                   88  CT-NOT-CHECKED      VALUE ' '.
               10  CT-RC                   PIC 9(04).
               10  CT-ABEND-CD             PIC X(04).
               10  CT-RESTART-KEY          PIC X(26).
           05  CT-FILLER                   PIC X(27).
```

### 9.2 The balancing equation

```
CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED + CT-CARRIED-FWD
```

This must hold **for every process**. Where it does not, the program must set `CT-OUT-OF-BAL`
(`CT-BAL-IND = 'O'`).

### 9.3 Three levels of proof

**Level 1 — per-process.** Every `CTLOUT` record must carry `CT-BAL-IND = 'B'`.

```bash
# Host-side check on generated control records
cd HARNESS && python3 -c "
from canonical import canonicalise
# see canonical.py --help; or simply submit CABCTLRP.jcl on the mainframe
"
```

On the mainframe, `JCL/CABCTLRP.jcl` does exactly this: it sorts the control file with card
`CABSRT08` (`INCLUDE COND=(40,1,CH,NE,C'B')`) and prints everything that is not in balance. **A
clean night produces an empty report.**

**Level 2 — chain continuity.** What one process wrote is what the next one read.
`CABRPT01` walks a **63-entry process chain table** proving `CT-WRITTEN` on the upstream process
equals `CT-READ` on the downstream one, and checks hash continuity, step sequence and missing
processes.

`HARNESS/contracts/compare_contract.json` declares the same chain as a list of edges:

```json
{"from": "CABING05", "to": "CABING06", "source_field": "CT-WRITTEN", "target_field": "CT-READ",
 "note": "CABING05 writes the clean stream. If the count on either side of this edge is
          inflated the edge breaks, which is the point of declaring it."}
```

**Level 3 — the estate-wide verdict.** `CABRPT01` (2,027 lines, `BATCH/REPORT/`) reads every
control record in the cycle, reproves the equation for each, walks the chain, summarises the
invoice-level proof written by `CABBIL11`, and prints **the verdict line operations read before
releasing the print stream.**

In `CABS7000` STEP040 it runs in line with `HALT=Y` so **its return code governs the close and the
print release.** RC 12 stops everything downstream.

### 9.4 What "clean" actually looks like

| Check | Clean | Where |
|---|---|---|
| Every `CT-BAL-IND` = `'B'` | Empty `CABCTLRP` report | `JCL/CABCTLRP.jcl` |
| No chain break across 63 edges | `CABRPT01` prints no chain-break lines | `CABS6000` or `CABS7000` STEP040 |
| Hash continuity | The four hash totals chain between adjacent processes | `CABRPT01` |
| No missing process | `EXPPRC` (expected process count) matches the count found | `CABS6000`, symbolic `EXPPRC` |
| Bill-level proof | `CABBIL11` reports every account in balance | `PROOFOUT` → `CABRPT01` |
| Verdict line | The line operations read | `BALOUT` |
| Return code | `CABRPT01` RC ≤ 4 | `CABS7000` STEP040 |

### 9.5 Four honest caveats about this framework

You should know these before you trust a green report.

1. **A sort step writes no control record at all.** `SORTEXIT/_README.md` §2 is blunt about the
   consequence: when `CABSE15D` suppresses 40,000 zero-value lines, or `CABSE15B` removes every
   IXC's minutes from the settlement aggregation, **the run still balances** — the program on the
   far side simply reads fewer records and reports the smaller number as its own `CT-READ`. There
   is no reconciliation point at which the difference becomes visible. The contract in
   `HARNESS/contracts/` records one edge, `CABRAT09 → CABBIL01`, as a **deliberate gap** for this
   reason, rather than pretending the chain is continuous. A gap in the chain is a place where
   nothing in the estate can see a divergence, which is why it is written down.

2. **Three programs satisfy the equation by moving zero into a count.** `BATCH/CONTROL/_MANIFEST.md`
   records it: `CABCTL01` counts factor and agreement rows that did not fit on its 400-byte extract,
   computes them into `WS-SUMM-CNT`, then moves **zero** into `CT-SUMMARISED` before balancing. The
   run always balances; the loss is visible only on the printed listing. `CABCTL02`'s
   `P8400-BALANCE` does the same.

3. **A control that passes is not the same as a figure that is right.** Several of the estate's
   own money controls compare two figures that are carried at different precisions, and absorb
   the difference under a tolerance. The control reports IN BALANCE and `CABRPT01` faithfully
   repeats it. `HARNESS/_README.md` §3 states the general rule: *"A harness that copies the
   estate's tolerances copies the estate's blindness."* Which controls, and what the residue
   costs, is in `SEALED/`.

4. **`CT-BAL-IND = 'O'` being routine in operations is not a reason to skip the check.** The harness
   asserts the equation on **both** sides independently and reports an out-of-balance legacy as a
   finding, not an exemption.

### 9.6 Comparing a run against the generated inputs

`HARNESS/` runs a staged L1–L5 comparison against a canonical form that neither side owns.

```bash
cd HARNESS
python3 -m unittest test_canonical -v        # 39 tests

python3 run_compare.py \
    --legacy    ../DATA/legacy \
    --candidate ../DATA/candidate \
    --contracts contracts/compare_contract.json \
    --probes    ../DATA/legacy/USAGE/boundary_probes.json \
    --out       ../DATA/attributed_run
```

Exit codes: `0` = MATCH or DIVERGENT-BY-DESIGN only; `1` = at least one DIVERGENT; `2` = the
harness could not run.

**Before the COBOL estate has been executed**, only L1, L2 and the first edge of L3 have data. L4
does not run at all (no `CABSBHDR`/`CABSBILL` datasets exist yet) and L5 runs only on the
counterparty meet-point view. `run_compare.py` reports `NOT RUN` with a reason rather than `MATCH`
— **a level skipped because its data did not exist is not a level that passed.** Seven of the twelve
seeded defects are only observable after the batch estate has run.

---

## 10. Restart and recovery

### 10.1 `CT-RESTART-KEY`

`CABSCTL.cpy` carries `CT-RESTART-KEY PIC X(26)` in `CT-STATUS`. On a controlled failure the
program writes its control record with the key of the last successfully processed input record.

Restart procedure:

1. Read the failing step's `CTLOUT` record. `REXX/CABCTLXT.exec` unpacks the COMP-3 counts and
   checks the equation — **but it is reference-only on TK4-.** Read the record with `IDCAMS PRINT`
   in `DUMP` format, or write a small COBOL utility.
2. Take `CT-RESTART-KEY`.
3. Supply it to the rerun job as a SYSIN parm.
4. Submit the family rerun member.

`REXX/CABRSTRT.exec` automated this in period: read the last control record, build a `RESTART=`
parameter, resubmit. It is reference-only. Note also that its stepname-to-sequence table is
maintained by hand and must be kept in step with the JCL library independently — a new step added
to any listed job requires a matching manual addition there.

### 10.2 Restartable vs full rerun

Every program's header comment block carries a `RESTART:` line —
`RESTARTABLE FROM CT-RESTART-KEY` or `FULL RERUN`. That line is the authority. Read the program
header before you restart anything.

The pattern across the estate:

| Class | Restart | Why |
|---|---|---|
| **Pure record-at-a-time transformers** — `CABING02`, `CABING03`, `CABING04`, `CABING05`, `CABING06`, `CABING10`, most of `BATCH/UTIL` | **Restartable from `CT-RESTART-KEY`** | Output is a new GDG generation. Rerun writes a fresh `(+1)`. |
| **Control-break / accumulator programs** — `CABING09`, `CABRAT09`, `CABRPT02`, `CABCTC01` | **Full rerun** | `CONVENTIONS.md`: *"Accumulate in the order records arrive. Never re-sequence before summing."* A partial restart produces a partial accumulation. |
| **Append-write programs** — `CABING07`, `CABING08`, `CABRAT12` | **Full rerun, and clean up first** | See §10.3. |
| **Two-store programs** — `CABJUR10`, `CABSET12`, `CABSET13`, `CABCTL02`, `CABCTL04`, `CABCTL05` | **Neither, safely** | See §10.4. |
| **The bill numbering program** — `CABBIL12` | **Full rerun, and reset `INVCTL`** | It assembles a 14-byte invoice number with a modulus-eleven check character and updates the `INVCTL` counter. A rerun without resetting the counter gaps the invoice number sequence. |
| **The restatement monster** — `CABJUR07` | **Full rerun** | Raises adjustment records. A partial run leaves partial adjustments that `CABJUR08` will then reverse incorrectly. |

### 10.3 Append writes — the rerun trap

Three files are opened `EXTEND` / `DISP=MOD` and **physical order carries meaning**:

| File | Written by | Why order matters |
|---|---|---|
| `TELCABS.CABS.AUDIT.LOG` | `CABING07`, `CABRAT12` | `CABING07` writes **three records per run** and the order of those three *is* the audit trail, read back positionally downstream. `CABRAT12` derives the **retry attempt count from physical position**, not from a stored field. |
| `TELCABS.CABS.USAGE.CFWD` | `CABING08` | Physical order is the cycle sequence, and the release logic on the *next* run depends on reading `CFWIN` back in that same order. |

`JCL/CABING7R.jcl`'s own comment says it: *"AUDLOG IS EXTEND — A RERUN ADDS ANOTHER 3-RECORD BLOCK
TO THE SAME AUDIT TRAIL, IT DOES NOT REPLACE THE FAILED ATTEMPT'S RECORDS."*

`JCL/CABING8R.jcl`: *"DO NOT RERUN THIS STEP TWICE FOR THE SAME CYCLE WITHOUT …"*

**Before rerunning any of these, copy the file off, truncate it back to its pre-run length, and
keep the copy.** Nothing in the estate will do it for you and nothing will tell you it needed
doing.

### 10.4 Two stores, one update

Six programs write the same business fact into two independent stores with **no coordination**:

| Program | Store 1 | Store 2 | What a failure between them leaves |
|---|---|---|---|
| `CABCTL02` | IMS `CARRFACT` segment | `TELCABS.CABS.FACTOR` KSDS | The quarter loaded in IMS and not in VSAM — online enquiry shows the new factor, jurisdictional split still prices at the old one |
| `CABCTL04` | IMS `BHSTSEG`/`BHSTDTL` | `TELCABS.CABS.BILLHDR` KSDS | The invoice number visible to the settlement statement program, the history row absent |
| `CABCTL05` | IMS `SETLSEG`/`SETLDTL` | `TELCABS.SETL.MASTER` KSDS | Same shape. `P9200-PRINT-TOTALS` prints `RUN CABSRSYN` — **and `CABSRSYN` does not exist anywhere in the estate** |
| `CABJUR10` | DB2 `CABSADJ` | VSAM balance master | The `COMMIT` covers DB2 only. The 2011 note claiming two-phase commit was never implemented |
| `CABSET12` | DB2 `SETLPERIOD` | VSAM close file | — |
| `CABSET13` | DB2 `SETLTRAN` | VSAM settlement master | Insert-count and VSAM-count are compared at end of run; a divergence produces a **message only**. The resync utility written in 2012 has never been scheduled |

**None of the six is recoverable by rerunning.** The IMS/DB2 side may be backed out and the VSAM
side will not be. On TK4- this is academic — all six are reference-only — but it is the shape of
the real recovery problem and it is the reason a modernization cannot treat these as ordinary
batch jobs.

### 10.5 GDG rolloff — what a rerun costs you

Every intermediate dataset in the daily stream is a GDG generation written as `(+1)`.
`CABGDGDF.jcl` defines the bases with `LIMIT(35) SCRATCH NOEMPTY`.

Consequences, and they are not obvious:

1. **A rerun that reaches `(+1)` consumes a generation.** Rerun `CABING01` five times and you have
   burned five generations of `USAGE.EDITED`, `USAGE.VALID`, `USAGE.DEDUP`, `USAGE.DATEVAL`,
   `USAGE.CLEAN`, `USAGE.SUSPENSE` and `USAGE.CONTROL`. At `LIMIT(35)` that is a week of retention
   gone in an evening.
2. **`SCRATCH` means the rolled-off generation is deleted, not uncataloged.** Once it rolls off it
   is gone. There is no recovering `(-36)`.
3. **The jobs that read back multiple generations break silently.** `CABS7000` STEP040 concatenates
   `CONTROL(0)(-1)(-2)(-3)`; STEP060 concatenates five. `CABS6200` concatenates four generations.
   `CABJ1600` reads `(-1)` **and** `(-3)`. `CABBIL10` reads the final header file at `(-1)` for the
   bill-to-bill variance test. **After three reruns, `(-3)` is not the generation those jobs were
   written to expect — it is a rerun of the current cycle.** The job will not fail. It will
   silently compare the cycle against itself.
4. **`NOEMPTY`** means an empty generation still counts toward the limit.

**Mitigation for a test environment:** set `LIMIT(3)` and treat every rerun as destroying history.
For anything you intend to analyse afterwards, `REPRO` the generation out to a non-GDG dataset
before you rerun.

### 10.6 Common abends — what causes each one *in this estate*

**S0C7 — data exception. Invalid packed-decimal data in an arithmetic operation.**

This is the abend you will see most. Specific causes here:

| Cause | Where |
|---|---|
| **Corrupt packed sequence number in the input.** The generator writes these on purpose — ~0.2% of records per day. | `CABING01`, `CABING03`. `GENERATORS/_README.md` §5 lists it under "Deliberate data conditions per day / corrupt packed sequence number ~0.2% — exercises S0C7 interception". |
| **A REDEFINES read through the wrong overlay.** `CD-REC-TYPE` `'03'` satisfies **both** `CD-VOICE-MOU` and `CD-DATA-SVC`; `'05'` satisfies both `CD-DATA-SVC` and `CD-SPECIAL-ACC`. Reading the data variant of a voice record puts display characters where COMP-3 is expected. | `CABING05`, `CABRAT02` |
| **The three CABSCDR variants are the wrong length.** `CD-VOICE-DETAIL` is 95 bytes over a 96-byte area, `CD-DATA-DETAIL` is 99, `CD-SPCL-DETAIL` is 97. Any tool trusting the redefines to be length-compatible reads three bytes past the variant area. | Anything reading `CABSCDR` |
| **Uninitialised WORKING-STORAGE.** OS/VS COBOL has no `INITIALIZE` (banned by `CONVENTIONS.md` anyway), so a COMP-3 field without a `VALUE` clause contains whatever was in storage. | Anywhere |
| **A sort exit's LINKAGE SECTION view not matching the actual bytes.** `SORTEXIT/_README.md` §4: *"Do not assume the record layouts in the LINKAGE SECTIONs match a copybook … in several cases they are views of a layout that only exists after another exit has rearranged it."* | `CABSE15A`, `CABSE35B` |

**Diagnosis:** the offset in the `IEA995I` message plus the `PMAP`/`DMAP` from the compile listing
gives you the statement and the field. This is why the compile PARM in §5.2 carries `DMAP,PMAP`.

**S806 — module not found in the library search order.**

| Cause | Where |
|---|---|
| **The ten unresolved subprograms.** `CABPARMR`, `CABHASH`, `CABERRWR`, `CABEDITF`, `CABSEQCK`, `CABOCNVL`, `CABCTLWR`, `CABFMTR`, `CABTBLLU`, `CABRTFMT`, `CABCIRCL` — none has source in this estate (§5.3). **You will hit S806 on `CABPARMR` in `P1000-INIT` of the very first program you run.** | Everywhere |
| **The dynamic call targets.** `CABRAT02` builds `'CABRAT'` + a 2-character suffix from `R2-EN-MODULE-SFX` in the rate table and calls it. Targets are `CABRATOA`, `CABRATTA`, `CABRATLT`, `CABRATTS`, `CABRATCC`, `CABRATSP`, `CABRATUN`, `CABRATOS` — **none of them exists as source in this estate.** Same shape for `CABJUR03`'s `CABJX` + state suffix, `CABJUR04`'s `CABRT` + jurisdiction suffix, `CABSET01`'s `CABMP` + region suffix, `CABSET07`'s `CABRA` + region suffix. | `CABRAT02` STEP030, `CABJ1200`, `CABJ1300`, `CABS2200`, `CABS2600` |
| **`STEPLIB` order.** `CABING01` STEP090 puts `TELCABS.CABS.LOADLIB.TEST` **first** — a soak from change CAB-2019-0447 that was never removed. If that library exists and holds a stale `CABING06`, that is what runs. | `CABING01` STEP090 |
| **Sort exit modules named in control cards that do not exist.** `CABSRT11` names `CABSXJUR`, `CABSRT12` names `CABSXZIP` and `CABSXBST`, `CABSRT15` names `CABSXLST`. **None of the four is in `SORTEXIT/` or anywhere else in this estate.** The sort will fail to load the exit. | Three sort steps |

**Diagnosis:** the S806 message names the module. Check the `STEPLIB`/`JOBLIB` concatenation in the
failing step, then check whether the module has source anywhere at all — for eleven of them, it
does not.

**B37 — out of space on a sequential dataset. (Also D37 = no primary, E37 = no more extents.)**

| Cause | Where |
|---|---|
| **The `SPACE=(CYL,(10,10),RLSE)` on every usage GDG DD is sized for a 3350 world.** At the DAILY profile a day's `USAGE.EDITED` is ~100 MB; 10 cylinders of a 3350 is 5.7 MB and 10 of a 3380 is 7.1 MB. **The secondary extent will run out at 15 extents.** | Every ingest and rating step |
| **`SORTWK` too small.** OS/360 Sort/Merge will fail rather than degrade. | Every sort step |
| **`TELCABS.CABS.AUDIT.LOG` and `USAGE.CFWD` grow on every rerun** because they are `DISP=MOD`. | `CABING07`, `CABING08`, `CABRAT12` |
| **`CABSBILL` is VB with 1 to 40 rate elements per record.** The maximum computed length is **1,647 data bytes** (127 fixed + 40 x 38), which is **LRECL 1651** on the DCB once the 4-byte RDW is counted; the COBOL `FD` clauses quote 1647 because a record description never includes the RDW (`GENERATORS/_README.md` §8). Size the bill detail datasets from the maximum, not the average. | `CABRAT10`, `CABBIL02` |

**Fix:** raise `SPACE=` and add `RLSE`; split across volumes; run at SMOKE.

**S322 — job or step exceeded its `TIME=` limit.**

| Cause | Where |
|---|---|
| `JCL/CABING01.jcl` carries `TIME=(15,0)` on the **job card** — 15 minutes for the whole 14-step job. **At the DAILY profile that is roughly the expected elapsed time for the job, with no margin.** Raise it. | `CABING01` |
| `CABS7000` carries `TIME=1440` (the maximum) and is fine. | — |
| **An infinite loop from a fall-through.** Complexity 01 is placed in seven programs: `CABING01 P6400`, `CABING03 P3200`, `CABING05 P4300`, `CABING09 P5100`, `CABING11 P4200`, `CABJUR03 P3300`, `CABJUR07 P2700`, `CABSET04 P2300`, `CABBIL05 P3400`, `CABFMT04 P3200`, `CABRPT03 P4200`. A paragraph with no `EXIT` drops into the next one; callers rely on that. **If you "fix" one by adding an `EXIT`, you change behaviour. If you get the `PERFORM … THRU` range wrong on a rerun, you can loop.** | Eleven programs |
| **A hidden error handler that reads the next record.** `CABCTL03`'s `P9990-DLI-FAILURE` *is part of the read loop, not a subroutine* — it reads the next transaction and `GO TO P2000-EXIT`. A handler like that with a condition that never clears loops forever. | `BATCH/CONTROL/` |

**Fix:** raise `TIME=` on the job card first, then look for the loop. `CABABEND` is on every failure
path in the estate and is the only place where a controlled failure becomes an operator-visible
event — but it is called, not automatic.

**Other abends worth knowing here:**

| Abend | Cause in this estate |
|---|---|
| **S013** | DCB mismatch on OPEN. `USAGE.DATA` is **VB 204** while every other usage file is **FB 200** — `CABING05` writes `DATOUT` as VB and `CABING09` reads `DATIN` as VB with `RECORD IS VARYING`. Get that DD wrong and you get S013. |
| **S37 / IEC030I B14** | Out of PDS directory blocks. See §4.2. |
| **U0xxx** | A user abend raised by `CABABEND` with the code in `CT-ABEND-CD`. Read the control record. |
| **IEC161I** | VSAM open error. Usually the cluster was never defined — check the complexity-15 datasets in §6.4 items 11–15. |

---

## 11. Known limitations — what will not run and why

**Stated plainly and completely, as asked.** This section is the honest accounting. Read it before
you plan the work.

### 11.1 The headline

Of **527 mainframe source files and ~195,000 lines**, **173 COBOL job-step programs
(≈ 136,000 lines) target the runnable environment**, supported by the **12 called subprograms in
`BATCH/COMMON/` (4,496 lines)**. The remaining **63 COBOL members (≈ 38,000 lines)** and the whole
of `IMS/`, `DB2/`, `ONLINE/`, `PLI/`, `REXX/`, `HLASM/` and `SORTEXIT/` are authored to production
standard for static analysis and will not execute on TK4-. Even the 173 will not produce a
completed cycle until you have compiled and link-edited `BATCH/COMMON/` and dealt with the IMS and
MQ call sites (§5.3).

### 11.2 Entire layers that cannot run

| Layer | Files | Why it cannot run on TK4- |
|---|---:|---|
| **`IMS/`** — 4 DBDs, 2 index DBDs, 6 PSBs, the gen job | 15 | **MVS 3.8j has no IMS DB and no DL/I batch region (`DFSRRC00`).** There is nothing to run `DBDGEN`/`PSBGEN`/`ACBGEN` against. |
| **`BATCH/CONTROL/`** — 6 IMS DL/I + 2 MQ programs | 9 | No IMS, no MQ, **and** they are Enterprise COBOL, which the TK4- OS/VS COBOL compiler will not accept. |
| **`DB2/`** — DDL, DCLGEN, BIND, GRANT, RUNSTATS | 11 | **MVS 3.8j has no DB2 subsystem.** Nothing to bind against. |
| **DB2-precompiled COBOL** — `CABJUR10`, `CABSET12`, `CABSET13` | 3 | No DB2 precompiler. Enterprise COBOL as well. |
| **`ONLINE/` + `ONLINE/BMS/`** — 6 CICS programs, 6 mapsets, assembly JCL | 15 | **No CICS region, no VSAM RLS, no BMS macro library (`DFHMAC`/`SDFHMAC`).** `CABMAPS.jcl` has never been submitted. Enterprise COBOL. |
| **`PLI/`** — 4 OS PL/I programs | 5 | OS PL/I Optimizing Compiler V2 syntax. Also `CABLGCNV.pli` references `%INCLUDE CABCDRLY`, **which is not in this repository**, so it is not independently compilable as delivered. |
| **`REXX/`** — 6 execs | 7 | **MVS 3.8j predates TSO/E REXX at the level used** (`ISFEXEC`/SDSF panel variables, `LISTCAT … GDG ALL` parsing, `OUTTRAP` against `SUBMIT`). `CABCMPDS` and `CABCTLXT` would run unmodified on a modern z/OS TSO/E; the other four would not. |
| **`HLASM/`** — 5 assembler modules | 6 | Written against a later macro library than TK4- ships (`WTO MF=(E,…)`, `SNAP`, `ABEND`, `GETMAIN R`) and use `ENTRY`/alias conventions and `EX`-driven code needing re-verification. `CABPKDEC` is reentrant and `GETMAIN R`/`FREEMAIN R` per entry, which is correct but costly at rating call rates. |
| **`SORTEXIT/`** — 27 E15/E35 exits | 29 | Three separate reasons, all disqualifying: (1) **Enterprise COBOL** — pointers, `SET ADDRESS OF`, scope terminators; (2) the exits would have to be link-edited as OS/VS COBOL or assembler with a **static save-area convention**, not LE-enabled modules; (3) **`CABSE15B` opens a QSAM file (`CARRTYPE`) from inside a sort exit**, which requires the DD to be allocated on the sort step — and the current `CABSRT` control cards do not do it. |
| **`VSAM/`** — 21 IDCAMS members | 21 | Marked reference-only because none of the target volumes `TELV01`–`TELV12` exists. **You can create them (§3) and then most of these will run** — with the two syntax corrections in §6.6. |

### 11.3 The sort control cards — read this before you plan a run

**15 of the 18 members in `JCL/CTLCARDS/` use statements OS/360 Sort/Merge does not support.**

| Card | Uses | Runs on OS/360 S/M? |
|---|---|---|
| `CABSRT01` | `INCLUDE COND=` | ❌ |
| `CABSRT02` | `OMIT COND=` | ❌ |
| `CABSRT03` | `SUM FIELDS=(120,7,PD)` | ❌ |
| **`CABSRT04`** | `SORT FIELDS`, `MODS=(E15,E35)`, `OPTION EQUALS`, `RECORD` | ✅ **syntactically clean** |
| `CABSRT05` | `OUTREC FIELDS=` | ❌ |
| `CABSRT06` | `MERGE FIELDS` + `INCLUDE COND=` | ❌ (the `MERGE` is fine, the `INCLUDE` is not) |
| `CABSRT07` | `SUM FIELDS=(160,7,PD)` + `MODS=(E35=CABSE35B)` | ❌ |
| `CABSRT08` | `INCLUDE COND=` | ❌ |
| `CABSRT09` | `INCLUDE COND=` | ❌ |
| `CABSRT10` | `SUM FIELDS=` | ❌ |
| `CABSRT11` | `MODS E15=(CABSXJUR…)`, `OPTION EQUALS,VLSHRT`, `RECORD TYPE=V` | ❌ — `VLSHRT` is DFSORT. `CABSXJUR` exists, as reference-only Enterprise COBOL |
| `CABSRT12` | `MODS E15=(CABSXZIP…),E35=(CABSXBST…)` | ✅ **syntactically clean** — but both exits are reference-only |
| `CABSRT13` | `SORT FIELDS=COPY` + `INCLUDE COND=` | ❌ — `COPY` is DFSORT |
| `CABSRT14` | `OUTFIL OMIT=` | ❌ |
| `CABSRT15` | `MODS E35=(CABSXLST…)` | ✅ **syntactically clean** — but the exit is reference-only |
| `CABSRT16` | `SUM FIELDS=NONE` | ❌ |
| `CABSRT17` | `INREC FIELDS=` + `OUTREC FIELDS=` | ❌ |
| `CABSRT18` | `INCLUDE COND=` | ❌ |

**Three cards of eighteen — `CABSRT04`, `CABSRT12`, `CABSRT15` — carry only statements OS/360
Sort/Merge accepts. All three name exits, and every exit in `SORTEXIT/` is reference-only, so
none of the three runs end to end without the exit work described below.**

**What this means practically.** To get a daily stream through, you must re-express each card's
business rule somewhere the sort can execute it. That is *exactly* the work that
`SORTEXIT/_README.md` §5 describes as the modernization task:

> *"Any target-state design must begin by extracting, from these twenty-seven modules and the
> thirty-five control-card members across `JCL/CTLCARDS/` and `JCL/CTLCARDS/MVT/`, the complete
> set of filter, transform, grouping and rounding rules, and restating them as explicit,
> testable pipeline stages. The rules must be *re-specified*, not translated: several of them
> (the territory list, the string limit, the zero thresholds, the 2012 grouping change) are
> constants in source that were never written down as requirements."*

Getting the estate to run on TK4- and modernizing it turn out to require the same first step. That
is not an accident — it is the point of the asset.

**Every `MODS=` reference in `JCL/CTLCARDS/` now resolves to source.** Twenty-two distinct exit
modules are named across the thirty-five control-card members, and all twenty-two have a `.cbl` in
`SORTEXIT/`. There are no dangling `MODS=` references left.

The imbalance runs the other way. `SORTEXIT/` holds **27** modules, so **five have source but are
named by no control card in the estate**: `CABSE15B`, `CABSE15C`, `CABSE15D`, `CABSE35C` and
`CABSE35D`. A dependency graph built from `MODS=` operands will show them as orphans, and a graph
built from COBOL call sites will not show any of the 27 at all.

### 11.4 Called subprograms

Every statically-called *estate* subprogram has source. Full table in §5.3. Restated as bare fact:

- **`CABPARMR` is called from 141 sites** and every program's `P1000-INIT` depends on it —
  `BATCH/COMMON/CABPARMR.cbl`.
- **`CABHASH` is called from 120 sites** — it accumulates the four hash totals the entire balancing
  framework rests on — `BATCH/COMMON/CABHASH.cbl`.
- The twelve modules in `BATCH/COMMON/` cover **519 call sites** between them. They appear in no
  JCL, so a build list driven off `EXEC PGM=` will miss all twelve.
- Of the 200 `CALL 'CABABEND'` sites, `CABABEND` exists as reference-only HLASM.

**What still has no source is `CBLTDLI` (36 sites) and the seven MQ verbs (15 sites)** — system
interface modules, not estate code, and absent from MVS 3.8j entirely. They are confined to
`BATCH/CONTROL/`, which is reference-only in any case.

**Dynamic `CALL` targets remain a separate problem** — see §5.3 and the dynamic-call families in
`CONTRACTS/complexity_placement.json`.

### 11.5 Documentation that contradicts the source — deliberate, do not "fix"

The estate carries authentic decay. These are recorded so you do not waste time reconciling them:

| Claim | Reality |
|---|---|
| `JCL/CTLCARDS/CABSRT04.ctl` describes `CABSE15A` as **"(ASSEMBLER, CABSE15A)"** | It was recoded in COBOL in 2004. The control card was never updated. |
| `HLASM/_MANIFEST.md` says `CABBITST` is *"statically called from `CABING01`, `CABING05`, `CABING09`, `CABING11`"* | **The string `CABBITST` does not appear in any `.cbl` file in the estate.** |
| `CABCTL05`'s `P9200-PRINT-TOTALS` prints `RUN CABSRSYN` | **`CABSRSYN` does not exist anywhere in the estate.** |
| `CABSE35A` calls its output area a **"prefix"** | Since the 1994 fixed-length change it sits in the trailing filler. |
| `CTC/_SITE_INDEX.md` lists **thirteen** centres | There are **twelve**. SITE13 (Kansas City) was folded into SITE04 (St Louis) in September 2014; CHG-2014-0918 was never closed. |
| `CABCTL02`'s 2007 change note claims "two phase commit added" | It only added the `CHKP` call. |
| `PLI/CABLGCNV.pli` is retained "for the remaining sites on the 1987 format" | Every site converted at the 1988 cutover. Its only caller (`CABCTL04 P7700`) is behind a switch that is never set. |
| `CABCTL04`'s revision history records the bridge added in 1998 "for the sites still on the old cut" | Not true of any site now. |
| `VSAM/CABVS060`'s history says the Factor table moved off `TELV09` in 2009 | `CABVS100` still allocates against `TELV09`. |
| `VSAM/_MANIFEST.md` classifies `CABVS060` as complexity 15 (orphan file) | **It is not an orphan.** `CABING10.cbl` has a live `SELECT FCTRIN`, and `CABIN10R.jcl`/`CABING01.jcl` carry live `//FCTRIN DD DSN=TELCABS.CABS.FACTOR`. The manifest itself records this caveat and flags the classification as superseded. |

Revision histories across the estate span 1987–2019 and **some are deliberately wrong or describe
behaviour that no longer exists** (`CONVENTIONS.md`). Do not reconcile them.

### 11.6 Environmental and behavioural constraints

| Constraint | Detail |
|---|---|
| **DASD** | Stock MVS 3.8j supports nothing later than a 3350 (§3.1). |
| **Address space** | 16 MB. `REGION=0M` on `CABS7000` means "all of it", which is not much. |
| **No scheduler** | `REXX/CABGENJC.exec` did the symbolic substitution and submission in period; it is reference-only. You substitute by hand (§7.0). |
| **No `RESTART=` automation** | `REXX/CABRSTRT.exec` is reference-only. |
| **Copybook self-disagreements** | **One** remains, listed in `GENERATORS/_README.md` §8: `CABSPRNT`'s `PC-BODY-A REDEFINES PC-BODY` is 118 bytes against 132. `CABSCDR` computes to exactly its declared 200 and `CABSBHDR` to its declared 400; `CABSBILL` declares LRECL 1651 against a 1,647-byte ODO maximum, which is the 4-byte RDW a RECFM VB record carries and is therefore correct, not a disagreement. **The generator reports and does not repair.** `COPYBOOKS/*` is frozen — `CONVENTIONS.md`: *"If a layout is wrong, report it, do not change it."* |
| **Overlapping 88-levels in the frozen copybook** | `CD-REC-TYPE` `'03'` satisfies both `CD-VOICE-MOU` and `CD-DATA-SVC`; `'05'` satisfies both `CD-DATA-SVC` and `CD-SPECIAL-ACC`. The harness resolves the ambiguity the same way for both sides *and records that the ambiguity exists as a finding*. |
| **`CABDATCV` resolves to two different modules** | Pivot **70** from `TELCABS.COMMON.LOADLIB` (2018), pivot **68** from `TELCABS.SETL.LOADLIB.EMERG` (1998), with different leap-year rules. Which one a program gets is decided by `STEPLIB` order at load time. `CABS2900` and `CABS3200` bind the 1998 version. **The same source-level `CALL 'CABDTCNV'` resolves a two-digit year of 68 or 69 to 1968/1969 in the settlement jobs and 2068/2069 in the rating jobs.** |
| **`CABPKDEC` rounds differently from its COBOL callers** | Half away from zero at the **fifth** decimal place; the callers' `COMPUTE … ROUNDED` is half-up at the **receiving field's** scale, normally two places, and toward zero for negatives in some releases. Amounts passing through both are rounded twice, in two directions. |

### 11.7 Seeded defects

**Twelve seeded defects, D1–D12, exist in this estate.** They are not annotated in the source. The
answer key is in `SEALED/`:

| File | Defects |
|---|---:|
| `SEALED/answer_key_ingest_rating.json` | 3 |
| `SEALED/answer_key_juris_settle.json` | 4 |
| `SEALED/answer_key_bill_format_report.json` | 4 |
| `SEALED/answer_key_reference_layer.json` | 1 |
| `SEALED/defect_placements.json` | the placement register for all 12 |

**`SEALED/` must be withheld for a blind run.** So must `HARNESS/defect_signatures.json` — it names
the defects and describes their fingerprints, and handing it to a blind run leaks the answer as
surely as the key does. `verdict.py` raises `BlindRunError` on either.

Three of the twelve are only observable under specific conditions, and all three conditions are
about the *input*, not the code:

- One needs the band-boundary probes. Volume-band edges in an access tariff are round numbers
  (50,000 / 100,000 / 250,000 / 500,000 / 1,000,000 / 2,500,000 / 5,000,000 minutes) and a random
  sample will essentially never land on one. The generator places *n* probes per day and writes
  them to `USAGE/boundary_probes.json`.
- One needs the `STRESS` profile or larger — see §8.
- One needs a bill detail line carrying more rate elements than an ordinary account produces. That
  is a property of the *shape* of the input rather than of its size: it happens when enough rated
  element records fold into a single account / period / section / line-sequence group for the
  assembled line to run long. Ordinary traffic does not do it and a random sample does not find it.
  `GENERATORS/gen_divergence.py` has a mode that places the shape, and unlike every other mode
  there it must be applied to **both** generations with the same seed — it shapes the input, it
  does not mimic a target behaviour. Check the assembly step's printed register for the element
  distribution afterwards.

Which is which is in `SEALED/`.

---

## Sources for external facts

| Fact | Source | Status |
|---|---|---|
| TK4- distribution and User's Manual | <http://wotho.ethz.ch/tk4-/> · <https://wotho.ethz.ch/tk4-/MVS_TK4-_v1.00_Users_Manual.pdf> | verified |
| TK4- derived from Turnkey 3 by Jürgen Winkelmann | Prince Webdesign, "An update on MVS Turnkey 4"; Jay Moseley | verified |
| TK4- logon `HERC01` / `CUL8TR`; `SHUTDOWN` then `LOGOFF` from TSO; `DIAG8CMD` authority | TK4- User's Manual | verified |
| Docker image, ports 3270/8038, volumes, `--platform linux/amd64` on ARM | <https://github.com/skunklabz/tk4-hercules> | verified |
| OS/360 Sort/Merge, Tom Armstrong v1.01, CBT File #1036, `mvtsort.tgz`, targets `SYS2.LINKLIB` + `SYS1.SORTLIB` | <https://www.jaymoseley.com/hercules/compilers/sort.htm> | verified |
| `IGNWKDD` and explicit `SORTWKdd` DD requirement | Tom Armstrong, *OS/360 Sort/Merge for MVS 3.8 Installation, Customization and Diagnosis Guide* v1.01 | verified |
| `SORTW1`–`SORTW6` on esoteric `SORTDA` in a standard MVS 3.8j build | Same guide | verified |
| MVS 3.8j supports no DASD later than 3350; Jim Morrison usermods add 3375/3380/3390; TK5 ships on 3390 | <https://www.jaymoseley.com/hercules/installMVS/modernDASD/modernDASD.htm> | verified |
| Hercules emulates 2311, 2314, 3330, 3340, 3350, 3375, 3380, 3390 | <http://www.hercules-390.org/hercload.html> | verified |
| TK5 supersedes TK4- | Jay Moseley; Prince Webdesign | verified |
| **Whether TK4- ships a sort, and which** | — | **unverified** |
| **Whether TK4- ships a working FTP server on port 2121** | — | **unverified** |
| **Exact TK4- procedure for extending the `SYSDA` unit group with a new volume** | — | **unverified** |
| **TK4- cataloged procedure names and PARM defaults for OS/VS COBOL** | — | **unverified** |
| **Whether every step here works unchanged on TK5** | — | **unverified — not tested** |

---

*`DOCS/HERCULES_RUNBOOK.md` · CABS Tier 5 wholesale access billing reference estate.
Companion documents: `DOCS/CAST_IMAGING_GUIDE.md`, `README.md`, `CONVENTIONS.md`.*
