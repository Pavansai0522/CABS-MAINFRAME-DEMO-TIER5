# CABS Tier 5 — Data Generators

Scale-parameterised generators that produce a complete, internally consistent
input set for the wholesale Carrier Access Billing System estate: reference
masters, daily usage, the inbound settlement feed, generation-level control
records and a reconcilable run manifest.

Everything is **EBCDIC (cp037)**, fixed-length, and laid out from the frozen
copybooks in `../COPYBOOKS`. No layout is restated in Python — `gen_common.py`
parses the `.cpy` members directly, so the generator cannot drift from the
data architecture. Where a copybook disagrees with itself, the generator
**reports and continues**; it does not repair. See *Copybook diagnostics*
below.

Python 3.9+. No third-party dependencies.

---

## 1. Quick start

```bash
cd GENERATORS

# unit tests first — COMP-3 correctness is load-bearing
python3 -m unittest test_gen_common -v

# smallest useful run
python3 generate.py --profile SMOKE --days 1 --seed 20260815 --outdir ../DATA/smoke

# the standard cycle, 30 daily generations, 8 workers
python3 generate.py --profile DAILY --days 30 --seed 20260815 \
        --outdir ../DATA/cycle240930 --cycle-end 2024-09-30 --workers 8

# a candidate estate for the comparison harness
python3 generate.py --profile SMOKE --days 1 --seed 20260815 \
        --outdir ../DATA/candidate --divergence all
```

---

## 2. Volume profiles

| `--profile` | CDRs per day | Shards/day | Approx. size/day | Approx. wall clock/day (1 worker) |
|---|---:|---:|---:|---:|
| `SMOKE`   | 50,000 | 1 | 10 MB | ~7 s |
| `DAILY`   | 500,000 | 2 | 100 MB | ~70 s |
| `STRESS`  | 2,000,000 | 8 | 400 MB | ~4.5 min |
| `TARGET`  | 100,000,000 | 400 | 20 GB | ~3.7 h (~30 min at `--workers 8`) |

`TARGET` is intended for the cloud target side only. The legacy estate under
Hercules is not sized for it; use `STRESS` for anything that has to run on
MVS 3.8j.

`STRESS` is the smallest profile that forces the summary sort to spill across
more than one `SORTWK`. Some behaviour in the sort-exit layer only appears once
that happens, so a `SMOKE` or `DAILY` run exercises a strictly smaller part of
the estate than the profile name suggests. Choose the profile against what you
intend to exercise, not against how long you are willing to wait.

---

## 3. Every flag on `generate.py`

| Flag | Default | Meaning |
|---|---|---|
| `--profile {SMOKE,DAILY,STRESS,TARGET}` | `DAILY` | volume profile; sets CDRs/day and scales circuits, settlement details and boundary probes |
| `--days N` | `1` | number of daily generations; each becomes one GDG generation |
| `--seed N` | `20260815` | master RNG seed. Same seed + profile + days + cycle-end ⇒ byte-identical output |
| `--outdir PATH` | `./cabs_data` | output root; subdirectories `REFERENCE/`, `USAGE/`, `SETTLEMENT/`, `CONTROL/` |
| `--cycle-end YYYY-MM-DD` | `2024-09-30` | last day of the billing cycle; the generated days end here |
| `--cycle-days N` | `30` | length of the billing cycle window, used to classify prior-cycle and future-cycle usage |
| `--records-per-day N` | *profile* | override the profile's record count for a targeted test |
| `--workers N` | `1` | parallel shard workers. **Does not change the output**, only the wall clock |
| `--copybooks PATH` | `../COPYBOOKS` | copybook directory |
| `--no-reference` | off | skip the master files and reuse `REFERENCE/reference_index.json` |
| `--no-usage` | off | skip the usage generations |
| `--no-settlement` | off | skip the inbound settlement feed |
| `--settlement-details N` | *profile* | inbound CMDS detail record count |
| `--boundary-probes N` | *profile* | exact-band-boundary probe records per day (see §6) |
| `--divergence SPEC` | `none` | apply named target-side behaviour differences; `all` or a comma-separated list, see §7 |
| `--manifest NAME` | `run_manifest.json` | manifest filename |
| `--quiet` | off | suppress progress output |

### Determinism

Output is byte-identical for a given `--seed`, `--profile`, `--days` and
`--cycle-end`, and is **independent of `--workers`**. Work is split into
fixed 250,000-record shards derived from the record count; workers only
decide who processes which shard, never how the shards are cut. Each shard
draws from its own BLAKE2b-derived sub-stream.

Verify it:

```bash
python3 generate.py --profile SMOKE --days 2 --seed 1 --outdir /tmp/a --workers 1 --quiet
python3 generate.py --profile SMOKE --days 2 --seed 1 --outdir /tmp/b --workers 8 --quiet
diff <(sha256sum /tmp/a/USAGE/*.dat | awk '{print $1}') \
     <(sha256sum /tmp/b/USAGE/*.dat | awk '{print $1}')   # no output
```

---

## 4. Modules

| Module | Responsibility |
|---|---|
| `gen_common.py` | EBCDIC cp037, COMP-3 pack/unpack with explicit scale, zoned decimal, YYDDD Julian helpers, COBOL copybook parser, `RecordBuilder`, `FixedRecordWriter`, `HashTotals`, `DeterministicRandom` |
| `gen_reference.py` | carrier master, rate table, PIU/PLU factors, circuit/trunk inventory |
| `gen_cdr.py` | the usage generator — all three CABSCDR variants |
| `gen_settlement.py` | inbound CMDS/RAO exchange file, counterparty meet-point view |
| `gen_divergence.py` | candidate-side behaviour differences (harness fixture) |
| `generate.py` | orchestrator, sharding, manifest |
| `test_gen_common.py` | 58 unit tests, COMP-3 correctness foremost |

### Arithmetic rules, enforced

* `decimal.Decimal` everywhere. `gen_common._dec()` **raises `TypeError` on a
  float** rather than accepting it — a float is how a five-decimal fractional
  cent rate silently becomes wrong.
* Every COMP-3 and zoned field carries its declared scale end to end.
  `unpack_comp3` returns a Decimal with exactly the declared number of decimal
  places; `0.00` and `0` stay distinguishable.
* Rounding is always an explicit argument. The default is `ROUND_HALF_UP`
  (COBOL `COMPUTE ... ROUNDED`); a caller reproducing a truncating `COMPUTE`
  must pass `ROUND_DOWN` and say so.

---

## 5. Files produced, and the record layouts

### `REFERENCE/`

| File | DSN | DD | Copybook | LRECL | Records |
|---|---|---|---|---:|---:|
| `TELCABS.CABS.CARRIER.dat` | `TELCABS.CABS.CARRIER` | `CARRMST` | `CABSCARR` | 138 | 450 |
| `TELCABS.CABS.RATE.dat` | `TELCABS.CABS.RATE` | `RATEMST` | `CABSRATE` | 619 | ~1,200 |
| `TELCABS.CABS.FACTOR.dat` | `TELCABS.CABS.FACTOR` | `FCTRMST` | `CABSFCTR` | 76 | ~3,000 |
| `TELCABS.CABS.CIRCUIT.dat` | `TELCABS.CABS.CIRCUIT` | `CIRCMST` | `CABSCIRC` | 136 | 2,000–60,000 |
| `reference_index.json` | — | — | — | — | ASCII sidecar |

**Carrier master** — 450 OCNs: 40 IXCs, 180 CLECs, 60 ILECs, 70 wireless,
100 resellers. Four-character OCNs, ~72% numeric and ~28% alphanumeric,
because `CABHASH` treats the two forms differently and the control-total
reconciliation has to survive the mix. Each carrier carries an ACNA, a CIC,
default PIU/PLU with five decimals, a reciprocal compensation rate, an ISP
cap in MOU, a CMDS RAO code and three BANs. ~4% are inactive or expired, so
`CABOCNVL` has something to reject.

**Rate table** — banded rates via `RT-BAND OCCURS 1 TO 24 DEPENDING ON
RT-BAND-CNT`. Written at the maximum ODO length with unused band slots
binary zero; `RT-BAND-CNT` governs. Rates carry five decimals and fractional
cents are normal. `RT-ROUND-RULE` is drawn **per record**, so sibling records
for the same element in different states disagree about rounding — which is
the estate's actual rule (`CONVENTIONS.md`: rounding is driven by
`RT-ROUND-RULE`, not by the verb in the code).

**PIU/PLU factors** — three quarterly filings per OCN/state/LATA. ~35% of the
current-quarter filings carry `FC-RESTATE-SW = 'Y'`. Of those, 1 in 4 has a
restatement window that crosses a year boundary and 1 in 8 carries a zero
prior factor.

**Circuit inventory** — trunk groups, CLLI codes, LATAs, service types.
~30% of switched and interconnect circuits are meet-point billed, and **~6% of
meet-point percentage pairs do not sum to 100.00000** — the two LECs file
independently and sometimes disagree.

### `USAGE/`

| File | DSN | DD | Copybook | RECFM | LRECL |
|---|---|---|---|---|---:|
| `TELCABS.CABS.USAGE.RAW.G0001V00.dat` … | `TELCABS.CABS.USAGE.RAW(+n)` | `RAWIN` | `CABSCDR` | FB | 200 |
| `boundary_probes.json` | — | — | — | — | — |

One generation per day, GDG-named `Gnnnn Vnn`. Every record is a
`CABS-CDR-RECORD`; the 96-byte `CD-VARIANT-AREA` is written through one of
the three REDEFINES overlays according to `CD-REC-TYPE`:

| `CD-REC-TYPE` | Variant written | Share |
|---|---|---:|
| `01`, `02` | `CD-VOICE-DETAIL` | 52% |
| `03` | ambiguous — both `CD-VOICE-MOU` and `CD-DATA-SVC` are true | 6% |
| `04` | `CD-DATA-DETAIL` | 12% |
| `05` | ambiguous — both `CD-DATA-SVC` and `CD-SPECIAL-ACC` are true | 4% |
| `06`, `07` | `CD-SPCL-DETAIL` | 20% |
| `08` | reciprocal compensation, voice layout | 6% |

Rate elements are the estate's own six-character codes: `ORIGAC`, `TERMAC`,
`LTRANS`, `TANSW ` (note the trailing space), `CCLINE`, `DATASV`, `DATATR`,
`RECIPC`, `UNELEM`, `MPBCHG`.

NPA-NXX, LATA and state values are drawn from a table of 30 real states with
their real LATA numbers and NPAs, so the jurisdiction tables resolve.

Deliberate data conditions per day:

| Condition | Share | Exercises |
|---|---:|---|
| clean | ~93% | the ordinary path |
| `CD-EDIT-STATUS` suspect (`1`–`5`) | ~4.5% | CABING01/CABING05 edit routing |
| `CD-EDIT-STATUS` fatal (`6`–`9`) | ~2.5% | the fatal edit routing in `CABING01`/`CABING05` |
| duplicate `CD-SEQ-NBR` within a key | ~0.3% | CABING03 duplicate detection |
| connect date before the cycle start | ~1.5% | CABING08 carry-forward |
| connect date after the cycle end | ~0.4% | cycle-boundary rejection |
| unknown OCN | ~0.2% | `CABOCNVL` |
| invalid day-of-year (`999`) | ~0.3% | CABING04 date validation |
| corrupt packed sequence number | ~0.2% | S0C7 interception |
| zero charged minutes | ~0.4% | CABING09 `P6600-NEGATIVE-MOU-ADJ` guard |
| exact band boundary | *n* probes | band selection at an edge — see §6 |

### `SETTLEMENT/`

| File | DSN | DD | Layout | LRECL |
|---|---|---|---|---:|
| `TELCABS.SETL.CMDS.IN.G0001V00.dat` | `TELCABS.SETL.CMDS.IN(0)` | `CMDSIN` | `WS-CMDS-RECORD` (declared in CABSET07/CABSET08 working storage) | 180 |
| `TELCABS.SETL.MPB.COUNTERPARTY.dat` | `TELCABS.SETL.MPB.COUNTERPARTY` | `MPBCPIN` | `CABSSETL` | 186 |

The CMDS exchange record is the 180-byte industry format: one `HD` header,
*n* `DT` details, one `TR` trailer. It is **entirely DISPLAY** — the MOU and
amount fields are signed zoned decimal with a trailing overpunch, not COMP-3.
The exchange format predates the internal packed layouts and has never been
renegotiated; a generator that writes COMP-3 here produces a file CABSET08
cannot read.

~3% of inbound details carry an RAO code that resolves to no OCN; one file in
eight has a trailer that does not agree with its own detail.

The counterparty meet-point file states *their* percentage and *their* share
for every meet-point circuit. It is what makes the L5 meet-point comparison
possible: where the two filed percentages do not sum to 100.00000, the two
files cannot both be right.

### `CONTROL/`

| File | DSN | DD | Copybook | LRECL |
|---|---|---|---|---:|
| `TELCABS.CABS.CONTROL.G0000V00.dat` | `TELCABS.CABS.CONTROL(+1)` | `CTLOUT` | `CABSCTL` | 180 |

One `CABS-CONTROL-RECORD` per generated usage file, process ID `GENUSAGE`.
The generator declares its counts and hash totals in the estate's own control
format, so `CT-READ` on `CABING01` has something to reconcile against and the
harness's L3 chain has an anchor. The balancing equation
`CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED + CT-CARRIED-FWD` holds
on these records by construction.

### `run_manifest.json`

Records the parameters, the copybook diagnostics, and for every file: record
count, byte count, SHA-256, and the four hash totals (`hash_minutes`,
`hash_amount`, `hash_seq`, `hash_ocn`) computed in arrival order using the
same accumulation rule the COBOL uses. This is what a mainframe run is
reconciled against.

---

## 6. Band-boundary probes

Volume-band edges in an access tariff sit on round quantities — 50,000 /
100,000 / 250,000 / 500,000 / 1,000,000 / 2,500,000 / 5,000,000 minutes. A
carrier's volume commitment is negotiated to sit on one of them, so real
traffic clusters at the edges far more often than a uniform sample would.

Randomly generated usage does not reproduce that. A random sample essentially
never lands on an exact boundary quantity, so any behaviour that depends on
which side of an edge a quantity falls goes unexercised no matter how many
records you generate.

The generator therefore places *n* records per day whose `CD-VC-CHG-MIN` is
exactly a boundary value, and writes the list to
`USAGE/boundary_probes.json`:

```json
{
 "day": "2024-09-28", "ocn": "H810", "ban": "251G657108931", "seq": 2,
 "rate_elem": "ORIGAC", "boundary_qty": "50000.00"
}
```

The harness asserts on these specific records rather than hoping a sample
lands on an edge. What the comparison should show for them is a question for
the run, not for this file.

---

## 7. `--divergence`: making a candidate estate

Until a real target implementation exists, the comparison harness needs
something to compare the legacy estate against. `gen_divergence.py` produces
one by applying named behaviour differences to the generated usage.

| Name | Mimics | Verdict the harness should reach |
|---|---|---|
| `band-boundary` | target treats an exact boundary as the higher band | DIVERGENT-BY-DESIGN |
| `tandem-rounding` | target rounds tandem minutes where legacy truncates | DIVERGENT-BY-DESIGN |
| `fatal-excluded` | target excludes fatal-status records from the billable stream | DIVERGENT-BY-DESIGN |
| `cycle-window` | target keeps prior-cycle usage the legacy window drops | DIVERGENT-BY-DESIGN |
| `element-overflow` | *shapes the input* — see below | DIVERGENT-BY-DESIGN |
| `unseeded-drift` | a one-cent movement with nothing behind it | **DIVERGENT** |
| `record-loss` | records simply missing | **DIVERGENT** (L1) |

The defect each of the first five is expected to attribute to is not stated
here; the harness derives it from `SEALED/`.

The last two are there on purpose. A harness that classifies everything as
DIVERGENT-BY-DESIGN is a rubber stamp; those two prove it still says no.

### `element-overflow` is different and has to be run differently

Every other mode mimics a *target behaviour* and belongs on the candidate side
only. `element-overflow` shapes the **input**: it manufactures a population of
accounts whose rated elements fold into a single bill detail line carrying more
rate elements than an ordinary account ever produces. Nothing on a CDR
distinguishes a correct target from the legacy here — both read the same usage
and part company several steps later — so there is no target behaviour to
mimic, only a precondition to create.

**Run it on both generations, with the same seed.** Applying it to one side
produces a large L1/L2 finding about the generator and tells you nothing about
the process under test; the mode is listed under `shapes_input` in the
divergence report so this cannot be missed by accident. The accounts it places
are listed by OCN and BAN under `element_overflow_accounts` — assert on those,
do not sample for them.

Two honest limits. The mode gets the shape onto the usage file; whether the
assembled line actually reaches the intended element count depends on the
ingest edit and on the `MAXELM` control card, so read the assembly step's
printed register afterwards rather than assuming. And the condition it is
setting up lives in bill detail, which does not exist until the COBOL estate
has been executed — `L4` reports NOT RUN on a generated-data-only run for
exactly that reason.

The divergence report goes into the manifest and is the scorecard a **blind**
harness run is marked against afterwards — it is never an input to the
harness.

---

## 8. Copybook diagnostics

The generator reports these on every run and does not correct them. They are
properties of the frozen data architecture:

```
CABSPRNT  PC-BODY-A REDEFINES PC-BODY but is 118 bytes against 132 (-14)
```

One only. Every other member in `COPYBOOKS/` reconciles with its declared
LRECL, and every REDEFINES fits inside its target.

- **`CABSBILL`** is the estate's only `RECFM VB` copybook, and it no longer
  reports anything. The declared LRECL of 1651 is the 1,647-byte maximum
  data length plus the 4-byte record descriptor word that a variable-length
  record carries and the copybook does not describe. The parser knows this:
  for a declared RECFM starting `V` it expects
  `declared LRECL == computed data length + RDW_LENGTH` and reports only a
  departure from that sum. The maximum is real: `BD-ELEMENT` occurs up to 40
  times at 38 bytes over a 127-byte fixed portion, so 127 + 40 × 38 = 1,647
  data bytes and 1,651 on the DCB. The COBOL `FD` clauses quote the data
  length (1647) because a COBOL record description never includes the RDW;
  the JCL `DCB=`, the sort `RECORD TYPE=V` card and the process contract
  register quote 1651 because those are physical lengths.
- **`CABSPRNT`** is a genuine shortfall. `PC-BODY-A` is shorter than
  `PC-BODY`, which is legal — a REDEFINES may be shorter than its target,
  only longer is an error. Note that the 118 is the parser's own figure and
  under-counts numeric-edited PIC characters; the true length is 131.

Handling: overlays are honoured at the redefined item's offset; records are
written at the **declared** LRECL and any shortfall is filled with EBCDIC
spaces (0x40), which is what a real `MOVE SPACES TO record` followed by field
moves leaves behind. Variable-length layouts are written at their natural
length, not padded to the maximum.

---

## 9. Loading into Hercules (TK4- / MVS 3.8j)

The generated files are already EBCDIC and already fixed-length, so no
codepage translation happens on the mainframe side. Transfer **binary**.

### 9.1 Transfer

Do not let FTP translate. Either:

```bash
# from the host running Hercules
ftp localhost 2121
> quote site fix 200          # or the LRECL of the file being sent
> binary
> put TELCABS.CABS.USAGE.RAW.G0001V00.dat 'TELCABS.CABS.USAGE.RAW(+1)'
```

…or stage the file onto a tape/AWSTAPE image and read it with `IEBGENER`.
Under TK4- the simplest reliable route is a card-image reader punch for small
files and an AWSTAPE volume for anything over a few megabytes.

### 9.2 Sequential datasets — `IEBGENER`

```jcl
//CABLOAD1 JOB (ACCT),'LOAD RAW USAGE',CLASS=A,MSGCLASS=X
//*
//* LOAD ONE DAY OF RAW USAGE INTO THE GDG.  THE INPUT IS ALREADY
//* EBCDIC FB 200 - DO NOT LET ANYTHING TRANSLATE IT.
//*
//STEP010  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  DSN=TELCABS.CABS.XFER.USAGE,DISP=SHR
//SYSUT2   DD  DSN=TELCABS.CABS.USAGE.RAW(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(CYL,(50,20),RLSE),
//             DCB=(RECFM=FB,LRECL=200,BLKSIZE=27800)
//SYSIN    DD  DUMMY
```

`BLKSIZE=27800` is 139 × 200, the largest multiple of the LRECL under 32760.
Use `BLKSIZE=27540` (153 × 180) for the 180-byte control and CMDS files.

### 9.3 VSAM masters — `IDCAMS DEFINE` + `REPRO`

```jcl
//CABLOAD2 JOB (ACCT),'LOAD CARRIER MASTER',CLASS=A,MSGCLASS=X
//*
//STEP010  EXEC PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
  DELETE TELCABS.CABS.CARRIER CLUSTER PURGE
  SET MAXCC = 0
  DEFINE CLUSTER ( NAME(TELCABS.CABS.CARRIER)          -
                   INDEXED KEYS(4 0)                   -
                   RECORDSIZE(138 138)                 -
                   SHAREOPTIONS(2 3)                   -
                   CYLINDERS(2 1)  VOLUMES(PUB001) )   -
         DATA    ( NAME(TELCABS.CABS.CARRIER.DATA) )   -
         INDEX   ( NAME(TELCABS.CABS.CARRIER.INDEX) )
/*
//STEP020  EXEC PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//INFILE   DD  DSN=TELCABS.CABS.XFER.CARRIER,DISP=SHR
//OUTFILE  DD  DSN=TELCABS.CABS.CARRIER,DISP=SHR
//SYSIN    DD  *
  REPRO INFILE(INFILE) OUTFILE(OUTFILE)
/*
```

Key and record sizes for the four masters:

| Cluster | `KEYS(len off)` | `RECORDSIZE` | Key fields |
|---|---|---|---|
| `TELCABS.CABS.CARRIER` | `KEYS(4 0)` | `(138 138)` | `CR-OCN` |
| `TELCABS.CABS.RATE` | `KEYS(18 0)` | `(619 619)` | `RT-TARIFF-CD` + `RT-RATE-ELEM` + `RT-JURIS-CD` + `RT-STATE-CD` + `RT-EFF-YYDDD` |
| `TELCABS.CABS.FACTOR` | `KEYS(14 0)` | `(76 76)` | `FC-OCN` + `FC-STATE-CD` + `FC-LATA` + `FC-EFF-YYDDD` |
| `TELCABS.CABS.CIRCUIT` | `KEYS(20 0)` | `(136 136)` | `CI-CIRCUIT-ID` |

**The rate cluster is loaded from a fixed 619-byte image**, not from
variable-length records. `RT-BAND` is `OCCURS 1 TO 24 DEPENDING ON
RT-BAND-CNT`; the generator writes every record at the maximum length with
unused band slots binary zero, and `RT-BAND-CNT` governs how many are
meaningful. `RECORDSIZE(619 619)` therefore, not `(avg max)`. If you prefer a
true variable-length cluster you must repack the file first — the COBOL does
not care either way because it never reads past `RT-BAND-CNT`.

`REPRO` requires the input to be **in key sequence**. The generator writes the
carrier, rate and factor files sorted by key. The circuit file is written in
generation order; sort it before the REPRO:

```jcl
//STEP015  EXEC PGM=SORT
//SORTIN   DD  DSN=TELCABS.CABS.XFER.CIRCUIT,DISP=SHR
//SORTOUT  DD  DSN=TELCABS.CABS.XFER.CIRCUIT.SORTED,DISP=(NEW,PASS),
//             UNIT=SYSDA,SPACE=(CYL,(10,5)),
//             DCB=(RECFM=FB,LRECL=136,BLKSIZE=27200)
//SYSIN    DD  *
  SORT FIELDS=(1,20,CH,A)
/*
```

### 9.4 GDG base

Define the GDG bases once before loading:

```jcl
//STEP010  EXEC PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
  DEFINE GDG ( NAME(TELCABS.CABS.USAGE.RAW) LIMIT(35) SCRATCH NOEMPTY )
  DEFINE GDG ( NAME(TELCABS.CABS.CONTROL)   LIMIT(35) SCRATCH NOEMPTY )
  DEFINE GDG ( NAME(TELCABS.SETL.CMDS.IN)   LIMIT(12) SCRATCH NOEMPTY )
/*
```

`JCL/CABGDGDF.jcl` in this estate already defines the full set.

### 9.5 Verifying the load

Reconcile against the manifest before running anything:

* record count — `IDCAMS PRINT COUNT(0)` on the sequential file, or
  `LISTCAT ENT(...) ALL` on the cluster (`REC-TOTAL`)
* hash totals — run `JCL/CABCTLRP.jcl`, which reads `TELCABS.CABS.CONTROL`
  and prints `CT-READ` and the four hash totals. They must equal
  `hash_minutes`, `hash_amount`, `hash_seq` and `hash_ocn` in the manifest for
  the corresponding file.

A mismatch on `hash_ocn` alone almost always means the transfer translated
the alphanumeric OCNs; a mismatch on `hash_minutes` alone means the COMP-3
fields were translated. Both mean the transfer was not binary.

---

## 10. Tests

```bash
python3 -m unittest test_gen_common -v
```

58 tests. The COMP-3 group checks three things independently:

1. **Known byte patterns** from the packed-decimal architecture, not derived
   from the implementation — `12345` unsigned is `12345F`, signed positive is
   `12345C`, signed negative is `12345D`; `S9(06)` gets a leading pad nibble
   and becomes `0123456C`; `S9(07)V9(02)` holding `123.45` is `000012345C` in
   five bytes.
2. **Round-trip identity** across digit counts 1–18, scales 0–5, positive,
   negative and zero, with the declared scale preserved on the way out.
3. **Agreement with a second, independently written implementation** that
   decodes nibble by nibble with integer shifts rather than via `bytes.hex()`.

Sign nibbles `C`, `A`, `E`, `F` are read as positive and `D`, `B` as negative.
A non-decimal digit nibble raises in strict mode — the Python equivalent of an
S0C7 — and is tolerated as zero when strict mode is off, which is what some of
the estate's own recovery paths do.

The parser tests assert the real offsets from `CABSCDR` (`CD-SEQ-NBR` at 17
for 5 bytes, `CD-VARIANT-AREA` at 54 for 96, all three variants starting at
54) and assert that the length disagreements are **reported and not
repaired**.
