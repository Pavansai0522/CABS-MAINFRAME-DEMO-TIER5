# CONTRACTS — the validation layer

**The contract is written from the legacy side, before anything is transformed,
and it is what a candidate transformation is scored against.**

---

## 1. What is in this directory

| File | What it is |
|---|---|
| `contracts.yaml` | **The Process Contract Register.** 247 processes. One entry per contracted process: what it is triggered by, what it consumes, what it produces, what it counts, what equation it must satisfy, what it guarantees on restart, and what the target state has to reproduce. |
| `contracts.json` | The same content, same generation, for anything that would rather read JSON. |
| `canonical_interchange_spec.md` | The canonical form both sides normalise into, and why neither side's native format is the reference. |
| `complexity_placement.json` | The consolidated 27-construct traceability file. Supersedes the two scope-limited placement files it was merged from. 1,613 verified placements. |
| `complexity_placement_bill_format_report.json` | Retained for provenance. Its content is folded into `complexity_placement.json`, which is authoritative. |
| `_README.md` | This file. |

Two generated workbooks sit in `../DOCS`: `Process_Contract_Register.xlsx`
(generated from `contracts.yaml`) and `Complexity_Traceability_Matrix.xlsx`
(generated from `complexity_placement.json`). Both are outputs. If they disagree
with the files above, regenerate them; do not edit them.

---

## 2. The contract-first principle

### The contract is authored first, from the legacy, by people who are not building the target

Every field in `contracts.yaml` was read out of the estate as it stands:

- the mandatory header comment block on each COBOL program, whose shape
  `CONVENTIONS.md` fixes, for purpose, application, balancing equation and
  restart guarantee;
- the `SELECT ... ASSIGN TO` clause for DD names, and the `OPEN INPUT` /
  `OPEN OUTPUT` / `OPEN EXTEND` / `OPEN I-O` statement for direction;
- the `FD` `RECORD CONTAINS` and `RECORDING MODE` clauses for length and format,
  and the `COPY` inside the FD for the layout;
- the JCL — 131 job members, 27 PROCs and 3 CTC site members — with PROCs
  expanded and symbolic parameters substituted, for datasets, dispositions,
  relative generations, conditions and symbolics;
- the 18 DFSORT control cards for what each sort step actually does to the data.

Nothing was taken from a design document, because there is no target design yet.
That is the point. **A contract written after the transform is a description of
the transform, not a test of it.** It will agree with whatever was built, because
it was read off whatever was built, and it will therefore certify a
transformation that quietly changed the invoice.

### Validation is not testing

They are different activities with different owners and different failure modes,
and conflating them is the commonest way a migration ships a wrong bill.

| | **Testing** | **Validation** |
|---|---|---|
| Question | Does the new system behave as specified? | Does the new system produce what the old system produced? |
| Reference | The specification the team wrote | The legacy, as it actually behaves |
| Who authors it | The team building the target | Someone reading the legacy |
| Written | With, or after, the build | **Before** the build |
| Passes when | The stated behaviour is met | The two sides reconcile at every process boundary, to the penny and to the record |
| Blind spot | A defect faithfully reimplemented is a pass | A legacy defect shows up as a divergence and has to be dispositioned |

Testing cannot see a requirement nobody wrote down. This estate is full of them:
the deduplication rule that exists only in `CABSRT03`, the settlement-eligibility
test that exists only in sort exit `CABSE15B`, the 45-state territory table
compiled into `CABSE15C`, the 2012 grouping change made in `CABSE35C` with no
matching change in any COBOL module. None of these is in any specification. All
of them change the invoice. Only a comparison against the legacy's actual output
finds them.

Validation cannot, on its own, tell you whether the legacy is *right*. That is
why the harness has a three-way verdict rather than pass/fail, and why
`DIVERGENT-BY-DESIGN` is scored positively (section 5).

### UNDETERMINED is a first-class value

`contracts.yaml` carries **1,964 UNDETERMINED items**, each with the reason and
what would resolve it. They are listed in full in the register and on the
*Undetermined Items* sheet of the workbook.

They are not a defect in the register. They are the finding.

- **609 dataset keys are UNDETERMINED.** The `SELECT` declares no `RECORD KEY`
  because the file is sequential, and no dataset declaration in the compare
  contract covers it. The business key of that data is not recoverable by reading
  the source. Someone who knows the business has to say what it is — and until
  they do, a comparison of that dataset cannot tell a missing record from a moved
  one.
- **514 record layouts are UNDETERMINED.** The FD names an inline `01` level
  rather than a `COPY`, so the layout is governed by no member of `COPYBOOKS/` and
  a change to it has to be made in every program that declares it. `CABCTC02`
  re-declares `CABS-RTBL-RECORD` from `CABRAT01` in nine site copies.
- **Every `expected_volume` is UNDETERMINED.** The estate states no record volumes
  anywhere; a JCL `SPACE` parameter is an allocation, not a volume. This is not
  cosmetic — at least one behaviour in this estate only appears once a sort
  spills past its string limit, so a validation run sized from guesswork will
  pass while never reaching it.
- **57 schedules are UNDETERMINED.** Scheduling lives in an external scheduler
  package that is not in the repository. Where a schedule appears in the register
  it was inferred from the job's own comment block, and the register says so.
- **9 programs have no invoking job step at all.** `CABING12` is named by no JCL
  member anywhere. The eight `BATCH/CONTROL` programs would need an IMS DLI batch
  region or an MQ batch job and neither exists here. Whether these are dead, run
  by hand, or run from a library outside this repository cannot be determined by
  reading the source.

A register with no gaps in it would be a more comfortable document and a less
useful one. Being honest about what static reading cannot recover is the part
that tells you where the human effort has to go.

---

## 3. How the register feeds the harness

```
COPYBOOKS/*.cpy ─────┐
                     ├──► GENERATORS/gen_common.py (the only copybook parser)
CONTRACTS/           │              │
  contracts.yaml ────┤              ├──► HARNESS/canonical.py ──► canonical NDJSON
  canonical_...md ───┘              │        (cabs.canonical.v1)      │  │
                                    │                                 │  │
HARNESS/contracts/compare_contract.json ──────────────────────────────┘  │
        (keys, variant rules, tolerances, process chain)                  │
                                                                          ▼
                                                        HARNESS/compare.py  L1–L5
                                                                          │
SEALED/answer_key_*.json ──► HARNESS/verdict.py ◄─────────────────────────┘
HARNESS/defect_signatures.json         │
                                       ▼
                          comparison_report.json + comparison_summary.txt
```

Concretely, each part of a contract entry lands somewhere:

| Contract field | Where it is used |
|---|---|
| `inputs[].dsn` / `outputs[].dsn` | Which physical files are paired between the two sides. |
| `inputs[].copybook` / `outputs[].copybook` | Which layout `canonical.py` decodes the record with. |
| `inputs[].key` | The business key at L1 and the join key at L2. Where it is `UNDETERMINED`, only L1-by-hash is possible. |
| `inputs[].lrecl` / `recfm` / `variable_length` | How records are framed — fixed length, or RDW-delimited VB. |
| `control_totals` | The nine `CABSCTL` fields asserted at **L3** on both sides independently. |
| `balancing_rule` | The equation asserted at L3. Where it is not the estate standard, the program's own equation is asserted as written. |
| `trigger.predecessor` | The process-chain edges: what one process wrote must be what the next one read. |
| `restart` | What a mid-stream failure is allowed to leave behind, and therefore what a rerun must be able to reproduce. |
| `compare_levels` | Which of L1–L5 this process participates in. |
| `target_state.equivalence_obligation` | The obligation a candidate has to meet for this process, in words, before anything is measured. |

Two things the register deliberately records rather than smooths over:

- **Sort steps write no control record.** All 16 of them. The contract says so on
  every one, with the balancing rule set to *NONE DECLARED*. The
  `CABRAT09 → CABBIL01` edge in the compare contract's process chain spans exactly
  such a gap. The chain is declared with the gap in it rather than pretending it
  is continuous — a divergence that arises inside a gap has no control record on
  either side to expose it.
- **The estate's own tolerances are not inherited.** `CABBIL11` applies a
  one-sided five-cent tolerance to its own account-level proof. The compare
  contract declares no tolerance on `BILLHDR` and says why. A harness that copies
  the estate's tolerances copies the estate's blindness.

---

## 4. Using the register to score a candidate transformation

### Step 0 — close the gaps you can afford to close

Before the first comparison, work the *Undetermined Items* sheet. You do not need
all 1,964; you need the ones on the datasets you are about to compare. In
practice that means: the business key for each dataset in scope, the record
layout for each inline-`01` file in scope, and a production volume for anything
whose behaviour is volume-dependent. Record the answers **in the contract**, so
the next person inherits them.

### Step 1 — freeze the contract

Tag `contracts.yaml`, `compare_contract.json` and `complexity_placement.json`
together. A comparison run against a contract that moved during the run is not a
measurement.

### Step 2 — run blind first

```bash
cd HARNESS
python3 run_compare.py --legacy L --candidate C --blind --out ../DATA/blind
```

`--blind` refuses both the answer key and the defect signatures. It will exit `1`
if there is any variance at all, because without the key nothing can be
classified as by-design. **That is a measurement, not a gate.** Running blind
first is what demonstrates the comparison engine was not tuned to the answer.

### Step 3 — run attributed, and diff the two

```bash
python3 run_compare.py --legacy L --candidate C --out ../DATA/attributed
```

The variance list must be **identical** between the two runs. Only the verdicts
change. If the variance list moves, the attribution layer is influencing the
comparison and the result is not trustworthy.

### Step 4 — score

Score against the contract, not against a feeling about the output.

**A. Coverage — how much of the estate was actually validated.**

| Measure | Source |
|---|---|
| Processes contracted and compared, of 247 | `contracts.yaml` ∩ the report |
| Datasets compared, of 205 | *Datasets* sheet ∩ the report |
| Levels that ran, and levels that did **not** | the report. A level skipped because its data did not exist is `NOT RUN`, never `MATCH`. |
| UNDETERMINED items still open on datasets in scope | *Undetermined Items* sheet |

Coverage is reported first and separately, because a clean result over 12 per cent
of the estate is not a clean result.

**B. Equivalence — did the two sides reconcile.**

| Level | Passes when |
|---|---|
| **L1** Record | The same records are present, once each, on the declared business key. No missing, no extra, no duplicate-count mismatch. |
| **L2** Field | Every field agrees, typed, exact by default. **Declared scale is compared as well as value.** |
| **L3** Control | The balancing equation in the contract holds on **both** sides for every process, and the hash totals chain across every declared edge. |
| **L4** Bill | Invoice header, per-carrier, per-BAN, per-rate-element and per-line-item money agree to the penny. |
| **L5** Settlement | Per-counterparty, per-period settlement agrees, including meet-point splits and PIU/PLU restatements. Both meet-point assertions are required, not one. |

Report signed, netted money totals as well as counts. Ten thousand variances
netting to zero and ten thousand netting to −$400 are entirely different
situations. Money totals cover scaled fields only — a difference of 97,270 on
`CD-VC-ORIG-NPANXX` is a different NPA-NXX, not ninety-seven thousand dollars.

**C. Construct handling — did the transformation deal with what is actually
there.**

Take `complexity_placement.json` and ask, per construct, what the candidate did
with it. A transformation that produced identical output but silently discarded a
construct has taken on risk that has not been paid for yet. In particular:

| Construct | The question to ask |
|---|---|
| 2 Dynamic call (12 sites) | Are all eight dispatch targets present, and is the target chosen from data at run time rather than hardwired? |
| 7 Variable-length records (28) | Does `BD-ELEM-CNT` survive, with every occurrence, in order? |
| 14 Sort utility logic (26) | Have the filter, grouping, summarisation and rounding rules from the 18 control cards **and** the 8 sort exits been restated as explicit stages — or has "run a sort" been reimplemented as "run a sort"? |
| 17 Two stores, one update (6) | What happens now on a failure between the two writes? A single transaction is a **behaviour change**, and a welcome one, but it must be declared. |
| 19 Two-digit year (134) / 25 Julian dates (208) | Is the pivot still 70 everywhere, and what happened to the `68` in the emergency assembler module? |
| 21 Rounding rules (77) | The estate rounds inconsistently **on purpose**. Has each instance been matched, or has the target imposed one house rule? |
| 26 Dead code (12) / 27 Dormant feature (7) | Removed, retained, or reactivated? Reactivating a dormant feature is not a refactor; `CABRAT13` prices a tariff withdrawn in 1998. |

**D. Defect disposition — the three-way verdict.**

| Verdict | Meaning | Scored |
|---|---|---|
| `MATCH` | The two sides agree. | — |
| `DIVERGENT-BY-DESIGN` | The variance traces to a known defect the legacy has been carrying. | **Positively.** The transformation surfaced something real. |
| `DIVERGENT` | A variance with nothing behind it. | **Blocks.** |

`DIVERGENT-BY-DESIGN` is **not** a pass and must never be silenced with a
tolerance. It means a business decision is now due — revenue assurance,
regulatory, restatement — not a code change. Several of the seeded defects cannot
be "fixed" during a migration without changing what carriers are billed, which is
a rate change in substance.

A **missed** defect is a statement about the validation, not about the candidate.
The report lists missed defects with their `detectable_by` classification, so
`Dnn … (parity_test_at_volume)` reads as *this needs the volume profile* and
`Dnn … (parity_test_at_element_volume)` reads as *this needs the input shaped,
not enlarged* — neither of them reads as *this is fine*.

**E. The things this instrument cannot score at all.**

Say so explicitly in the report rather than leaving a silence:

- **Which copy of a multi-site program is authoritative.** `CABCTC01` is deployed
  at twelve sites and `CABCTC02` at nine; run JCL exists for three. Nothing in the
  estate identifies the correct copy and no rule can derive one. Four business
  questions have to be answered by a person with authority over wholesale
  settlement before that family can be migrated at all. See
  `CTC/_MANIFEST.md` section 5.
- **Whether an UNDETERMINED value was guessed correctly.** If a key was supplied
  by a person, the comparison is only as good as that person.
- **Whether a legacy behaviour ought to be preserved.** The harness reports that
  it was or was not. It cannot tell you whether it should have been.

---

## 5. Rules for maintaining this directory

1. **`contracts.yaml` is generated, not edited.** It is derived from the source.
   If it is wrong, either the source changed or the derivation is wrong; fix the
   one that is actually at fault and regenerate.
2. **A resolved UNDETERMINED goes back into the contract**, with the name of who
   supplied it and when. An answer that lives only in someone's head is not an
   answer.
3. **`complexity_placement.json` is authoritative for placements.** Per
   `CONVENTIONS.md`, a construct added to the source must be added here, or the
   traceability matrix stops being true.
4. **The answer keys stay sealed.** `SEALED/answer_key_*.json` is touched by
   `verdict.py` alone, after the comparison, never inside it. The *Seeded Defects*
   sheet in the traceability workbook carries id, file, construct and
   `detectable_by` only, so it can be handed to a team being asked to find the
   defects without giving them the answers.
5. **A tolerance requires a reason.** `test_canonical.py` fails the build if one
   is declared without it. This is the rule that keeps the estate's own blindness
   out of the validation.
