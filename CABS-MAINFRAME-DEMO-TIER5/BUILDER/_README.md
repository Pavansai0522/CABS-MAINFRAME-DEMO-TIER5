# BUILDER — how the utility tier was produced, and why

## What is in here

| File | What it is |
|---|---|
| `build_families.py` | The generator. Documented, parameterised, deterministic. |
| `_MANIFEST.md` | What the last run produced — every program, its size, paragraph count, REDEFINES count, OCCURS limit, field inventory, copybook usage and the job step that runs it. Generated; do not hand-edit. |
| `_README.md` | This file. |

## What it produces

95 OS/VS COBOL programs into `BATCH/UTIL/` and 54 JCL members into `JCL/`, roughly 54,700 lines of COBOL and 2,900 lines of JCL, across four utility archetypes:

| Code | Archetype | Programs |
|:--|:--|--:|
| `RT` | Rate-table maintenance — `CABURTnn` | 24 |
| `EX` | Dataset extract — `CABUEXnn` | 27 |
| `XR` | Cross-reference report — `CABUXRnn` | 22 |
| `CV` | File conversion — `CABUCVnn` | 22 |

A job runs one to three of these programs, which is why there are fewer JCL members than programs. That is also how the real estate is arranged — utility work is bundled into a job, not given a job each.

## Why this tier was generated and the rest was not

The estate is deliberately built in two tiers, and the line between them matters.

**Hand-authored.** `BATCH/INGEST`, `BATCH/RATING`, `BATCH/BILLCALC`, `BATCH/JURIS`, `BATCH/SETTLE`, `BATCH/FORMAT`, `BATCH/REPORT`, `BATCH/CONTROL`, the `CTC/` multi-site family, and the whole reference layer (`ONLINE/`, `DB2/`, `IMS/`, `PLI/`, `HLASM/`, `REXX/`, `SORTEXIT/`, `VSAM/`). Every one of these carries specific constructs from the 27-complexity catalogue in a specific paragraph, and every placement is traceable in `CONTRACTS/complexity_placement.json`. Several carry seeded defects recorded in `SEALED/`. Those things cannot be generated: a hidden error handler reached by `GO TO` from three sites, a fall-through paragraph missing its `EXIT`, a rounding rule that disagrees with the program downstream of it, a dormant feature behind a switch the JCL has substituted `N` into since 1994 — each of those has to be placed by hand, in a paragraph that makes business sense, and then documented. A generator that produced them would produce them uniformly, and uniform complexity is not what an estate looks like.

**Generated — this tier.** The utility programs carry no catalogued complexity. What they carry is *volume*, and volume is itself a property of the estate that has to be represented honestly. In the client scan, this tier was the bulk of the file count and roughly half the lines. Leaving it out would understate the estate; hand-authoring 95 more programs that carry nothing interesting would spend days for no analytical gain.

So: hand-author what has to be understood, generate what only has to be there.

## The thing this generator is most careful about

**It must not produce clones.** If the utility tier came out as near-copies of a template, CAST Imaging clone detection would light up on 95 programs and report a duplication problem the estate does not have. That would be a false signal, and it would then have to be explained away in front of the client — which is worse than not having the tier at all, because it undermines every other finding in the report.

Every structural property is therefore randomised per program from a seed derived from the program name:

- **Working-storage tag.** Each program gets a unique two-character tag, so no two programs share a single field name. This is the single biggest lever.
- **Field inventory.** Counter, quantity, amount, text, subscript and switch counts all vary independently. Range across the tier: 77 to 220 declared items.
- **REDEFINES.** 2 to 10 per program, over the input record and the parm card.
- **`OCCURS` table size.** 50 to 750 entries, drawn per program.
- **File count.** 3 to 9 FDs — one to three inputs, one to three outputs, optional suspense, optional print, mandatory `CTLOUT`.
- **Copybook usage.** `CABSWRK` always (mandatory), plus zero to two archetype-appropriate copybooks, plus `CABSPRNT` where the program prints. 1 to 4 per program.
- **`CALL` pattern.** 3 to 12 calls, 3 to 7 distinct targets, drawn from the shared subroutine set.
- **Paragraph inventory.** 31 to 81 paragraphs. Names are composed from per-archetype verb and noun pools, so no two programs share a paragraph-name sequence — the builder asserts this and fails the run if two do.
- **Paragraph bodies.** Drawn from a pool of seventeen body generators (accumulate, range edit, table search, `STRING` assembly, `INSPECT`, `UNSTRING`, switch cascade, percentage, collar, absolute, key compare, detail write, print line, suspense write, hash, external call, reset), shuffled per program and instantiated against that program's own field names.
- **Mandatory boilerplate.** `P8100-BUILD-CONTROL-REC` is a set of independent `MOVE` statements, and the builder shuffles their order — which is exactly what happens in a real estate where twenty different people wrote the same paragraph. `P8200-CHECK-BALANCE` has four variants. `P1100-OPEN-FILES` has five message wordings and three `DISPLAY` placements. The audit report has four label wordings and may be printed in a different order.
- **Size.** Each program is given a target line count from its size band and the builder adjusts paragraph counts and rebuilds until it lands within twenty lines of it.

Measured across the 95 programs after generation:

| Measure | Result |
|---|---|
| Duplicate paragraph-name sequences | none |
| Longest identical consecutive run between any two programs | 35 lines |
| Mean longest identical run across all 4,465 pairs | 17.3 lines |
| Highest line-level similarity between any two programs | 0.60 |
| Distinct working-storage tags | 95 of 95 |

The residual similarity is the mandatory control boilerplate — `P8000-CONTROL`, `P8100`, `P8200`, `P8300` and the open-file block — which `CONVENTIONS.md` requires every program in this estate to carry. That similarity is real, it is present in the hand-authored families too, and it is a true finding about the estate rather than an artefact of generation.

## Size distribution

Follows the observed real-estate distribution. **Do not level this out** — a flat size distribution is one of the tells that an estate has been generated rather than accumulated.

| Band | Programs |
|---|--:|
| under 200 lines | 0 |
| 200–500 | 56 |
| 500–1,000 | 32 |
| 1,000–2,000 | 7 |
| over 2,000 | 0 |

The floor of roughly 380 lines is not arbitrary. A CABS batch program cannot be shorter, because `CONVENTIONS.md` requires the `P0000-MAINLINE` shape, the full `P8000-CONTROL` block writing `CABS-CONTROL-RECORD`, the `COPY CABSWRK`, the positional parm card and the period-authentic header. That floor is a property of the standard, and the hand-authored families show the same floor.

## What every generated program conforms to

- OS/VS COBOL (1974): no `EVALUATE`, no `INITIALIZE`, no inline `PERFORM`, no scope terminators, no reference modification.
- `PERFORM para THRU para-EXIT`; periods terminate statements.
- Fixed format; nothing beyond column 72; `05` items at column 12, `10` and `88` items at column 16, `PIC` aligned at column 43 — the same layout as the hand-authored families.
- `P0000-MAINLINE` performing `P1000-INIT`, `P2000-PROCESS` until `WS-EOF`, `P8000-CONTROL`, `P9000-TERM`, `STOP RUN`.
- `P8000-CONTROL` present in every program, writing `CABS-CONTROL-RECORD` to DD `CTLOUT` and setting `CT-BAL-IND` from the balancing equation.
- `COPY CABSWRK` in every program.
- Header comment block with a revision history spanning 1987–2019.
- **No comment names its own construct or admits a defect.** The comment pools are period-authentic operational prose. Nothing in the generated source says "generated", "template", "dead code" or a complexity number.

These were verified after the run: zero lines beyond column 72, zero forbidden constructs, zero reference modification, and `P0000-MAINLINE`, `P8000-CONTROL` and `COPY CABSWRK` present in all 95.

## How to regenerate

From the estate root:

```
python3 BUILDER/build_families.py
```

Options:

```
--root PATH        estate root (defaults to the parent of BUILDER/)
--programs N       number of COBOL programs (default 95)
--jobs N           number of JCL members (default 55)
--seed N           master seed (default 19870311)
--dry-run          report what would be produced, write nothing
```

The build is **deterministic**. Rerunning with the same seed overwrites the same files with the same bytes, so a rebuild is a no-op in version control. Changing the seed produces a different but equally valid tier — useful if a second, differently-shaped estate is ever needed for a comparison exercise.

Programs are written to `BATCH/UTIL/` and JCL to `JCL/CABU7nnn.jcl`. The builder rewrites `BUILDER/_MANIFEST.md` on every run.

If the program count is raised beyond 100, `TAG_POOL` in the source has to be extended — the builder fails loudly rather than reusing a tag, because reusing a tag would reintroduce shared field names and with them the clone signal this generator exists to avoid.
