# Withhold for a blind run

This estate is built to be handed to a modernization effort **blind**: the reader
should be able to find the seeded defects by analysing and running the code, not
by reading about them.

Five artefacts are answer-bearing. Remove them from the copy you hand over.
Everything else in the tree is safe to give away, and
`HARNESS/test_containment.py` is the check that proves it — it fails if any
sealed sentence, or any defect-to-program attribution, appears anywhere outside
this list.

| Artefact | Why |
|---|---|
| `SEALED/` | The answer keys themselves (`answer_key_*.json`) and the placement register (`defect_placements.json`) — which program, which paragraph, what it does, what it costs and what the correct modernization response is. |
| `HARNESS/defect_signatures.json` | The machine-readable attribution rules. Names each defect and describes how it shows up in the data. `HARNESS/verdict.py` raises `BlindRunError` on it under `--blind`, by design. |
| `DATA/attributed_run/` | The output of an **attributed** comparison run — a report that has already had the answer key applied to it. Regenerate blind with `run_compare.py --blind` if you want a report in the handover. |
| `HUB/index.html` | The interactive knowledge graph. Per the deliverable spec its seeded-defect page carries `id`, `file` and `detectable_by` — no construct statement, no description, no business impact, no paragraph. That is still a list of the twelve files to look at, so it is answer-bearing for a blind candidate even though it is safe to show anyone else. `HARNESS/test_containment.py` asserts those three fields are all it carries. |
| `DOCS/AUDIT_REPORT.md` | An independent conformance audit of the build. It necessarily quotes the containment failures it found, including defect ids and paragraph names. It is a record of how the asset was assured, not part of the asset. |

`HARNESS/test_containment.py` reads this file. Adding a path to the table above
exempts it from the containment check, so the table is the thing to review — not
the test result on its own.

Two things are deliberately **not** on this list:

* `CONTRACTS/complexity_placement.json` — the 27-construct traceability register.
  All 1,613 construct placements are published on purpose; a construct is not a
  defect. The per-defect placements that used to sit in it are now in
  `SEALED/defect_placements.json`.
* `GENERATORS/gen_divergence.py` — it fabricates a candidate estate by mimicking
  known differences, but it no longer holds the mode-to-defect mapping. That
  table moved into `SEALED/defect_placements.json` and is loaded from there; with
  `SEALED/` withheld the generator reports `null` for every mode.
