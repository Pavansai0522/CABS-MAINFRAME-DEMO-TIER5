# IMS — MANIFEST

IMS DB definition source for the CABS reference layer. Read `_README.md` first — it explains the
databases, the PSB-to-program mapping, and the **two databases referenced by the programs whose
DBD/PSB source is deliberately not in this repository**.

**13 members, 626 lines.** All REFERENCE-ONLY — MVS 3.8j / Hercules TK4- has no IMS.

| File | Lines | Purpose | Complexities carried | Runnable? |
|---|---:|---|---|---|
| `CABCARDB.dbd` | 59 | CARRIER database. HDAM/OSAM, `RMNAME=(DFSHDC40,10,500,824)`. Root `CARRSEG` keyed on `CROCN`; dependents `CARRBILL`, `CARRFACT`, `CARRAGMT`. | Randomiser parameters tuned in 1997 and never revisited; HDAM means an unqualified `GN` returns roots in randomiser order, not key order — `CABCTL01` depends on a downstream sort to correct that. | REFERENCE-ONLY |
| `CABCIRDB.dbd` | 49 | CIRCUIT database. HIDAM/VSAM. Root `CIRCSEG` keyed on `CICKTID`; dependents `CIRCMPB`, `CIRCTERM`. `LCHILD` to the primary index. | HIDAM primary index is a separate database — a two-member dependency that a single-file scan will miss. | REFERENCE-ONLY |
| `CABCIRIX.dbd` | 32 | HIDAM primary index database for `CABCIRDB`. `ACCESS=INDEX`, segment `CIRCIDX`. | Exists only to support `CABCIRDB`; has no application meaning of its own. | REFERENCE-ONLY |
| `CABSETDB.dbd` | 54 | SETTLEMENT database. HDAM/OSAM. Root `SETLSEG` keyed on `STKEY` (type 1 + OCN 4 + period 6 + seq 9); dependents `SETLDTL`, `SETLDISP`. Carries `XDFLD SETLXOCN` over `STOCNPD` and an `LCHILD` to the secondary index. | **Secondary index.** A program running under `PROCSEQ` sees the same database in a different sequence and gets the *index* key in the PCB key feedback area, not the root key. `CABCTL06` depends on this. | REFERENCE-ONLY |
| `CABSETSX.dbd` | 29 | Secondary index database over `CABSETDB`, segment `SETLXSEG`, key `SETLXOCN` (OCN + settle period). | Same as above. The index is maintained by IMS from a field `CABCTL05` populates — a data dependency with no code-level link. | REFERENCE-ONLY |
| `CABBHSDB.dbd` | 41 | BILLHIST database. HISAM/VSAM, prime + overflow. Root `BHSTSEG` keyed on `BHKEY` (BAN 13 + bill period 6); dependent `BHSTDTL`. | HISAM overflow chains lengthen if the quarterly reorg is missed. `CABCTL04` inserts into it every cycle and nothing fails when the reorg is skipped — the online enquiry just slows down. | REFERENCE-ONLY |
| `CABCT01P.psb` | 22 | PSB for `CABCTL01`. `CABCARDB`, all four segments, `PROCOPT=G`. | Read-only. | REFERENCE-ONLY |
| `CABCT02P.psb` | 21 | PSB for `CABCTL02`. `CABCARDB`, `CARRSEG` + `CARRFACT`, `PROCOPT=A`. | Updater — see complexity 17 in `BATCH/CONTROL/_MANIFEST.md`. | REFERENCE-ONLY |
| `CABCT03P.psb` | 22 | PSB for `CABCTL03`. `CABCIRDB`, all three segments, `PROCOPT=A`. | Updater. | REFERENCE-ONLY |
| `CABCT04P.psb` | 18 | PSB for `CABCTL04`. `CABBHSDB`, both segments, `PROCOPT=AP`. | Updater — see complexity 17. | REFERENCE-ONLY |
| `CABCT05P.psb` | 18 | PSB for `CABCTL05`. `CABSETDB`, `SETLSEG` + `SETLDTL`, `PROCOPT=A`. | Updater — see complexity 17. | REFERENCE-ONLY |
| `CABCT06P.psb` | 22 | PSB for `CABCTL06`. `CABSETDB` with **`PROCSEQ=CABSETSX`**, three segments, `PROCOPT=G`. | The `PROCSEQ` operand is the only place in the estate that says the settlement database is read in secondary-index order. It is in the PSB, not in the program. | REFERENCE-ONLY |
| `CABIMSGN.jcl` | 239 | DBDGEN → PSBGEN → ACBGEN job. Assembles each DBD and PSB, link-edits into `IMS.DBDLIB` / `IMS.PSBLIB`, then runs `DFSRRC00` `PARM='UPB,...'` for ACBGEN. | **11/13** — the gen sequence is a hard ordering dependency (a DBDGEN without a following ACBGEN leaves the online region running the old definition). The `/DBR` and `/STA` commands around it are in the comment, not in the job. | REFERENCE-ONLY |

## Deliberate gaps recorded here for traceability

| Missing artefact | Referenced by | Notes |
|---|---|---|
| `CABRATDB` DBD/PSB | Rate/tariff DL/I access path | Not in this repository. See `_README.md` §"What is NOT here". |
| `CABBANDB` DBD/PSB | Account/BAN validation path | Not in this repository. See `_README.md` §"What is NOT here". |

These are not omissions to be corrected. They are the deliberate reproduction of a condition found
in real scans (69 programs calling IMS with no DBD/PSB in scope) and the estate is expected to
carry them.

## Documentation drift in the DBD/PSB comment boxes (deliberate)

Each DBD and PSB carries a revision history spanning 1989-2016 with at least two stale entries
per member — a segment described as being added "next release" that never was, and a randomiser
or index tuning note from 1997 that has not been revisited since. These are period-authentic and
must not be reconciled with the actual macro source.
