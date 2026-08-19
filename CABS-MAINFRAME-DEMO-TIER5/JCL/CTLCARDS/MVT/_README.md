# JCL/CTLCARDS/MVT — OS/360 Sort/Merge equivalents

**17 control card members.** Parallel to the 18 members in the parent directory. Same member
names. Nothing in the parent directory was renamed, moved or edited.

---

## 1. Why there are two sets

`JCL/CTLCARDS/` holds the cards the estate actually runs today. They are written for a sort
product that supports `INCLUDE`, `OMIT`, `SUM`, `INREC`, `OUTREC`, `OUTFIL` and `FIELDS=COPY`.

Of the eighteen members, exactly one — `CABSRT04` — is executable on MVT-era OS/360 Sort/Merge.
The other seventeen use at least one control statement that the target sort does not have:

| Statement used | Members that use it |
|---|---|
| `INCLUDE COND=` | `CABSRT01`, `CABSRT06`, `CABSRT08`, `CABSRT09`, `CABSRT13`, `CABSRT18` |
| `OMIT COND=` | `CABSRT02` |
| `SUM FIELDS=` | `CABSRT03`, `CABSRT07`, `CABSRT10`, `CABSRT16` |
| `INREC` / `OUTREC` | `CABSRT05`, `CABSRT17` |
| `OUTFIL` | `CABSRT14` |
| `FIELDS=COPY` | `CABSRT13` |
| `OPTION EQUALS` | fifteen of the eighteen |
| `OPTION VLSHRT` | `CABSRT11` |

This is not an error in the estate and it has not been treated as one. **Real estates carry
control cards written for a sort product that a target platform may not have.** The cards in the
parent directory are the historical record of what the business rules are and where they were
written down. They stay exactly as they are.

This directory is the parallel runnable set. For each of the seventeen non-conforming members
there is an OS/360 Sort/Merge equivalent that uses only four statements:

```
 SORT FIELDS=(pos,len,fmt,seq,...)      or  MERGE FIELDS=(...)
 RECORD TYPE=F,LENGTH=(nnn)             or  TYPE=V,LENGTH=(max,,,min,max)
 MODS E15=(name,nnnn,SORTEXIT,N),E35=(name,nnnn,SORTEXIT,N)
 END
```

Every rule that the DFSORT card expressed declaratively — every `INCLUDE`, `OMIT`, `SUM`,
`INREC`, `OUTREC`, `OUTFIL`, `FIELDS=COPY` and every dependency on `OPTION EQUALS` — has been
**moved into an E15 or E35 user exit module** in `SORTEXIT/`.

`CABSRT04` has no member here. It already conforms.

---

## 2. Mapping — DFSORT card to MVT card and exit

| Member | DFSORT statement removed | The business rule it carried | Now held in | Exit type |
|---|---|---|---|---|
| `CABSRT01` | `INCLUDE COND=` | Keep record types 01-08; keep only source system codes 03 (EMI gateway), 05 (CRIS feed), 07 (mediation). Everything else dropped silently with no suspense entry. | `CABSXSRC` | E15 |
| `CABSRT02` | `OMIT COND=` | Drop records with a fatal edit status of 6 through 9 — they were already suspended upstream and validating them again would double-suspend them. | `CABSXEDT` | E15 |
| `CABSRT03` | `SUM FIELDS=(120,7,PD)` | The estate's only deduplication-by-summing rule: collapse duplicate OCN / BAN / circuit / USOC keys into one record, adding conversation minutes. | `CABSXMIN` | E35 |
| `CABSRT05` | `OUTREC FIELDS=` | Reformat, and zero the retry count at bytes 165-167 on every record so a rerun does not inherit a stale count. No COBOL module in the rating family resets that field. | `CABSXRTY` | E35 |
| `CABSRT06` | `INCLUDE COND=` on a **merge** | Keep only the current billing cycle. The cycle literal was hand-maintained on the card every bill period. | `CABSXCYC` | E35 |
| `CABSRT07` | `SUM FIELDS=(160,7,PD)` **plus** the existing `MODS=(E35=(CABSE35B,...))` | Summarise by OCN / BAN / jurisdiction, **then** apply the rate-table round rule to the summed amount. One card cannot chain two E35 exits, so one module now does both. | `CABSXSUM` | E35 |
| `CABSRT08` | `INCLUDE COND=(40,1,CH,NE,C'B')` | Keep only control records that did not balance. | `CABSXBAL` | E15 |
| `CABSRT09` | `INCLUDE COND=(60,1,CH,NE,C'C')` | Drop closed accounts before the bill trigger sees them. They are therefore not counted in the trigger's skip reason S2. | `CABSXACC` | E15 |
| `CABSRT10` | `SUM FIELDS=(37,8,PD,45,8,PD)` | Collapse duplicate BAN / period / section / line-seq / element-seq keys, adding quantity and amount. No COBOL program in the estate knows duplicate element sequences are possible. | `CABSXDTL` | E35 |
| `CABSRT11` | `OPTION EQUALS,VLSHRT` | Short-record tolerance on a variable-length file. The exit now deletes anything under 108 bytes rather than let the sort take a key out of a truncated record. It also re-derives the jurisdiction byte from state and LATA. | `CABSXJUR` | E15 |
| `CABSRT12` | `OPTION EQUALS` | Line order within a print document. The exit now builds a line ordinal into the 20-byte sort key it lays over each line, and the output exit strips it and applies the burst rule. | `CABSXZIP` + `CABSXBST` | E15 + E35 |
| `CABSRT13` | `FIELDS=COPY` **and** `INCLUDE COND=` | The four live trading-partner codes in tonight's EDI interchange. MVT Sort has no copy-only mode, so the card carries a real key and the partner list moved into the exit. | `CABSXEDI` | E15 |
| `CABSRT14` | `OUTFIL OMIT=` | Drop zero-amount detail records from the tape despatch. Labels and trailers exempt. | `CABSXTAP` | E35 |
| `CABSRT15` | `OPTION EQUALS` | "Keep the last attempt for each process." The card relied on input order within equal keys. Without `EQUALS` the exit decides the survivor from the rerun number and the cycle date on the two records. | `CABSXLST` | E35 |
| `CABSRT16` | `SUM FIELDS=NONE` and `OPTION EQUALS` | Exact-duplicate elimination across four suspense generations, deliberately excluding the run id from the comparison so the survivor carries the **oldest** run id and the ageing report reports first suspension. | `CABSXDUP` | E35 |
| `CABSRT17` | `INREC FIELDS=` and `OUTREC FIELDS=` | The ledger company is not a field — it is the first two bytes of the invoice number. One exit moves it to the front so the sort can key on it; the other strips it back off so the close program reads the layout it has always read. | `CABSXLDG` + `CABSXLDR` | E15 + E35 |
| `CABSRT18` | `INCLUDE COND=(74,11,...)` | Select bill-proof records whose **result text** reads `OUT OF BAL ` — the test is on the text written upstream, not on the difference field. | `CABSXOOB` | E15 |

**19 exit modules were written to carry these rules.** Four of them — `CABSXJUR`, `CABSXZIP`,
`CABSXBST`, `CABSXLST` — were already named by `MODS` operands on `CABSRT11`, `CABSRT12` and
`CABSRT15` in the parent directory but had never had source in the tree. The other fifteen are
new and exist only because a declarative rule had to go somewhere.

Full descriptions are in `SORTEXIT/_MANIFEST.md`.

---

## 3. What changed in the sort keys

The `SORT FIELDS=` / `MERGE FIELDS=` key lists are byte-for-byte identical to the originals
everywhere except two members:

- **`CABSRT13`** had `SORT FIELDS=COPY`. MVT Sort has no copy-only mode, so the MVT member carries
  a real key of `(9,4,CH,A,1,8,CH,A)`. The consequence is that the output is no longer in input
  order. Nothing downstream of the EDI interchange filter has been shown to depend on input
  order, but the change is a change and is recorded here rather than left to be discovered.
- **`CABSRT17`** keeps the original positions `(1,2,CH,A,17,6,CH,A)` because `CABSXLDG` reshapes
  the record before the sort sees it, exactly as `INREC` did. The record length on the card is
  402 rather than 400 for the same reason.

`OPTION EQUALS` is gone from every member. Fifteen of the eighteen originals carried it. In three
of those fifteen — `CABSRT12`, `CABSRT15` and `CABSRT16` — the *result* genuinely depended on it,
and in all three cases the guarantee has been rebuilt inside the exit rather than left to the
sort. The MVT card for each of the three states which exit now holds it.

---

## 4. The modernization implication

This is the point of the construct, and it is worth stating plainly.

**A transformation that reads only the control cards will miss this logic.** After the move, the
MVT cards are four lines of sort keys, record lengths and module names. There is no `INCLUDE`
to parse, no `SUM` to map to a `GROUP BY`, no `OUTREC` to map to a projection. A control-card
analyser reports the members as trivial sorts.

**A transformation that reads only the COBOL will also miss it.** None of these rules is coded in
any COBOL program in the estate. `CABING01` through `CABING12` do not filter on source system.
`CABRAT09` does not round the summary. `CABBIL01` does not exclude closed accounts. `CABBIL02`
does not deduplicate element sequences. `CABFMT06` writes EDI segments for every carrier. The
programs are, individually, correct and complete readings of themselves.

The rules now live in a **third place that neither analysis path traverses**: 19 load modules in
`TELCABS.CABS.SORTEXIT`, referenced only by the `MODS` operand of a control card, invoked only by
the sort program, and never named in a `CALL` statement anywhere in the estate.

The call graph does not contain them. The data lineage does not contain them. A static analyser
following `CALL` and `COPY` and `EXEC PGM=` will not reach them. The only path that finds them is:

```
JCL step  ->  SYSIN DD  ->  control card member  ->  MODS operand  ->  load module name
          ->  source member in the sort exit library
```

— four indirections, three of which are text in a data file rather than a language construct.

### What this means in practice

| | Cards only | COBOL only | Both, without the exits |
|---|---|---|---|
| Record-type and source-system filter | missed | missed | missed |
| Fatal-edit-status suppression | missed | missed | missed |
| Two of the four deduplication rules | missed | missed | missed |
| Summary-level rounding | missed | missed | missed |
| Closed-account exclusion | missed | missed | missed |
| Trading-partner list | missed | missed | missed |
| Zero-amount tape suppression | missed | missed | missed |
| Retry-count reset | missed | missed | missed |
| Keep-the-last rerun rule | missed | missed | missed |
| Jurisdiction re-derivation | missed | missed | missed |
| Print-stream burst rule | missed | missed | missed |

Eleven distinct business rules. In the DFSORT set, six of them were at least visible as text on a
control card even if no analyser parsed them. After the move, none of them is.

### The recommended response

1. **Enumerate the exits first, from the `MODS` operands, before any code analysis begins.** The
   control cards are the only index that exists.
2. **Treat every exit module as a first-class source artefact**, not as a build detail. Each one
   is a pipeline stage in the target design.
3. **Re-specify, do not translate.** The straddling-LATA table, the approved source-system list,
   the 400-line document buffer, the trading-partner codes, the hand-maintained cycle literal and
   the round-rule alphabet are constants in source that were never written down as requirements.
4. **Reconcile the two card sets before decommissioning either.** The DFSORT set is the record of
   *what the rule is*. The MVT set plus the exits is the record of *where the rule executes*.
   Neither alone is the specification.
5. **Expect the counts not to tie.** Several exits drop records and report the drop to SYSOUT
   only. Those drops never reach a `CABS-CONTROL-RECORD`, so a run that removes material volume
   still satisfies `CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED + CT-CARRIED-FWD`.

---

## 5. Runnable status

The MVT cards are syntactically executable on MVS 3.8j Sort/Merge. The exit modules they name are
**reference-only** for the same three reasons the original eight exits are — they are Enterprise
COBOL rather than OS/VS COBOL 1974, they are LE-enabled rather than statically link-edited with an
OS save-area convention, and two of them open a QSAM file from inside the exit and would need the
DD allocated on the sort step. See `SORTEXIT/_MANIFEST.md` for the detail.
