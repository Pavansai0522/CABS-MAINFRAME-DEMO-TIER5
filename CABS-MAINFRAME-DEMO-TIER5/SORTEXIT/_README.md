# SORTEXIT — OS/360 Sort/Merge user exits

**This folder is the single most consequential blind spot in the estate.**

Twenty-seven programs here carry production business rules — which records enter a file, which are
thrown away, how usage is aggregated, and how fractional cents are treated — and **not one of
them appears in any COBOL call graph, any `EXEC PGM=` statement, or any `CALL` literal
anywhere in the estate.** They are reached only through the `MODS=` operand of a Sort control
card. A reverse-engineering exercise that follows programs and JCL EXEC statements will not
find them, and a reader who reads only the COBOL will form a materially wrong picture of what
the batch stream does.

---

## 1. What an E15 and an E35 actually are

OS/360 Sort/Merge (and every descendant of it — DFSORT, SyncSort, and the `SORT` program that
ships with MVS 3.8j) allows the caller to hook user-written modules into fifteen defined points
in the sort. Two of those points matter here:

| Exit | Point in the sort | What it can do |
|---|---|---|
| **E15** | Entered once for **every record read from `SORTIN`**, before the record enters the sort | Alter the record, delete it, insert extra records, or substitute itself for `SORTIN` entirely |
| **E35** | Entered once for **every record leaving the final merge**, before the record is written to `SORTOUT` | Alter the record, delete it, insert extra records, summarise, or terminate the sort |

They are declared on the Sort control card, not in the JCL:

```
 SORT FIELDS=(5,4,CH,A,40,1,CH,A,50,3,CH,A)
 MODS=(E15=(CABSE15A,4096),E35=(CABSE35A,4096))
 OPTION EQUALS
 RECORD TYPE=F,LENGTH=200
```

`MODS=` names the load module and the amount of storage the sort must acquire for it. The
module is loaded from the `STEPLIB`/`JOBLIB` concatenation of the sort step, exactly like any
other load module — but nothing in the JCL says so. The only textual link between the control
card and the source is the eight-character module name inside the `MODS=` operand.

### Calling convention

Both exits are entered by the sort with **register 1 addressing a parameter list of fullwords**:

**E15** — two words:
```
   +0   A(input record)      or binary zero when SORTIN is exhausted
   +4   A(record length halfword)   (used for variable-length input only)
```

**E35** — three words:
```
   +0   A(record leaving the final merge)   or binary zero at end of merge
   +4   A(record most recently written to SORTOUT)
   +8   A(record length halfword)
```

The exit replies in **register 15** (the COBOL `RETURN-CODE` special register):

| RC | E15 meaning | E35 meaning |
|---|---|---|
| 00 | No action — sort takes the record unchanged, take next | No more records to insert — take next |
| 04 | **Delete the record** | **Delete the record — do not write it to `SORTOUT`** |
| 08 | Return the altered/inserted record addressed by word 1, then re-enter | Write the record addressed by word 1, then re-enter with the same input |
| 12 | Do not enter this exit again for the rest of the sort | Same |
| 16 | Terminate the sort with a user completion code | Same |

Three consequences of this convention drive everything in this folder:

1. **The module is loaded once and stays resident for the whole sort step.** WORKING-STORAGE
   therefore persists across entries. Every counter, table and accumulator in these
   twenty-seven programs relies on that. There is no `INITIAL` attribute and no `CANCEL`.
2. **Return code 08 causes a re-entry with the *same* input record.** Any exit that inserts a
   record (a control break total, a header, a trailer, a residue adjustment) must hold its own
   state across that re-entry by hand. That is why `CABSE35C` carries `WS-PENDING-BREAK-SW`
   and `CABSE35D` carries `WS-INSERT-STATE`.
3. **A final entry with a null record address** is the exit's only opportunity to flush
   whatever it has accumulated. If it does not insert there, the accumulation is silently lost.

---

## 2. Why static analysis misses all of this

| The link that exists | Why a scanner does not follow it |
|---|---|
| `MODS=(E15=(CABSE15A,4096))` inside a `//SYSIN DD *` or a `.ctl` member | The control card is **data**, not JCL and not code. Most parsers treat instream SYSIN as an opaque blob and PDS members with unknown suffixes as unclassified text. Even a parser that reads the card has to know Sort control-card grammar to recognise `MODS=` as a module reference. |
| The exit module in `TELCABS.COMMON.LOADLIB` | Resolved by the **loader at run time** from the STEPLIB concatenation. There is no `EXEC PGM=CABSE15A` and no COBOL `CALL 'CABSE15A'` anywhere. |
| The exit's effect on the data | Expressed as a return code in `RETURN-CODE`, interpreted by the sort. Nothing downstream reads a flag or a field that says a record was dropped. |
| The exit's own accounting | Written with `DISPLAY` to `SYSOUT`. It is **not** written to a `CABS-CONTROL-RECORD` on `CTLOUT`, so the estate's balancing framework never sees it. |

The estate's own control framework makes this worse rather than better. Every COBOL program
must satisfy `CT-READ = CT-WRITTEN + CT-REJECTED + CT-SUMMARISED + CT-CARRIED-FWD`. A sort
step writes no control record at all. So when `CABSE15D` suppresses 40,000 zero-value lines,
or `CABSE15B` removes every IXC's minutes from the settlement aggregation, **the run still
balances** — the program on the far side of the sort simply reads fewer records and reports
the smaller number as its own `CT-READ`. There is no reconciliation point at which the
difference becomes visible.

---

## 3. What is actually decided in this folder

| Module | Type | Business rule that lives here and nowhere else |
|---|---|---|
| `CABSE15A` | E15 | Reformats the 200-byte CDR into the rating work layout — **the sort key positions on `CABSRT04` only line up after this exit has run**. Drops any record whose rate element code is still spaces. Those records are not suspended; they are gone. |
| `CABSE15B` | E15 | Carrier-type selection for settlement. Loads a carrier-type table from `CARRTYPE` and drops every record for a carrier that is not a settlement party, is unknown, or is outside its agreement term. The carrier master holds the same indicator and the aggregation step never reads it. |
| `CABSE15C` | E15 | Jurisdictional inclusion. Drops local traffic before the access split; **rewrites an indeterminate jurisdiction to interstate in place**; drops intrastate traffic outside a 45-state territory list that is maintained in source. |
| `CABSE15D` | E15 | Zero-usage suppression, with a threshold of 0.01 minutes / $0.005 — not zero. Exempts setup, credit and make-up lines. The suppressed value is accumulated and reported to SYSOUT only. |
| `CABSE35A` | E35 | Stamps the 12-byte "rating control prefix" into bytes 189-200. No copybook describes this area and no COBOL program references it. It carries the run stamp and the **sort work-dataset ordinal** that `CABSE35B` later depends on. |
| `CABSE35B` | E35 | Applies `RT-ROUND-RULE` to the **summed** amount after `SUM FIELDS` has collapsed the keys, and accumulates the fractional-cent residue, releasing it as one adjustment record at end of merge. A reviewer reading `CABRAT09` alone will conclude the summary is unrounded. It is not. |
| `CABSE35C` | E35 | Full control-break summarisation. Deletes every detail record and emits one total per OCN/BAN/period/jurisdiction group. **The rate element was deliberately removed from the control group in 2012** — a grouping change made in a sort exit, with no corresponding change in any COBOL module. |
| `CABSE35D` | E35 | Cuts the industry-format header and trailer per receiving RAO, including the detail count and amount hash the receiving carrier balances against. `CABSET07` never sees these records and cannot reproduce them. |

---

## 4. Reading the exits safely

- **Do not assume the record layouts in the LINKAGE SECTIONs match a copybook.** They are hand-
  maintained views of the same bytes, and in several cases (`CABSE15A`, `CABSE35B`) they are
  views of a layout that only exists *after* another exit has rearranged it.
- **Check `RETURN-CODE` on every path.** The business rule is the return code, not the code
  around it. A paragraph that computes carefully and then falls through to `MOVE 4 TO
  RETURN-CODE` has thrown the record away.
- **Trace WORKING-STORAGE across entries.** Anything that is not reset in a paragraph that runs
  every entry is an accumulator, and accumulators in these programs are load-bearing.
- **Trace the null-record-address path.** That is where flushes happen. An accumulator that
  reaches that path holding a stale or reset value produces a silently wrong flush.

## 5. Modernization implication

Rehosting or recompiling the COBOL alone reproduces none of this. Any target-state design must
begin by extracting, from these twenty-seven modules and the thirty-five control-card members
across `JCL/CTLCARDS/` and `JCL/CTLCARDS/MVT/`, the complete set of filter, transform,
grouping and rounding rules, and restating them as explicit, testable pipeline stages. The rules must be *re-specified*, not
translated: several of them (the territory list, the string limit, the zero thresholds, the
2012 grouping change) are constants in source that were never written down as requirements.
