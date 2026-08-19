# PLI/ — Manifest

All members in this folder are **REFERENCE-ONLY**. They are written in OS PL/I (Optimizing Compiler /
OS PL/I V2 era syntax) and are not part of the OS/VS COBOL batch estate that runs under Hercules
TK4-/MVS 3.8j. They represent the PL/I layer of the estate — rate table maintenance, settlement
reconciliation reporting, factor validation and legacy record conversion — that a real 1980s-2010s
telecom billing shop would have carried alongside its COBOL batch stream, typically maintained by a
smaller, longer-tenured team than the COBOL applications.

| File | Lines | Purpose | Complexities carried | RUNNABLE / REFERENCE-ONLY |
|---|---|---|---|---|
| `CABRTMNT.pli` | 422 | Rate table maintenance utility - applies A/C/D transactions against the rate KSDS, edits effective-date overlap, prints an audit listing | none placed here | REFERENCE-ONLY |
| `CABSTREC.pli` | 320 | Settlement reconciliation report - match-merges our settlement master against a counterparty netting file, reports variance and unmatched items | none placed here | REFERENCE-ONLY |
| `CABFCVAL.pli` | 317 | Factor validation routine - range, consistency and quarterly-sequence edits on PIU/PLU/PSU factors | none placed here (rounding divergence noted below) | REFERENCE-ONLY |
| `CABLGCNV.pli` | 293 | Legacy 1987 usage record conversion (133-byte to 200-byte CABSCDR) | **Complexity 26 - dead code** (see below) | REFERENCE-ONLY |

## CABLGCNV — complexity 26, dead code

`CABLGCNV.pli` converts the pre-1988 133-byte usage record format into the current 200-byte `CABSCDR`
layout used by the ingest family. Its header comment describes it, in period-authentic terms, as
retained "for the remaining sites on the 1987 format" and states it is invoked from the main ingest
control stream, `BATCH/CONTROL/CABCTL04.cbl`, when a legacy-format switch is set for an incoming feed.

That switch is never set. `CABCTL04.cbl`'s legacy-format indicator is a hardcoded `'N'` that no
transaction, parameter card or control table row in this estate ever moves to `'Y'`. Every site that
originally submitted usage on the 1987 format converted to the current 200-byte format at the 1988
cutover documented in `CABLGCNV.pli`'s own revision history (`V1.02 1988-09-30`) — the program has had
no live caller for over three decades, but nothing in its source, its JCL, or its comments says so. This
is the same shape as the other two dead-code instances placed in the COBOL estate
(`CONTRACTS/complexity_placement.json`, complexity 26): a program that still compiles, still runs
correctly if invoked directly, and is walked past by every real production path.

## CABFCVAL — rounding convention divergence

`CABFCVAL.pli` validates PIU/PLU/PSU factors at their full five-decimal precision (`FIXED DEC(8,5)`,
matching `COPYBOOKS/CABSFCTR.cpy`) for the range, consistency and quarterly-sequence edits, but the
exception record it writes (`EX_PIU_RPT`, `EX_PLU_RPT`, `EX_PSU_RPT`, all `PIC '9V999'`) rounds each
factor to three decimals before writing it — matching the Factor Operations review spreadsheet import
format rather than the validation precision. A factor that fails an edit by a margin only visible in
the fourth or fifth decimal place will show as passing when a reviewer eyeballs the rounded value on
the exception listing. This mirrors the estate-wide convention, documented in `CONVENTIONS.md`, that
rounding behaviour is deliberately inconsistent program to program and is not to be normalized.

## Notes on period-authentic construction

- `CABRTMNT.pli` and `CABSTREC.pli` both write a `CABSCTL`-layout control record inline (declared
  field by field) rather than through a shared PL/I `%INCLUDE` member, since no PL/I copy library for
  `CABSCTL` exists in this build; the COBOL programs share the copybook, the PL/I programs each
  declare their own copy of the same layout.
- `CABSTREC.pli` builds its two unmatched-item lists (settlement-master-only and netting-file-only) as
  a singly linked list in `BASED` storage reached through a `POINTER`, allocated one node at a time via
  `ALLOCATE`, because the count of unmatched items is not known until both pre-sorted input files have
  been read to end.
- `CABLGCNV.pli` references `%INCLUDE CABCDRLY` for the shared legacy-to-current field crosswalk
  constants (default line number, default tariff code, default LATA, the valid-jurisdiction list). That
  include member is not part of this build — it belongs to the CICS on-line conversion transaction
  layer, which is out of scope for this folder — so `CABLGCNV.pli` is not independently compilable as
  delivered here, consistent with its REFERENCE-ONLY status.
- All four programs write their control record directly from hardcoded job/step identifiers rather
  than through a shared parameter-driven mechanism, since none of them run under the JCL generation
  scheme described in `REXX/CABGENJC.exec`.
