# ONLINE — CICS Suite Manifest

All six programs are **REFERENCE-ONLY**. MVS 3.8j / TK4- carries no CICS
region, no VSAM RLS, and no BMS support — none of this layer has ever been
compiled, link-edited or executed. It exists so a CAST Imaging (or
equivalent) scan of the estate has a genuine, internally-consistent CICS
online suite to resolve XCTL/LINK targets against, in the same way the
DB2, IMS and batch reference layers exist without a runnable counterpart
on Hercules. Compiler target is Enterprise COBOL per `CONVENTIONS.md` —
`EVALUATE` and scope terminators are used freely here.

## Programs

| File | Lines | Purpose | Complexities carried | RUNNABLE on TK4- / REFERENCE-ONLY |
|---|---|---|---|---|
| `CABONL01.cbl` | 387 | CAB0 menu/dispatcher. SEND/RECEIVE MAP, validates the option, XCTLs to CAB1-CAB5 with the shared COMMAREA, maintains the 4-deep nav stack (push on dispatch, pop on PF12 cancel). Old-style `HANDLE CONDITION`. | COMMAREA state carried across screens (nav stack); leftover `WS-RPT-MENU-SW` field from the withdrawn 1998-2003 option 6 (REPORTS) | REFERENCE-ONLY |
| `CABONL02.cbl` | 363 | CAB1 carrier (OCN) inquiry. Direct `READ` on `CARRMST`, `STARTBR`/`READNEXT`/`READPREV`/`ENDBR` for forward/backward paging. Fully `RESP()`-style — no `HANDLE CONDITION` at all (2016 rewrite). | COMMAREA browse-state dependency; `CM-SAVED-KEY-1` reuse (OCN) | REFERENCE-ONLY |
| `CABONL03.cbl` | 464 | CAB2 invoice inquiry. `READ` on `BILLHDR`, five-line-per-screen browse of `BILLDTL` keyed by BAN+period, paged with PF7/PF8. Old-style `HANDLE CONDITION`, including a `DUPKEY` handler for a condition the current key structure can no longer raise. | COMMAREA browse-state dependency (explicit, see below); `CM-SAVED-KEY-2` reuse (BAN); decayed `DUPKEY` handler | REFERENCE-ONLY |
| `CABONL04.cbl` | 377 | CAB3 factor (PIU/PLU) maintenance. `READ UPDATE`/`REWRITE` against `RATEFCTR`, `WRITE` for a new quarter (PF5), range edits, confirm-before-update second screen reusing the same physical map. Fully `RESP()`-style (2015 rewrite). | Range edits (0-1 fraction); confirm/pending COMMAREA state; sets the undocumented maintenance flag byte (see below) | REFERENCE-ONLY |
| `CABONL05.cbl` | 526 | CAB4 dispute entry. `LINK PROGRAM('CABONL06')` for the current settlement position, `WRITE` to the dispute file, `EXEC CICS SYNCPOINT`. Old-style `HANDLE CONDITION` including a live `DUPREC` trap. Contains the dormant delegated-authority auto-credit path. | **Complexity 27 — dormant feature** (`P6000-AUTO-CREDIT`, see below); tests the undocumented maintenance flag byte; behaviour keyed on `CM-RETURN-TO` | REFERENCE-ONLY |
| `CABONL06.cbl` | 508 | CAB5 settlement inquiry. `STARTBR`/`READNEXT`/`READPREV`/`ENDBR` on `SETLMST`. Dual-mode: runs as the CAB5 transaction (browses interactively) or as a LINKed subroutine of CABONL05 (`CM-FUNCTION-CD = 'SP'`, no map I/O). Old-style `HANDLE CONDITION`. | Dual entry-mode dispatch on `EIBCALEN`/`CM-FUNCTION-CD`; COMMAREA browse-state dependency; `CM-SAVED-KEY-3` reuse (OCN+period) | REFERENCE-ONLY |

Total online COBOL: 2,625 lines across 6 programs.

## Supporting copybook

| File | Lines | Purpose | RUNNABLE / REFERENCE-ONLY |
|---|---|---|---|
| `COPYBOOKS/CABSCOMM.cpy` | 99 | Shared CICS COMMAREA layout for CAB0-CAB5 — nav stack, browse state, three general-purpose saved-key slots, edit/confirm switches, the 8-byte positional flag area, session identification. New copybook, not frozen. | REFERENCE-ONLY (CICS-only artifact) |

## CICS content coverage (mandatory list from the build instructions)

`SEND MAP`/`RECEIVE MAP` (ERASE, DATAONLY, ALARM, CURSOR) — all six programs.
`HANDLE CONDITION` (old-style) — CABONL01, CABONL03, CABONL05, CABONL06.
`RESP(WS-RESP)` style — CABONL02, CABONL04 (the two 2015/2016 rewrites; the
suite is deliberately inconsistent between vintages).
`READ`/`READ UPDATE`/`REWRITE`/`WRITE`/`STARTBR`/`READNEXT`/`READPREV`/
`ENDBR` — CABONL02 (CARRMST), CABONL03 (BILLHDR/BILLDTL), CABONL04
(RATEFCTR), CABONL05 (DISPUTE, CREDMEMO), CABONL06 (SETLMST).
`RETURN TRANSID(...) COMMAREA(...) LENGTH(...)` — all six.
`XCTL` — CABONL01 (to CAB1-CAB5), CABONL02/03/04/05/06 (back to CAB0),
CABONL06 (to CAB4, PF4).
`LINK PROGRAM(...) COMMAREA(...)` — CABONL05 to CABONL06.
`ASKTIME`/`FORMATTIME` — all six.
`ABEND ABCODE(...)` — all six, generic error handler.
`SYNCPOINT` — CABONL05, after the dispute `WRITE`.
`COPY DFHAID.` / `COPY DFHBMSCA.` — all six.
Pseudo-conversational structure (`EIBCALEN = ZERO` test, `EIBAID` against
`DFHPFn`) — all six.

## COMMAREA state rules that exist nowhere in writing

These are genuine cross-program dependencies carried through
`CABSCOMM.cpy`. None of them is documented in a comment beyond the bland,
generic notes already in the copybook and the source below (per house
style, no comment names the rule or its consequence) — they exist only as
behaviour, the way this kind of coupling actually survives in a legacy
estate. Listed here for the analyst; nowhere in the source itself.

| Field | Set by | Tested by | Effect |
|---|---|---|---|
| `CM-FLAGS` byte 5 (`CM-FLAG-5`) | `CABONL04`, `P5000-PROCESS-CONFIRM`, on a successful `REWRITE`/`WRITE` to `RATEFCTR` | `CABONL05`, `P3000-RECEIVE-AND-VALIDATE` | A factor update this cycle silently blocks a dispute against the same OCN/period until the flag is reset. Nothing resets it — it survives only for the life of the COMMAREA chain. |
| `CM-BR-LAST-KEY` / `CM-BR-PAGE-NBR` / `CM-BR-EOF-SW` / `CM-BR-BOF-SW` | Whichever of `CABONL02`, `CABONL03`, `CABONL06` most recently did a direct lookup or a page | The same program, on the next PF7/PF8 in the same conversation | PF7/PF8 silently do nothing useful (a "look up a key first" message) unless a lookup already ran in this session — the browse position is never independently re-derivable from the screen. |
| `CM-SAVED-KEY-1` | `CABONL02` (carrier OCN) | Nothing currently reads it back — reserved slot, established by convention only | Slot 1 is understood estate-wide to mean "the OCN currently in view," but that convention is not written anywhere. |
| `CM-SAVED-KEY-2` | `CABONL03` (BAN) | Nothing currently reads it back | Same pattern as slot 1, BAN instead of OCN. |
| `CM-SAVED-KEY-3` | `CABONL05` (`P4000`, before the LINK) and `CABONL06` (`P2200`/`P3500`/`P3600`/`P8600`) | `CABONL05` `P1800-SEND-ENTRY-SCREEN` and `P4000`; `CABONL06` `P6000-SUBROUTINE-LOOKUP` | The same 20-byte slot is packed two different ways depending which program last touched it — bytes 1-4 are always an OCN, bytes 5-10 are a settlement period, and nothing enforces that the two programs agree on that packing beyond both having been written by the same 2004 project. |
| `CM-RETURN-TO` | `CABONL01` (always `'CAB0'` before every XCTL); `CABONL06` `P8600-DISPUTE-THIS-SETTLEMENT` (`'CAB5'`) | `CABONL05` `P1800`, `P3000`, `P5600` | Entering CABONL05 from CABONL06 instead of the menu changes field requirements (BAN not required) and prefills OCN/period from `CM-SAVED-KEY-3` — a second, undocumented entry contract for the same transaction. |
| `CM-FUNCTION-CD = 'SP'` | `CABONL05` `P4000`, immediately before `LINK`, cleared immediately after | `CABONL06` `P0000-MAINLINE` | The only signal distinguishing "attached as the CAB5 transaction" from "linked as a subroutine." Any other program linking to CABONL06 without setting this correctly would get the transaction's full map-sending behaviour instead of a silent lookup. |
| `CM-FILLER` bytes 1-9 (redefined locally in both programs as `LK-SETL-NET-DUE`/`LK-SETL-DIRECTION`) | `CABONL06` `P6000-SUBROUTINE-LOOKUP` | `CABONL05` `P4000` and `P5600-WRITE-DISPUTE` | The copybook calls this field general-purpose growth room. In practice nine of its twenty bytes are a private, offset-dependent contract between exactly these two programs. Widening any field earlier in `CABSCOMM.cpy` without updating the hardcoded `PIC X(372)` filler in both local `REDEFINES` would silently corrupt this handoff. |

## Complexity 27 — dormant feature

`CABONL05.cbl`, paragraph `P6000-AUTO-CREDIT` (with `P6100-COMPUTE-CREDIT`,
`P6200-FORMAT-CREDIT-MEMO`, `P6300-WRITE-CREDIT-MEMO` — 90 lines total,
header box included) is a complete, intact delegated-authority credit
facility: it looks up a delegated credit percentage by dispute reason code
(25%-75% of the disputed amount), caps it at the operator's authority
limit, formats a credit memo record, writes it to `CREDMEMO`, and marks
the dispute record credited.
It is gated on `CM-AUTO-CREDIT-SW = 'Y'` — a COMMAREA field that is
declared in `CABSCOMM.cpy` and read here, but that no program in the
suite ever moves `'Y'` into. The revision history at the top of
`CABONL05.cbl` records it as added under `CR-2291` in November 2004 and
suspended the following month "pending tariff review." The suspension was
never lifted in source; the code was never removed.
