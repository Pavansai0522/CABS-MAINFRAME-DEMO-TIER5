# CONTROL FAMILY — MANIFEST

The reference-layer batch family: six **Enterprise COBOL IMS DL/I** programs and two **Enterprise
COBOL MQI** programs. Two vintages are deliberate — the runnable batch estate (INGEST, RATING,
JURIS, SETTLE, BILLCALC, FORMAT, REPORT) is OS/VS COBOL 1974; this family is Enterprise COBOL, so
scope terminators and `EVALUATE` appear here and nowhere else. See `CONVENTIONS.md`.

Database definitions are in `IMS/` — read `IMS/_README.md` before reading any DL/I program here.

**8 programs, 5,689 lines.** All **REFERENCE-ONLY** — Hercules TK4- / MVS 3.8j has neither IMS
nor MQ.

| Program | Lines | Purpose | PSB / queue | Complexities carried | Runnable? |
|---|---:|---|---|---|---|
| `CABCTL01.cbl` | 687 | Carrier profile extract and listing. Walks `CARRSEG` with `GU`/`GN`, `GNP` down to `CARRBILL`, `CARRFACT` (qualified `>=` on effective date) and `CARRAGMT`; flattens four segments into a 400-byte extract. | `CABCT01P`, `PROCOPT=G` | **7** — only 3 factor rows and 2 agreements fit on the extract; the rest are counted and dropped, and the loss is reported on the listing only. HDAM means the extract is in randomiser order, not key order. | REFERENCE-ONLY |
| `CABCTL02.cbl` | 747 | Quarterly PIU/PLU factor application. Range-edits the quarterly card, `GHU` on `CARRSEG`, `GHNP`/`REPL`/`ISRT` on `CARRFACT`, then `READ`/`REWRITE`/`WRITE` on the VSAM factor KSDS. `CHKP` every N cards. | `CABCT02P`, `PROCOPT=A` | **17 — TWO STORES, ONE UPDATE.** See below. Also: the prior-factor values written to IMS come from the IMS segment and those written to VSAM come from the VSAM record, so once the two drift the restatement uses whichever its own input carries. `P8400-BALANCE` moves zero into `CT-SUMMARISED` so the equation always holds. | REFERENCE-ONLY |
| `CABCTL03.cbl` | 801 | Circuit inventory maintenance. Add / change / disconnect / meet-point / term transactions. `GHU`, `REPL`, `ISRT` with two-level SSA lists, `DLET` on `CIRCMPB`. | `CABCT03P`, `PROCOPT=A` | **3 — HIDDEN ERROR HANDLER.** See below. Since 2000 a disconnect flags the root rather than deleting it, but still deletes the meet-point children. | REFERENCE-ONLY |
| `CABCTL04.cbl` | 720 | Bill history load and invoice register update. `GHU`/`REPL`/`ISRT` on HISAM `BHSTSEG`, `ISRT` on `BHSTDTL` under a two-level SSA list, then `READ`/`REWRITE`/`WRITE` on the VSAM bill header KSDS. | `CABCT04P`, `PROCOPT=AP` | **17 — TWO STORES, ONE UPDATE** and **26 — DEAD CODE**. See both below. Also **19** — the retention date is derived by stepping a two-digit year and wrapping at 99. | REFERENCE-ONLY |
| `CABCTL05.cbl` | 661 | Settlement posting. `GHU`/`REPL`/`ISRT` on `SETLSEG`, `ISRT` on `SETLDTL`, then `READ`/`REWRITE`/`WRITE` on the VSAM settlement master. Populates `SS-OCN-PERIOD`, the source field for the secondary index. | `CABCT05P`, `PROCOPT=A` | **17 — TWO STORES, ONE UPDATE.** See below. The resynchronisation utility `CABSRSYN` referenced in the source and in `P9200-PRINT-TOTALS` **does not exist anywhere in the estate**. | REFERENCE-ONLY |
| `CABCTL06.cbl` | 630 | Settlement dispute enquiry through the **secondary index**. `GU`/`GN` qualified on `SETLXOCN`, `GNP` on `SETLDTL` and on `SETLDISP` qualified by status. Ages each dispute into five bands. | `CABCT06P`, `PROCOPT=G`, **`PROCSEQ=CABSETSX`** | Reading under `PROCSEQ` puts the **index** key in the PCB key feedback area, not the root key — the program takes the root key out of the segment instead. **25** — the ageing arithmetic multiplies whole years by 365, which is close enough for a banded report and wrong across leap years. | REFERENCE-ONLY |
| `CABCTL07.cbl` | 705 | Outbound settlement gateway. Reads `SETLIN`, formats a 300-byte gateway message, `MQPUT` to `CABS.SETTLE.OUT` on queue manager `CSQ1`, `MQCMIT` every 250 messages (SYSIN-driven). Resend mode skips forward to a SYSIN restart key. | `CABS.SETTLE.OUT` | **16** — the record's fate leaves the estate at the `MQPUT`; there is no file, no control record and no return path that records whether the gateway accepted it. `MQMD-REPLYTOQ` is set to `CABS.SETTLE.NACK`, which nothing in the estate ever reads. | REFERENCE-ONLY |
| `CABCTL08.cbl` | 738 | Inbound settlement acknowledgement consumer. `MQGET` from `CABS.SETTLE.ACK` with `MQGMO-WAIT` + `MQGMO-SYNCPOINT`, matches on `MQMD-CORRELID`, posts to the acknowledgement status file, `MQCMIT` every 100, ends on `MQRC-NO-MSG-AVAILABLE`. | `CABS.SETTLE.ACK` | **3 — HIDDEN ERROR HANDLER.** See below. | REFERENCE-ONLY |

---

## Complexity 17 — two stores, one update (three instances)

Three programs write the same business fact into two independent stores with **no coordination
between them**. IMS work is inside the scope of the `CHKP` call; the VSAM write is hardened the
moment it is issued and is not backed out when the checkpoint is abandoned.

| Program | First store (IMS) | Second store (VSAM) | Where the gap is | What is detected |
|---|---|---|---|---|
| `CABCTL02` | `CARRFACT` segment, `REPL` in `P4200` / `ISRT` in `P4400` | `TELCABS.CABS.FACTOR` KSDS, `REWRITE`/`WRITE` in `P5400`/`P5600` | The `CHKP` in `P6000` covers only the IMS side. A failure between `P4000` and `P5000` leaves the quarter loaded in IMS and not in VSAM — the online enquiry shows the new factor and the jurisdictional split still prices at the old one. | Nothing. The two counts are printed side by side in `P9200-PRINT-TOTALS` and no action is taken. |
| `CABCTL04` | `BHSTSEG` root and `BHSTDTL` children, `P3000`/`P4000` | `TELCABS.CABS.BILLHDR` KSDS, `P5000` | A checkpoint abandoned after `P5000` backs out the IMS segments and leaves the VSAM record, so the invoice number is visible to the settlement statement program while the history row is not there. | A printed difference in `P9200-PRINT-TOTALS`. Step still ends RC 0. |
| `CABCTL05` | `SETLSEG` root and `SETLDTL` child, `P3000`/`P4000` | `TELCABS.SETL.MASTER` KSDS, `P5000` | Same shape. Additionally, `P3400-REPLACE-ROOT` deliberately does not delete existing detail segments before a replace, relying on the duplicate-insert rejection. | `P9200-PRINT-TOTALS` computes the divergence and prints `RUN CABSRSYN`. **`CABSRSYN` does not exist in the estate.** |

None of the three sources describes the gap as a defect. Each carries a plausible in-period
justification — a 2007 "two phase commit added" change note in `CABCTL02` that in fact only added
the `CHKP` call, a HISAM reorg note in `CABCTL04`, and a reference to a resynchronisation utility
in `CABCTL05` that was specified and never scheduled.

## Complexity 3 — hidden error handler far from invocation (two instances)

| Program | Handler | Physical position | Reached from | What it does that the callers depend on |
|---|---|---|---|---|
| `CABCTL03` | `P9990-DLI-FAILURE` | **Last paragraph in the program**, after `P9000-EXIT` | `GO TO` from `P3100-ADD-CIRCUIT`, `P3200-POSITION-ROOT`, `P3400-CHANGE-CIRCUIT`, `P3500-DISCONNECT`, `P4000-MEET-POINT`, `P4200-REPL-MPB`, `P4300-ISRT-MPB`, `P4500-TERM-SEGMENT` | Suspends the transaction, cuts the audit row, **forces a `CHKP`** so committed work is hardened, counts the failure, terminates the run once 100 failures accumulate, **reads the next transaction**, then `GO TO P2000-EXIT`. It is part of the read loop, not a subroutine. | Commented only as `* COMMON DATABASE FAILURE HANDLING. SEE CABS-STD-036.` |
| `CABCTL08` | `P9995-MQ-FAILURE` | **Last paragraph in the program**, after `P9000-EXIT` | `GO TO` from `P2100-GET-MESSAGE`, `P4200-MATCH-ACK`, `P6300-UPDATE-STATUS` | Writes a suspense record, `MQCMIT`s the partial unit of work, refreshes the restart key, performs `P8000-CONTROL`, then `GO TO P9000-TERM`. | Commented only as `* COMMON MQ FAILURE HANDLING. SEE CABS-STD-036.` |

In both cases a reader following the mainline `PERFORM` structure will never reach the handler, and
a call-graph tool that models `PERFORM` but not `GO TO` will show it as unreachable. It is neither
unreachable nor optional — it is where the restart position comes from.

## Complexity 26 — dead code (one instance, spanning two languages)

`CABCTL04.cbl` contains `P7700-BRIDGE-OLD-FORMAT` (~30 lines), which converts a 1987-vintage
133-byte bill header into the current layout by calling the PL/I program **`CABLGCNV`**
(`PLI/CABLGCNV.pli`).

`P7700` is performed only from `P2100-READ-HEADER`, guarded by:

```
           IF WS-BRIDGE-REQUIRED
               PERFORM P7700-BRIDGE-OLD-FORMAT THRU P7700-EXIT
           END-IF.
```

`WS-BRIDGE-REQUIRED` is the 88-level on `WS-BRIDGE-SW`, which is declared `VALUE 'N'` and is
**never moved to anywhere in the program**. The parm card carries a `WP-BRIDGE-SW` field that
looks as though it would set it; nothing transfers one to the other.

Consequences worth stating plainly:

- `CABLGCNV.pli` — a genuine 293-line PL/I conversion program — is reachable from exactly one
  place in the estate, and that place never executes.
- A call-graph tool will draw a COBOL→PL/I edge and report the PL/I program as live.
- Neither source says so. `CABCTL04`'s revision history records the bridge being added in 1998
  "for the sites still on the old cut", and `CABLGCNV`'s comment box says it is retained "for the
  remaining sites on the 1987 format". Neither statement is true of any site now.

## Other things a reader should not miss

- **`CABCTL01`** counts factor and agreement rows that did not fit on the extract, computes them
  into `WS-SUMM-CNT`, and then moves **zero** into `CT-SUMMARISED` before balancing. The run
  always balances; the loss is visible only on the printed listing.
- **`CABCTL06`** is documented in its revision history as being callable as a linked subroutine
  from the online enquiry as well as from batch. The CICS program that would link to it
  (`ONLINE/CABONL06.cbl`) is a separate implementation of the same enquiry. The two do not share
  code.
- **`CABCTL07`** and **`CABCTL08`** are a nominal request/response pair, but `CABCTL07` sets
  `MQMD-REPLYTOQ` to `CABS.SETTLE.NACK` while `CABCTL08` reads `CABS.SETTLE.ACK`. Nothing in the
  estate consumes the NACK queue.
- All eight programs write a `CABS-CONTROL-RECORD` to `CTLOUT` from a mandatory `P8000-CONTROL`,
  per `CONVENTIONS.md`. In three of them the balancing equation is satisfied by moving zero into a
  count rather than by the counts actually agreeing.

## Runnable vs reference-only

All eight are **REFERENCE-ONLY**. MVS 3.8j has no IMS DB, no IMS DLI batch region (`DFSRRC00`),
and no MQ. The programs are also Enterprise COBOL, which the TK4- OS/VS COBOL compiler will not
accept. They are authored to production standard for CAST Imaging analysis and are not executed.
