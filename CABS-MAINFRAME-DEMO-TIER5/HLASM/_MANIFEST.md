# HLASM — MANIFEST

Assembler service routines called from the COBOL estate. OS/VS Assembler / HLASM source, standard
OS linkage (`SAVE (14,12)` style register save, register 1 addressing a parameter list of
fullword addresses, reply in the parameter area rather than in register 15 except where noted).

**5 source members, 1,749 lines** (4 distinct modules; `CABDATCV` exists twice — see below).

| File | Lines | Purpose | Load library | Complexities carried | Runnable? |
|---|---:|---|---|---|---|
| `CABPKDEC.asm` | 366 | Packed-decimal arithmetic service. `ZAP`/`AP`/`SP`/`MP`/`DP` in a 16-byte work area; functions `ADD` `SUB` `MUL` `DIV` `RND` `ZAP` `CMP`. Reentrant, GETMAINs its own work area. | `TELCABS.COMMON.LOADLIB` | **2** — called **dynamically** from RATING and SETTLE (name built in working storage, `CALL` by identifier), so the binder never resolves it and it appears in no static call graph. **Rounding divergence** — see below. | REFERENCE-ONLY |
| `CABBITST.asm` | 261 | Usage status flag bit service. `TM` / `OI` / `NI` / `XI` / `EX`-driven mask operations on the one-byte edit-status field; functions `TEST` `SETB` `CLRB` `FLIP` `CNTB` `DUMP`. | `TELCABS.COMMON.LOADLIB` | Statically called from `CABING01`, `CABING05`, `CABING09`, `CABING11`. The field it manipulates is a `PIC X(01)` in `CABSCDR` whose 88-levels treat it as a display digit — a byte with more than one bit on satisfies a different 88 from the one the intake set. | REFERENCE-ONLY |
| `CABDATCV.asm` | 463 | Date conversion and day arithmetic. YYDDD ↔ CCYYMMDD ↔ absolute day number, add days with rollover, difference in days, leap-year test. Alias entry point `CABDTCNV`, which is the name the COBOL side calls. | `TELCABS.COMMON.LOADLIB` | **12 — library search order.** See below. | REFERENCE-ONLY |
| `EMERG/CABDATCV.asm` | 330 | **The same module name**, at the 1998 vintage. Assembled under problem record PR-9964 on 1998-12-08 and left in the emergency library. | `TELCABS.SETL.LOADLIB.EMERG` | **12 — library search order.** See below. | REFERENCE-ONLY |
| `CABABEND.asm` | 329 | Controlled abend and dump formatter. Three-line `WTO` to the console and job log, side-by-side hex/character formatting of a caller-supplied diagnostic area to `SYSPRINT`, `SNAP` of the caller's save-area chain to `SNAPDD`, then `ABEND (n),DUMP` — or a return, if the caller passes action code `R`. | `TELCABS.COMMON.LOADLIB` | Statically called from every batch module's `P9500-ABEND` / `P9900-FATAL-EXIT`. Listed in `CONTRACTS/complexity_placement.json` under `external_subprograms_static_call`. | REFERENCE-ONLY |

---

## Complexity 12 — library search order (`CABDATCV` in two libraries)

Two different load modules with the same name and the same entry points exist in the estate:

| | `HLASM/CABDATCV.asm` | `HLASM/EMERG/CABDATCV.asm` |
|---|---|---|
| Version | V2.03 (2018) | V1.08E (1998) |
| Load library | `TELCABS.COMMON.LOADLIB` | `TELCABS.SETL.LOADLIB.EMERG` |
| **Century pivot** | **70** — matches `DW-PIVOT-YY` in `COPYBOOKS/CABSDATE.cpy` and the seven inline literal 70s across the COBOL estate | **68** — set to match the industry exchange specification |
| Leap-year rule | Divisible by 4, **not by 100 unless by 400** (century rule added 1998-12-08 in the common version) | Divisible by 4 only |
| `ADDD` implementation | Converts to an absolute day number, adds, converts back (rewritten 2003) | Steps the day-of-year and rolls the year one at a time (the 1998 emergency fix) |
| Functions supported | `JTOG` `GTOJ` `JTOA` `ATOJ` `ADDD` `DIFF` `LEAP` | `JTOG` `GTOJ` `ADDD` `LEAP` only |

**Which one a program gets depends entirely on STEPLIB concatenation order**, and the estate is
not consistent:

| Job | STEPLIB order | Binds |
|---|---|---|
| `JCL/CABS2900.jcl` (dispute handler) | `SETL.LOADLIB.EMERG`, `SETL.LOADLIB`, `COMMON.LOADLIB` | **the 1998 version** — pivot 68 |
| `JCL/CABS3200.jcl` (settlement posting) | `SETL.LOADLIB.EMERG`, `SETL.LOADLIB`, `CABS.LOADLIB`, `COMMON.LOADLIB` | **the 1998 version** — pivot 68 |
| `JCL/CABS2450.jcl` (wireless settlement) | `SETL.LOADLIB`, `SETL.LOADLIB.EMERG`, `COMMON.LOADLIB` | the 1998 version, if `SETL.LOADLIB` does not hold a copy |
| `JCL/CABRAT01.jcl`, `CABJ1800.jcl`, `CABJ2000.jcl`, all RATING reruns | `CABS.LOADLIB` / `SETL.LOADLIB`, `COMMON.LOADLIB` | **the 2018 version** — pivot 70 |

So the same source-level call — `CALL 'CABDTCNV'` — resolves a **two-digit year of 68 or 69 to
1968/1969 in the settlement jobs and to 2068/2069 in the rating jobs**, and applies a different
leap-year rule for century years. Nothing in the COBOL, and nothing in the JCL comments, says so.
The only place the difference is visible is by comparing the two assembler sources, which are in
different source libraries.

Neither source admits the divergence. Both read as correct in isolation, which is the point.

## Rounding divergence in `CABPKDEC` (not flagged in source)

`CABPKDEC`'s `MUL`, `DIV` and `RND` functions round **half away from zero at the fifth decimal
place**: the routine takes the magnitude, adds a half-unit of the next place down from `ADJTAB`,
divides by the corresponding power of ten from `DIVTAB`, and restores the sign. A negative value
therefore rounds away from zero exactly as a positive one does.

Its COBOL callers in RATING and SETTLE use `COMPUTE ... ROUNDED`, which under OS/VS COBOL is
**half-up at the receiving field's scale — normally two decimal places**, and which for a negative
intermediate rounds toward zero in some compiler releases.

The two conventions therefore disagree on:
- the **place** at which rounding happens (5dp in the assembler, 2dp in the COBOL receiving field);
- the **direction** for negative amounts (away from zero vs. toward zero).

Amounts that pass through `CABPKDEC` and are then moved to a `PIC S9(nn)V9(02)` field are rounded
twice. The comment block in `CABPKDEC` presents its convention as the house standard
(`CABS-STD-019`) and does not mention the callers. This is deliberate and must not be "corrected"
in the source.

## Runnable vs reference-only

All five are **REFERENCE-ONLY**:
- They assemble with HLASM/OS-VS Assembler macros (`WTO MF=(E,...)`, `SNAP`, `ABEND`, `GETMAIN R`,
  `DCB`, `OPEN`/`CLOSE`/`PUT`) that MVS 3.8j supports, but the modules are written against a
  later macro library and use `ENTRY`/alias conventions and `EX`-driven code that would need
  re-verification under the TK4- assembler.
- `CABPKDEC` is reentrant and issues `GETMAIN R` / `FREEMAIN R` per entry, which is correct but
  costly at the call rates the rating estate would drive under TK4-.
- None of them are referenced by an `EXEC PGM=` — they are subroutines, and the runnable COBOL
  layer that would call them has not been link-edited against them.

## What a reverse-engineering exercise must not miss

1. `CABPKDEC` is **dynamically** called. It will not appear in a binder-derived call graph.
2. `CABDATCV` resolves differently per job. A single-node "CABDATCV" in any dependency graph is
   wrong; there are two implementations and the edge is decided at load time by STEPLIB order.
3. `CABBITST` is the only code in the estate that understands the edit-status byte as a bit map.
   The COBOL 88-levels over the same byte encode a different, incompatible interpretation.
4. `CABABEND` is on every failure path in the estate and is the only place where a controlled
   failure becomes an operator-visible event.
