# REXX/ — Manifest

All members in this folder are **REFERENCE-ONLY**. TK4- ships MVS 3.8j, which predates TSO/E REXX at
the level used here (`ISFEXEC`/SDSF panel variables, `LISTCAT ... GDG ALL` output parsing, `OUTTRAP`
against `SUBMIT`). None of these execs will run against the Hercules image. `CABCMPDS` and `CABCTLXT`
are both plain sequential/EXECIO REXX with no SDSF dependency and would run unmodified under a modern
z/OS TSO/E; the other four depend on facilities (SDSF, GDG base LISTCAT, the internal reader SUBMIT
behaviour used for self-resubmission) that are realistic for a 1990s-2010s production shop but outside
this estate's runnable scope.

| File | Lines | Purpose | Complexities carried | RUNNABLE / REFERENCE-ONLY |
|---|---|---|---|---|
| `CABGENJC.exec` | 164 | Reads a control table and generates + submits JCL with run-specific symbolics | **Complexity 9** (see below) | REFERENCE-ONLY |
| `CABMONJB.exec` | 130 | SDSF-based job output queue monitor; flags RC > 4 and abends, notifies the operator | none | REFERENCE-ONLY |
| `CABCMPDS.exec` | 150 | Positional or keyed sequential dataset comparison utility | none | REFERENCE-ONLY (would run on a modern z/OS TSO/E) |
| `CABCTLXT.exec` | 136 | Unpacks a CABSCTL control record's packed counts and checks the balancing equation | none | REFERENCE-ONLY (would run on a modern z/OS TSO/E) |
| `CABGDGCL.exec` | 125 | GDG generation cleanup with a hardcoded permanent-retention exclusion list | none | REFERENCE-ONLY |
| `CABRSTRT.exec` | 167 | Restart helper - reads the last control record, builds a RESTART= parameter, resubmits | none | REFERENCE-ONLY |

## Complexity 9 on CABGENJC — read this before assuming the JCL library tells you what a job runs with

`CABGENJC.exec` is the one member in this folder carrying a placed complexity from
`CONTRACTS/complexity_placement.json`. It reads `TELCABS.CABS.CONTROL.PARMTBL`, an 80-byte fixed-card
dataset holding one row per job/symbolic combination: job name, proc name, symbolic name, symbolic
value, an effective-cycle date window (`EFF-FROM`/`EFF-THRU`), and an active switch. For the job name
passed as an argument, the exec selects every row whose active switch is `Y` and whose effective
window covers today's date, builds a `// SET` statement for each selected symbolic plus a single
`// EXEC proc,SYMBOL=value,...` statement, writes the result to a throwaway temporary JCL member, and
submits it. The temporary member is deleted immediately after submission.

The practical effect: the JCL library member for a job like `CABS3100` never contains the cycle date,
the bill period, the DB2 subsystem name, or any of the other symbolics that job runs with each cycle —
those values live in `TELCABS.CABS.CONTROL.PARMTBL` and are assembled fresh, at submit time, by this
exec. **A static scan of the JCL library — CAST Imaging or otherwise — will show `// EXEC CABPDB2P`
with symbolic references like `&&CYCLDT` and `&&BILLPR` and nothing that resolves them.** The actual
values a given run used are recoverable only by finding the `PARMTBL` row that was active for that
run's date, or by reading the generated-and-discarded temporary JCL member out of the JES spool before
it ages off — the temp member itself is never kept on disk. This is the same effect achieved
deliberately elsewhere in this estate through dynamic COBOL `CALL` targets and sort-card-only business
rules: the logic that determines runtime behaviour lives outside the artifact a static scanner reads.

The house comment style for this exec calls the behaviour a convenience ("SAVES OPERATIONS RETYPING
THE CYCLE DATE IN NINETEEN MEMBERS") rather than describing it as an analysis blind spot — that is the
in-universe framing an operations team would actually have used to justify building it in 1994.

## Notes

- `CABCTLXT.exec` hardcodes the `CABSCTL` byte offsets (see `COPYBOOKS/CABSCTL.cpy`) because there is
  no COBOL copybook parser available in this REXX environment; the offsets are computed once from the
  copybook's `PIC` clauses and then maintained by hand, with the same drift risk that implies if the
  copybook ever changes without a corresponding update here.
- `CABGDGCL.exec`'s exclusion list (`TELCABS.SETL.AUDIT.TRAIL`, `TELCABS.CABS.ACCTG.EXCEPTION`,
  `TELCABS.SETL.RESTATE.ARCHIVE`) is maintained by hand in the source, not read from a control dataset.
- `CABRSTRT.exec`'s stepname-to-sequence table is likewise maintained by hand and must be kept in step
  with the JCL library independently — a new step added to any listed job's JCL member requires a
  matching manual addition here.
