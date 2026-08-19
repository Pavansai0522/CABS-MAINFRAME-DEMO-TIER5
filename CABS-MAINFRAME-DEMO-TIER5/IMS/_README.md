# IMS reference layer — what is here and what is deliberately missing

This folder holds the IMS DB definitions for the CABS reference layer: four databases, two index
databases, six program specification blocks, and the gen job that builds them.

The six Enterprise COBOL DL/I batch programs that use them live in `BATCH/CONTROL/` — see
`BATCH/CONTROL/_MANIFEST.md`.

## What is here

| Database | DBD member | Organisation | Root | Dependents |
|---|---|---|---|---|
| CARRIER | `CABCARDB.dbd` | **HDAM**, OSAM, `RMNAME=(DFSHDC40,10,500,824)` | `CARRSEG` (`CROCN`) | `CARRBILL`, `CARRFACT`, `CARRAGMT` |
| CIRCUIT | `CABCIRDB.dbd` + `CABCIRIX.dbd` | **HIDAM**, VSAM, with its primary index database | `CIRCSEG` (`CICKTID`) | `CIRCMPB`, `CIRCTERM` |
| SETTLEMENT | `CABSETDB.dbd` + `CABSETSX.dbd` | **HDAM**, OSAM, with a **secondary index** on `STOCNPD` (OCN + settle period) via `XDFLD SETLXOCN` | `SETLSEG` (`STKEY`) | `SETLDTL`, `SETLDISP` |
| BILLHIST | `CABBHSDB.dbd` | **HISAM**, VSAM, prime + overflow | `BHSTSEG` (`BHKEY` = BAN + bill period) | `BHSTDTL` |

| PSB | Program | PROCOPT | Notes |
|---|---|---|---|
| `CABCT01P.psb` | `CABCTL01` | `G` | All four CARRIER segments |
| `CABCT02P.psb` | `CABCTL02` | `A` | `CARRSEG`, `CARRFACT` |
| `CABCT03P.psb` | `CABCTL03` | `A` | All three CIRCUIT segments |
| `CABCT04P.psb` | `CABCTL04` | `AP` | BILLHIST, insert-heavy |
| `CABCT05P.psb` | `CABCTL05` | `A` | `SETLSEG`, `SETLDTL` |
| `CABCT06P.psb` | `CABCTL06` | `G` | **`PROCSEQ=CABSETSX`** — the database is presented in secondary-index sequence |

`CABIMSGN.jcl` runs DBDGEN, PSBGEN and ACBGEN in that order.

---

## What is NOT here — and why that is realistic

**Two databases that the programs reference have no DBD or PSB in this repository.**

| Referenced as | Referenced by | Status |
|---|---|---|
| `CABRATDB` — rate/tariff database | The `CALL 'CBLTDLI'` interface used by the online rating enquiry and by the settlement rate resolution path | **No DBD, no PSB, no ACB source anywhere in the estate** |
| `CABBANDB` — account/BAN database | The account-level validation used by the billing and settlement families | **No DBD, no PSB, no ACB source anywhere in the estate** |

This is deliberate, and it mirrors what real scans find. In the reference engagement behind this
asset, **69 programs were found issuing IMS DL/I calls against databases whose DBD and PSB source
was not in any library handed over.** The usual explanations, all of which apply here:

1. **The DBDs live in a different library that was never in scope.** IMS source is conventionally
   kept in `IMS.DBDSRC` / `IMS.PSBSRC` under the systems-programming group, not in the
   application's own PDS. Application-team handovers routinely omit it.
2. **The gen source was lost and only the ACBs survive.** The database still runs, because ACBGEN
   output is what the control region loads. Nobody has needed the source since the last DBDGEN.
3. **The database belongs to another application.** `CABRATDB` was owned by the tariff group;
   CABS was given PSB sensitivity to it and never held the definition.
4. **It was migrated and the stub was left behind.** The calls still compile because `CBLTDLI` is
   resolved at link-edit from the IMS interface module, not from the DBD.

### Why this matters for analysis

A static analyser will see the `CALL 'CBLTDLI'` and the SSA literals, and can recover the segment
and field names from the SSA structures in working storage. It **cannot** recover:

- the database organisation (HDAM vs HIDAM vs HISAM changes access cost and rebuild strategy
  completely);
- the hierarchy — which segment is a child of which, and therefore what a `GNP` will actually
  return;
- the key structure and lengths, so it cannot tell a qualified SSA from a malformed one;
- logical relationships and secondary indexes, which are the only way to know that a `GU` on one
  database can position in another;
- `PROCOPT`, so it cannot tell a read-only program from an updating one.

**The correct response is not to guess.** It is to record the gap explicitly: list every program
issuing DL/I calls, list every DBD name and segment name recovered from the SSAs, and flag the
databases for which no definition exists as a named dependency to be retrieved from the systems
group before any migration estimate is committed. An estimate produced without them will be wrong
about data volume, about access paths, and about which programs are updaters.

## The other thing to watch for in `BATCH/CONTROL/`

Three of the six DL/I programs update an IMS segment **and** a VSAM file with no coordination
between the two. IMS work is inside the scope of the `CHKP` call; the VSAM write is hardened
immediately and is not backed out when the checkpoint is abandoned. See
`BATCH/CONTROL/_MANIFEST.md` for which three and where.
