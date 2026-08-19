# ONLINE/BMS — Mapset and Assembly Manifest

All members are **REFERENCE-ONLY**. MVS 3.8j / TK4- has no CICS region and
no BMS macro library (`DFHMAC`/`SDFHMAC`) available to it — none of this
layer has ever been assembled or link-edited. The two-pass assembly job
(`CABMAPS.jcl`) has never been submitted. This layer exists so that the
symbolic map field names referenced by `ONLINE/CABONL01.cbl` through
`CABONL06.cbl` resolve to a genuine physical screen layout, the way a
production estate's BMS source and its CICS COBOL are meant to agree.

| File | Lines | Purpose | Complexities carried | RUNNABLE on TK4- / REFERENCE-ONLY |
|---|---|---|---|---|
| `CABM0100.bms` | 91 | Mapset for CAB0 (`CABONL01`) — main menu, 5-option select field, message line, PF legend. Original 1991 layout, no color/hilight. | Field inventory notes a withdrawn 1998-2003 option 6 (REPORTS) that occupied row 14 with no corresponding DFHMDF ever added and no mapset revision logged for it | REFERENCE-ONLY |
| `CABM0200.bms` | 109 | Mapset for CAB1 (`CABONL02`) — carrier (OCN) inquiry, one record per screen, PF7/PF8 browse legend. 1991 layout, ISP cap/recip fields added 1997. | — | REFERENCE-ONLY |
| `CABM0300.bms` | 92 | Mapset for CAB2 (`CABONL03`) — invoice inquiry, bill header plus a 5-line scrolling detail window. 1992 layout, jurisdiction split added 1998. | — | REFERENCE-ONLY |
| `CABM0400.bms` | 94 | Mapset for CAB3 (`CABONL04`) — factor (PIU/PLU) maintenance. One physical map serves both the edit screen and the confirm-before-update screen; `CONF` starts protected and is unprotected dynamically by `CABONL04` moving `DFHBMFSE` to `CONFA`. 1993 layout, PSU field added 1996. | — | REFERENCE-ONLY |
| `CABM0500.bms` | 98 | Mapset for CAB4 (`CABONL05`) — dispute entry. First mapset built under the 2004 color-terminal standard (`COLOR=`/`HILIGHT=` used throughout). `CONF` is unprotected from the outset here, unlike `CABM0400`. | — | REFERENCE-ONLY |
| `CABM0600.bms` | 108 | Mapset for CAB5 (`CABONL06`) — settlement inquiry. 2004 vintage, color/hilight; `DSW` (dispute switch) is reverse-video. Never sent when `CABONL06` is running linked as a subroutine. | — | REFERENCE-ONLY |
| `CABMAPS.jcl` | 62 | Two-pass BMS assembly/link job — `TYPE=MAP` for the physical map (into the CICS load library) and `TYPE=DSECT` for the symbolic map (into the COBOL copybook library, members `CABM0100`-`CABM0600`, `COPY`'d by the matching `CABONLnn.cbl`). Steps for mapsets 2-6 are noted as mechanical repeats of the pattern shown for `CABM0100` rather than spelled out six times. | — | REFERENCE-ONLY |

Total: 592 lines of BMS source across six mapsets, plus 62 lines of
assembly JCL.

## Vintage split

`CABM0100.bms` through `CABM0400.bms` (CAB0-CAB3) are the original
1991-1993 layouts: `EXTATT=MAPONLY`, no `COLOR=`/`HILIGHT=`, basic
attribute byte (`baseA`) only. `CABM0500.bms` and `CABM0600.bms` (CAB4,
CAB5) are 2004-vintage, built under the color-terminal standard adopted
with that year's dispute-tracking project: `EXTATT=YES`, `COLOR=` and
`HILIGHT=` on every field. Both eras still generate the basic `baseA`
attribute byte used for dynamic protect/unprotect toggling in the COBOL —
that mechanism did not change between vintages, only the cosmetics did.

## Symbolic map naming

Every field with a COBOL-visible symbolic name is given as the label of
its `DFHMDF` macro (column 1-8), 4-6 characters, so the generated
symbolic map entries (`baseL`, `baseA`/`baseF`, `baseI`, `baseO`) stay
within the 8-character COBOL name limit. Unnamed `DFHMDF` fields (titles,
menu text, PF-key legends, border lines) carry no symbolic entry and
cannot be altered by the transaction at run time — see the field
inventory block at the bottom of each `.bms` member for the full list of
named fields, kept by hand per `CABS-STD-021` section 3 rather than
regenerated from the macros.
