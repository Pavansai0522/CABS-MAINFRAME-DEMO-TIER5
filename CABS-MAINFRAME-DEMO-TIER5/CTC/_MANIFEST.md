# CTC — CARRIER TRAFFIC CONSOLIDATION — MULTI-SITE MANIFEST

Two programs, deployed across twelve regional access billing centres. Each centre promotes its own copy into its own load library (`TELCABS.<centre>.CABS.LOADLIB`) from its own source library. There is no central source of record for either program.

This directory reproduces the single most consequential characteristic found in the client scan: **the same program name appearing at many sites, near-identical, with no mechanism in the estate for deciding which copy is the real one.** In the client scan, 331 distinct basenames appeared across 12 sites and 302 of 305 multi-site programs carried identical finding signatures — statically indistinguishable, operationally not the same thing at all.

Application code `CABS`. OS/VS COBOL 1974 throughout — no `EVALUATE`, no reference modification, no scope terminators. Every copy writes `CABS-CONTROL-RECORD` to DD `CTLOUT` in `P8000-CONTROL` and every copy `COPY CABSWRK`.

**21 COBOL members, 21,107 lines. 3 JCL members, 236 lines.**

| Program | Purpose | Principal datasets |
|---|---|---|
| `CABCTC01.cbl` | Carrier traffic consolidation and usage summary — reads the edited wholesale access usage file in OCN / BAN / rate element sequence, loads the carrier master into core, applies a three-level control break and writes one summary record per rate element carrying the three jurisdiction minute buckets | USGIN + CARRMST -> SUMOUT, SUSOUT, RPTOUT |
| `CABCTC02.cbl` | Carrier rate reconciliation and variance extract — two-sided match-merge of the CABCTC01 summary against the rated summary, rebuilds the expected charge from the flattened rate table and classifies the difference | SUMIN + RATIN + RTBLIN -> RECOUT, SUSOUT, RPTOUT |

---

## 1. Which site holds which program

| Site | Centre | `CABCTC01.cbl` | `CABCTC02.cbl` | Run JCL |
|:--|:--|:--:|:--:|:--:|
| SITE01 | ATL — Atlanta | yes (930 lines) | yes (1,105 lines) | `CABC0100.jcl` |
| SITE02 | CLT — Charlotte | yes (930) | yes (1,105) | — |
| SITE03 | HOU — Houston | yes (930) | yes (1,105) | — |
| SITE04 | STL — St Louis | yes (930) | **absent** | — |
| SITE05 | DAL — Dallas | yes (930) | yes (1,105) | `CABC0100.jcl` |
| SITE06 | NSH — Nashville | yes (930) | yes (1,105) | — |
| SITE07 | PHX — Phoenix | yes (930) | **absent** | — |
| SITE08 | IND — Indianapolis | yes (930) | yes (1,105) | — |
| SITE09 | CHI — Chicago | yes (951) | yes (1,087) | `CABC0100.jcl` |
| SITE10 | CLE — Cleveland | yes (930) | **absent** | — |
| SITE11 | NWK — Newark | yes (914) | yes (1,120) | — |
| SITE12 | SAC — Sacramento | yes (930) | yes (1,105) | — |

`CABCTC01` is at all twelve sites. `CABCTC02` is at nine. Three centres — STL, PHX, CLE — have no reconciliation program at all. Nobody currently in the department can say whether those three centres never took it, took it and backed it out, or run it from another centre's library. The estate does not record the answer.

---

## 2. `CABCTC01` — what is identical and what is not

### Byte-identical group — 7 of 12

`SITE01`, `SITE02`, `SITE04`, `SITE06`, `SITE08`, `SITE10`, `SITE12`

SHA-256 (first 12) `b99e5c41f50b`. 930 lines. `WS-PGM-VERSION` = `V1.09`. These seven are the same file to the byte, including the revision history block.

### Differ in one literal only — 3 of 12

Each of these is byte-identical to the group above except for a **single line**.

| Site | Line | Group value | This site's value |
|:--|:--|:--|:--|
| SITE03 | 160 | `WS-SUMM-STATE-DFLT ... VALUE 'NC'` | `VALUE 'TX'` |
| SITE05 | 162 | `WS-MOU-THRESHOLD ... VALUE 999999.99` | `VALUE 499999.99` |
| SITE07 | 163 | `WS-TARIFF-EFF-YYDDD ... VALUE 96182` | `VALUE 98213` |

All three still report `V1.09`. The version literal was not moved, so no inventory built from `WS-PGM-VERSION` can see these three at all.

Worth noticing what the state literal actually does. `WS-SUMM-STATE-DFLT` is moved into `CS-STATE-CD` on **every** summary record the program writes, including records for traffic that did get a jurisdiction. Houston (SITE03) carries `TX`. Dallas (SITE05) is also in Texas and carries `NC`, because Dallas took the threshold change and not the state change. Sacramento carries `NC`. Whether the `NC` in the other eleven copies is a default that nobody ever revisited or a value that means something to a downstream consumer is not recorded anywhere in the estate.

### Differ in logic — 2 of 12

**SITE09 (CHI) — 951 lines, `V1.11`, 23 lines different from the group.**

- Carries an extra revision entry `V1.11 2017-08-22 M.DELACROIX` describing a trunk group edit added "at this centre only".
- Carries an extra paragraph `P2250-VALIDATE-TRUNK-GROUP` and the `PERFORM` of it at the bottom of `P2200-VALIDATE-USAGE`. No other site has this paragraph. It suspends any voice record with a blank trunk group or a zero CIC. Those records are consolidated normally at the other eleven sites.
- The SITE09 run JCL carries a comment recording that `STEP025`, the factor study extract this edit fed, was pulled in 2019. The edit was left in the program. It still suspends records.

**SITE11 (NWK) — 914 lines, `V1.08`, 26 lines different from the group.**

- The revision history **stops at `V1.08 2011-02-15`**. The `V1.09 2016-05-11` entry that the other eleven copies carry is not present. Newark never received the 2016 change.
- Consequently the paragraph `P2650-EXCLUDE-ISP-MINUTES` and its `PERFORM` inside `P2600-ACCUMULATE` are absent. At the other eleven sites, voice traffic on an ISP-bound trunk group is subtracted from the interstate bucket and moved to the local bucket. At Newark it stays interstate.
- `P3100-APPLY-ROUNDING` uses a **different rounding rule**. Eleven copies read `COMPUTE WS-RW-OUT-MINUTES ROUNDED = WS-RW-RAW-MINUTES`. Newark reads `MOVE WS-RW-RAW-MINUTES TO WS-RW-OUT-MINUTES`, which truncates. The accumulator is at five decimals and genuinely holds fractions, because `P2645-SPLIT-ON-PIU` divides indeterminate traffic between the interstate and intrastate buckets on the carrier percent-interstate-use factor. Every summary record Newark writes is therefore up to half a minute lighter than the same traffic consolidated anywhere else, and `CABCTC02` at Newark reconciles against that lighter figure without noticing.

### Summary of the divergence

| Property | Sites carrying it |
|:--|:--|
| 2016 ISP-minute exclusion (`P2650`) | 11 of 12 — all except SITE11 |
| 2017 trunk group edit (`P2250`) | 1 of 12 — SITE09 only |
| `COMPUTE ... ROUNDED` in `P3100` | 11 of 12 — all except SITE11 |
| `WS-SUMM-STATE-DFLT` = `'NC'` | 11 of 12 — all except SITE03 |
| `WS-PGM-VERSION` = `V1.09` | 10 of 12 — SITE09 says V1.11, SITE11 says V1.08 |

---

## 3. `CABCTC02` — what is identical and what is not

### Byte-identical group — 5 of 9

`SITE01`, `SITE02`, `SITE06`, `SITE08`, `SITE12`

SHA-256 (first 12) `74badc8ad7d7`. 1,105 lines. `WS-PGM-VERSION` = `V2.14`.

### Differ in one literal only — 2 of 9

| Site | Line | Group value | This site's value |
|:--|:--|:--|:--|
| SITE03 | 262 | `WS-VARIANCE-TOLERANCE ... VALUE 0.05` | `VALUE 0.25` |
| SITE05 | 263 | `WS-RETRO-CUTOFF-YYDDD ... VALUE 09001` | `VALUE 12001` |

The tolerance is one-sided by design — a rated amount **below** expected inside the tolerance is passed as agreed, a rated amount above expected is reported at any size. Houston's tolerance is five times the other centres'. Dallas's retro cutoff holds three additional years of rated-only keys out of the reconciliation extract entirely; they are counted into `CT-CARRIED-FWD` and the control record still balances.

### Differ in logic — 2 of 9

**SITE09 (CHI) — 1,087 lines, `V2.11`, 20 lines different.**

- Revision history stops at `V2.11 2008-01-22`. The `V2.14 2018-06-25` entry the other copies carry is not there.
- The paragraph `P3750-COLLAR-EXPECTED` and its `PERFORM` in `P3200-COMPUTE-EXPECTED` are absent. Chicago does not apply the tariff minimum and maximum to the expected amount before comparing it with the rated amount. Every collared rate element at Chicago reconciles as a variance and appears on the exception report. The centre has been working that report by hand since 2018.

**SITE11 (NWK) — 1,120 lines, `V2.15`, 17 lines different.**

- Carries an extra revision entry `V2.15 2019-10-02 M.DELACROIX` and an extra paragraph `P3800-CHECK-CROSS-CENTRE`, performed from `P3500-BUILD-RECON-REC`, which reclassifies any summary record raised by a different centre as one-sided and adds it to `CT-CARRIED-FWD`. No other site has this paragraph.
- Note what this means in combination with the `CABCTC01` divergence at the same site: Newark reconciles truncated minutes against a rated side computed elsewhere from unrounded minutes, and the resulting drift is inside the 0.05 one-sided tolerance for small accounts and outside it for large ones.

---

## 4. Run JCL

Three centres' run JCL is present. It is not the same JCL.

| Member | Centre | Divergence from SITE01 |
|:--|:--|:--|
| `SITE01/CABC0100.jcl` | ATL | Four-library STEPLIB. Both steps run. |
| `SITE05/CABC0100.jcl` | DAL | Three-library STEPLIB — `TELCABS.COMMON.LOADLIB` was never added on `STEP030`. `RATIN` is `DUMMY`, so `CABCTC02` at Dallas reconciles against an empty rated side and reports every key as summary-only. Sort control cards come from a centre library, not the central one. |
| `SITE09/CABC0100.jcl` | CHI | Five-library STEPLIB with a centre `EMERG` library concatenated **first**, so a fix can sit in front of the promoted load module without a promotion. Carries a comment block where `STEP025` used to be. |

The other nine centres' run JCL is not in this estate. It exists on those systems.

---

## 5. Which copy is authoritative?

**The estate does not answer this question. There is no artefact anywhere in it that identifies an authoritative copy of either program, and no rule that can be applied to derive one.**

Everything that looks like it might settle it does not:

- **The version literal does not settle it.** `WS-PGM-VERSION` is a hand-maintained `VALUE` clause. Three of the five divergent `CABCTC01` copies carry `V1.09`, the same value as the byte-identical group, because the person who changed the literal did not change the version. A version-based inventory sees ten identical `V1.09` copies and two outliers, which is wrong in both directions.
- **The revision history does not settle it.** The histories are inconsistent by construction. SITE11's `CABCTC01` history and SITE09's `CABCTC02` history each stop at a release the others moved past. A history that stops is indistinguishable from a history that was never updated.
- **The highest version does not settle it.** SITE09 `CABCTC01` is `V1.11` and SITE11 `CABCTC02` is `V2.15`, both higher than the group, but both are local changes that no other centre took and neither was ever intended to be estate-wide.
- **The most common copy does not settle it.** Seven identical `CABCTC01` copies is a majority, not a decision. The 2016 ISP-minute exclusion that eleven copies carry is a settlement obligation; the one copy that lacks it is arguably the one that is wrong, and majority reasoning would have protected it if the vote had gone the other way.
- **The largest or newest file does not settle it.** File size tracks how many local edits a centre made, not correctness. Modification timestamps in this estate are the timestamps of the last library copy, not of the last change to the logic.
- **`CAST Imaging` will not settle it.** Static analysis will correctly report twelve `CABCTC01` programs with near-identical structure and will correctly identify the paragraphs that differ. It cannot tell you which behaviour the business is supposed to have. Clone detection reports the similarity; it does not report the intent.

**What the difference actually is, is a business decision, and it has to be made by a person.** Specifically, someone with authority over wholesale settlement has to answer four questions that no tool can answer:

1. Should ISP-bound minutes sit in the local bucket or the interstate bucket, and from what date? Eleven centres say local since 2016. Newark says interstate. One of those positions is the settlement obligation and the other is exposure.
2. Should the summary minute total round or truncate? The two rules produce different invoices. Both have been in production for a decade.
3. Should the trunk group edit at Chicago apply everywhere, apply nowhere, or has it been suspending valid records since the study it fed was retired in 2019?
4. Should the reconciliation tolerance be 0.05 or 0.25, one-sided or two-sided, and should the three centres with no reconciliation program at all have one?

Until those four are answered, any migration of this family is a migration of an undecided question. Converting twelve copies faithfully preserves twelve behaviours. Converting one copy and pointing all twelve centres at it silently changes the invoice at up to eleven of them. **Both of those are decisions. Neither of them is a technical decision.**

The realistic sequence is: reconcile the twelve copies into a single specification with the differences explicit (mechanical, and tooling helps a lot); take the four business decisions above (human, and no tooling helps at all); then build one program. The middle step is the one that determines the timeline, and it is the one that is invisible in a static analysis report.

---

## 6. Cross-references

- Summary layout `CABS-CTCS-RECORD` is defined inline in both programs and is **not** in `COPYBOOKS/`. Twelve copies of a record layout that no copybook governs.
- `CABCTC02` re-declares `CABS-RTBL-RECORD` and its two `REDEFINES` from `BATCH/RATING/CABRAT01.cbl`. A change to the flattened extract has to be made in `CABRAT01` and in nine copies of `CABCTC02`.
- `CABCTC01` reads `TELCABS.CABS.CARRIER` (`CABSCARR`), owned by the ingest application.
- `CABCTC02` reads `TELCABS.CABS.RATSUMM` and `TELCABS.CABS.RATETBL`, both produced by the rating suite.
