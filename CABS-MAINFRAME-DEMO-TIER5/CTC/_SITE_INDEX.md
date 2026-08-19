# REGIONAL ACCESS BILLING CENTRE INDEX

    TELCABS WHOLESALE ACCESS BILLING
    CARRIER TRAFFIC CONSOLIDATION - SITE REGISTER
    MAINTAINED BY : APPLICATIONS SUPPORT - CARRIER TRAFFIC
    LAST FULL REVIEW : 2014-11-03
    LAST AMENDMENT   : 2017-02-19  (SITE09 CONTACT TEAM)
    DISTRIBUTION     : OPERATIONS, APPLICATIONS SUPPORT, CAPACITY

This is the operations-era register the night shift used to work out who to
ring when a consolidation step failed. It was kept in a shared folder and
printed for the operations bridge. It has not been reviewed since 2014 and
the only amendment since then was a contact team change.

| Site | Centre | Region | LPAR | SYSPLEX | Sched grp | Contact team | Pager |
|:--|:--|:--|:--|:--|:--|:--|:--|
| SITE01 | ATL — Atlanta, GA | Southeast | PRDA | PLEXA | CABSSE1 | Access Billing Support — Atlanta | 4471 |
| SITE02 | CLT — Charlotte, NC | Southeast | PRDA | PLEXA | CABSSE1 | Access Billing Support — Atlanta | 4471 |
| SITE03 | HOU — Houston, TX | Southwest | PRDB | PLEXB | CABSSW2 | Access Billing Support — Dallas | 4488 |
| SITE04 | STL — St Louis, MO | Central | PRDB | PLEXB | CABSCE1 | Regional Billing Ops — St Louis | 4512 |
| SITE05 | DAL — Dallas, TX | Southwest | PRDB | PLEXB | CABSSW2 | Access Billing Support — Dallas | 4488 |
| SITE06 | NSH — Nashville, TN | Southeast | PRDA | PLEXA | CABSSE1 | Access Billing Support — Atlanta | 4471 |
| SITE07 | PHX — Phoenix, AZ | Mountain | PRDC | PLEXC | CABSMT1 | Regional Billing Ops — Phoenix | 4530 |
| SITE08 | IND — Indianapolis, IN | Central | PRDD | PLEXD | CABSCE2 | Regional Billing Ops — Chicago | 4544 |
| SITE09 | CHI — Chicago, IL | Central | PRDD | PLEXD | CABSCE2 | Carrier Traffic Engineering — Chicago | 4547 |
| SITE10 | CLE — Cleveland, OH | Northeast | PRDE | PLEXE | CABSNE1 | Regional Billing Ops — Newark | 4561 |
| SITE11 | NWK — Newark, NJ | Northeast | PRDE | PLEXE | CABSNE1 | Regional Billing Ops — Newark | 4561 |
| SITE12 | SAC — Sacramento, CA | Pacific | PRDC | PLEXC | CABSPC1 | Regional Billing Ops — Phoenix | 4530 |
| SITE13 | KCY — Kansas City, MO | Central | PRDB | PLEXB | CABSCE1 | Regional Billing Ops — St Louis | 4512 |

## NOTES CARRIED FORWARD FROM THE 2014 REVIEW

1.  SITE13 (KCY) TRAFFIC WAS FOLDED INTO SITE04 (STL) OVER THE 2014
    LABOR DAY WEEKEND. THE KCY LOAD LIBRARIES AND SOURCE LIBRARIES WERE
    LEFT IN PLACE PENDING THE ANNUAL AUDIT AND ARE TO BE DELETED ONCE
    THE AUDIT HAS SIGNED OFF THE FINAL KCY BILL RUN. RAISED AS
    CHG-2014-0918. STILL OPEN AT THE TIME OF THIS REVIEW.

2.  SITE04 AND SITE13 SHARE SCHEDULING GROUP CABSCE1. AFTER THE FOLD
    THE KCY JOBS WERE LEFT DEFINED AND FLAGGED HELD SO THAT THE
    SCHEDULE COULD BE PUT BACK QUICKLY IF THE FOLD HAD TO BE REVERSED.
    THEY HAVE NOT BEEN RELEASED SINCE 2014-09-01.

3.  SITE07 (PHX) AND SITE12 (SAC) SHARE LPAR PRDC AND THE SAME CONTACT
    TEAM BUT SIT IN DIFFERENT REGIONS FOR REPORTING. OPERATIONS SHOULD
    RING THE PHOENIX NUMBER FOR BOTH.

4.  SITE09 (CHI) IS THE ONLY CENTRE WHOSE FIRST-LINE CONTACT IS AN
    ENGINEERING TEAM RATHER THAN A BILLING OPERATIONS TEAM. THIS DATES
    FROM THE FACTOR STUDY WORK AND WAS AMENDED IN 2017 AT THE REQUEST
    OF THAT TEAM.

5.  RECONCILIATION (CABCTC02) DOES NOT RUN AT EVERY CENTRE. AS AT THE
    2014 REVIEW IT WAS NOT SCHEDULED AT STL, PHX OR CLE. THE REASON WAS
    NOT RECORDED AT THE TIME AND COULD NOT BE ESTABLISHED DURING THE
    REVIEW.

6.  PAGER NUMBERS ARE THE 4-DIGIT INTERNAL NUMBERS. THE EXTERNAL
    NUMBERS ARE HELD BY THE OPERATIONS BRIDGE.

## KNOWN TO BE OUT OF DATE

This register lists **thirteen** centres. There are **twelve**. SITE13 (KCY)
was folded into SITE04 (STL) in September 2014 and has not consolidated
traffic since. It is still listed here because CHG-2014-0918 was never
closed, and it is still listed in the scheduler for the same reason.

Anyone building an inventory from this file will count thirteen sites and
will look for a `SITE13` directory that does not exist. Anyone building an
inventory from the directory tree will count twelve and will not know that
a thirteenth ever existed, or that St Louis has been carrying Kansas City's
traffic under its own site code since 2014 — which is why `CS-SITE-CD` on a
St Louis summary record is not a reliable indicator of where the traffic
originated.

The contact teams, the pager numbers and the sysplex names have not been
verified since 2014 and at least two of the named teams have been
reorganised since. Treat this file as evidence of what the estate looked
like, not as a description of what it is.
