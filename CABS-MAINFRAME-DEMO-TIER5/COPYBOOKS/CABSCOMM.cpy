      *****************************************************************
      * CABSCOMM - CICS COMMAREA PASSED BETWEEN ALL CAB0-CAB5         *
      * TRANSACTIONS.  ONE LAYOUT SHARED BY THE FULL ONLINE SUITE SO   *
      * THAT XCTL AND LINK CAN HAND CONTEXT FORWARD WITHOUT A REREAD   *
      * OF THE TERMINAL'S PRIOR SCREEN.  SEE CABS-STD-014, ONLINE      *
      * COMMAREA STANDARDS.  DO NOT REORDER FIELDS - CM-FILLER AT THE  *
      * BOTTOM IS THE ONLY SAFE PLACE TO GROW THIS LAYOUT.             *
      *****************************************************************
       01  CABS-COMM-AREA.

      * WHAT THE RECEIVING PROGRAM IS BEING ASKED TO DO, AND WHERE
      * CONTROL SHOULD GO WHEN IT IS DONE.  CM-RETURN-TO CARRIES THE
      * TRANID OF THE SCREEN THAT ISSUED THE XCTL OR LINK SO A COMMON
      * SUB-FUNCTION CAN BEHAVE DIFFERENTLY DEPENDING ON WHO CALLED IT.
           05  CM-FUNCTION-CD              PIC X(02).
           05  CM-RETURN-TO                PIC X(04).
           05  CM-PREV-MAP                 PIC X(08).
           05  CM-NEXT-MAP                 PIC X(08).
           05  CM-CALLER-TRANID            PIC X(04).

      * FOUR DEEP PUSHDOWN OF WHERE THE OPERATOR HAS BEEN, MAINTAINED
      * BY HAND AT EACH XCTL.  CM-NAV-DEPTH IS THE NUMBER OF ENTRIES
      * CURRENTLY STACKED.  A FIFTH XCTL WITHOUT AN INTERVENING POP
      * OVERLAYS ENTRY 4 - THE ESTATE HAS NEVER GONE FIVE SCREENS DEEP
      * SO THIS HAS NOT BEEN OBSERVED IN PRODUCTION.
           05  CM-NAV-STACK OCCURS 4 TIMES INDEXED BY CM-NX.
               10  CM-NS-PGM               PIC X(08).
               10  CM-NS-MAP               PIC X(08).
               10  CM-NS-KEY               PIC X(20).
           05  CM-NAV-DEPTH                PIC 9(01).

      * BROWSE POSITIONING.  A PAGING TRANSACTION THAT XCTLS AWAY AND
      * IS XCTLD BACK TO (MENU-AND-RETURN) PICKS THE BROWSE UP WHERE
      * IT LEFT OFF RATHER THAN REPOSITIONING TO THE FRONT OF THE FILE.
           05  CM-BROWSE-STATE.
               10  CM-BR-LAST-KEY           PIC X(25).
               10  CM-BR-DIRECTION          PIC X(01).
                   88  CM-BR-FORWARD            VALUE 'F'.
                   88  CM-BR-BACKWARD           VALUE 'B'.
               10  CM-BR-PAGE-NBR           PIC 9(04).
               10  CM-BR-EOF-SW             PIC X(01).
                   88  CM-BR-AT-END             VALUE 'Y'.
               10  CM-BR-BOF-SW             PIC X(01).
                   88  CM-BR-AT-START           VALUE 'Y'.

      * GENERAL PURPOSE KEY SAVE AREA.  CONTENT IS WHATEVER KEY THE
      * CURRENT CHAIN OF SCREENS IS WORKING AGAINST - AN OCN, A BAN,
      * A CIRCUIT ID - AND IS ESTABLISHED BY WHICHEVER TRANSACTION
      * POPULATED IT MOST RECENTLY.  CABS-STD-014 RESERVES THREE
      * SLOTS SO A CHAIN OF UP TO THREE RELATED LOOKUPS CAN BE
      * CARRIED WITHOUT ADDING FIELDS EVERY TIME A NEW SCREEN JOINS
      * THE SUITE.
           05  CM-SAVED-KEY-1               PIC X(20).
           05  CM-SAVED-KEY-2               PIC X(20).
           05  CM-SAVED-KEY-3               PIC X(20).

      * MAINTENANCE / CONFIRMATION STATE FOR THE TWO-SCREEN UPDATE
      * PATTERN (EDIT SCREEN, THEN A SEPARATE CONFIRM SCREEN BEFORE
      * THE REWRITE OR WRITE IS ISSUED).
           05  CM-EDIT-STATE.
               10  CM-CONFIRM-SW            PIC X(01).
                   88  CM-CONFIRM-PENDING       VALUE 'Y'.
               10  CM-UPDATE-PENDING-SW     PIC X(01).
                   88  CM-UPDATE-PENDING        VALUE 'Y'.
               10  CM-EDIT-SEQ              PIC 9(02).
               10  CM-AUTO-CREDIT-SW        PIC X(01).

      * POSITIONAL FLAG AREA.  RESERVED - SET BY THE MAINTENANCE
      * TRANSACTIONS.  SEE CABS-STD-014 SECTION 4 FOR THE CURRENT
      * BYTE ASSIGNMENTS BEFORE ADDING A NEW ONE.
           05  CM-FLAGS                     PIC X(08).
           05  CM-FLAGS-R REDEFINES CM-FLAGS.
               10  CM-FLAG-1                PIC X(01).
               10  CM-FLAG-2                PIC X(01).
               10  CM-FLAG-3                PIC X(01).
               10  CM-FLAG-4                PIC X(01).
               10  CM-FLAG-5                PIC X(01).
               10  CM-FLAG-6                PIC X(01).
               10  CM-FLAG-7                PIC X(01).
               10  CM-FLAG-8                PIC X(01).

      * SESSION IDENTIFICATION AND THE LAST CICS RESPONSE SEEN BY
      * ANY PROGRAM IN THE CHAIN - USEFUL WHEN A DOWNSTREAM SCREEN
      * NEEDS TO KNOW WHETHER THE HANDOFF ARRIVED CLEAN.
           05  CM-USERID                    PIC X(08).
           05  CM-TERMID                    PIC X(04).
           05  CM-LAST-RESP                 PIC S9(08) COMP.
           05  CM-LAST-RESP2                PIC S9(08) COMP.

      * WORK DATE/TIME, REFRESHED BY ASKTIME/FORMATTIME ON FIRST
      * ENTRY SO A CHAIN OF LINKED PROGRAMS SHARES ONE TIMESTAMP.
           05  CM-WORK-DATE                 PIC X(08).
           05  CM-WORK-TIME                 PIC X(08).

      * ONE-LINE MESSAGE CARRIED FORWARD ACROSS AN XCTL SO THE NEXT
      * SCREEN CAN DISPLAY WHY CONTROL ARRIVED THE WAY IT DID.
           05  CM-MSG-TEXT                  PIC X(60).

           05  CM-FILLER                    PIC X(20).
