      *****************************************************************
      * CABSRT03 - BANDED RATE WORK AREA.  FLATTENED BAND POOL - ALL  *
      * BANDS FOR ALL ELEMENTS LIVE IN ONE POOL AND EACH RATE TABLE   *
      * ENTRY CARRIES AN OFFSET INTO IT (R2-EN-BAND-OFFSET).          *
      * THIS SAVED 180K OF REGION IN 1993 AND HAS NEVER BEEN UNDONE.  *
      * NESTED VIA CABSRT02.  NESTS CABSRT04.                         *
      *****************************************************************
       01  R3-BAND-POOL.
           05  R3-POOL-CNT                 PIC 9(04) VALUE 0.
           05  R3-POOL-ENTRY OCCURS 1 TO 2400 TIMES
                    DEPENDING ON R3-POOL-CNT
                    INDEXED BY R3-PX.
               10  R3-PL-FROM              PIC S9(11) COMP-3.
               10  R3-PL-THRU              PIC S9(11) COMP-3.
               10  R3-PL-RATE              PIC S9(05)V9(05) COMP-3.
               10  R3-PL-PCT               PIC S9(03)V9(05) COMP-3.
       01  R3-BAND-WORK.
           05  R3-BW-QTY                   PIC S9(13)V9(02) COMP-3
                                                            VALUE 0.
           05  R3-BW-SEL-SUB               PIC S9(04) COMP-3 VALUE 0.
           05  R3-BW-SEL-RATE              PIC S9(05)V9(05) COMP-3
                                                            VALUE 0.
           05  R3-BW-RESIDUAL              PIC S9(13)V9(02) COMP-3
                                                            VALUE 0.
           05  R3-BW-FOUND-SW              PIC X(01) VALUE 'N'.
               88  R3-BW-FOUND             VALUE 'Y'.
               88  R3-BW-NOT-FOUND         VALUE 'N'.
           05  R3-BW-MODE                  PIC X(01) VALUE 'S'.
               88  R3-BW-STEPPED           VALUE 'S' 'G'.
               88  R3-BW-GRADUATED         VALUE 'G' 'T'.
               88  R3-BW-FLAT              VALUE 'F'.
       COPY CABSRT04.
