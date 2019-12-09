       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COBDOA01.
       DATE-WRITTEN.   12/4/2019.
       AUTHOR.         DAVID AMOS.
       DATE-COMPILED.
      *----------------------------------------------------------------
      *|    THIS PROGRAM READS A FILE AND CREATES A |
      *|    STUDENT ROSTER REPORT.                  |
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAINT-MASTER
               ASSIGN TO "C:\IHCC\COBOL\PAINTEST.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRTOUT
               ASSIGN TO "C:\IHCC\COBOL\PJOBEST.RPT"
               ORGANIZATION IS RECORD SEQUENTIAL.
       

       DATA DIVISION.
       FILE SECTION.

       FD  PAINT-MASTER
           LABEL RECORD IS STANDARD
           DATA RECORD IS PAINT-REC
           RECORD CONTAINS 23 CHARACTERS.

       01  PAINT-REC.
           05  PAINT-EST-NO            PIC X(4).
           05  PAINT-DATE.
               10  PAINT-YY            PIC 9(4).
               10  PAINT-MM            PIC 99.
               10  PAINT-DD            PIC 99.
           05  PAINT-WALL-SQ-FT        PIC 9(4).
           05  PAINT-DOOR-SQ-FT        PIC 999.
           05  PAINT-PRICE-GAL         PIC 99V99.

       FD  PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.

       01  PRTLINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WORK-AREA.
           05  C-TOTAL-SQ-FT           PIC 9(4).
           05  C-GAL                   PIC 99V99.
           05  C-PAINT-EST             PIC 99999V99.
           05  C-LABOR-EST             PIC 99999V99.
           05  C-TOTAL-EST             PIC 999999V99.
           05  C-PCTR                  PIC 99          VALUE 0.
           05  C-GT-CTR                PIC 999         VALUE 0.
           05  C-GT-GAL                PIC 99999V99    VALUE 0.
           05  C-GT-PAINT-EST          PIC 9999999V99  VALUE 0.
           05  C-GT-LABOR-EST          PIC 9999999V99  VALUE 0.
           05  C-GT-TOTAL-EST          PIC 99999999V99 VALUE 0.
           05  MORE-RECS               PIC XXX         VALUE 'YES'.

       01  CURRENT-DATE-AND-TIME.
           05  THIS-DATE.
               10  I-YY                PIC 9(4).
               10  I-MM                PIC 99.
               10  I-DD                PIC 99.
           05  I-TIME                  PIC X(11).

       01  COMPANY-TITLE.
           05  FILLER                  PIC X(6)        VALUE "DATE: ".
           05  O-MM                    PIC 99.
           05  FILLER                  PIC X           VALUE "/".
           05  O-DD                    PIC 99.
           05  FILLER                  PIC X           VALUE '/'.
           05  O-YY                    PIC 9(4).
           05  FILLER                  PIC X(38)       VALUE SPACES.
           05  FILLER                  PIC X(22)       
                                       VALUE "AMOS'S PAINT ESTIMATOR".
           05  FILLER                  PIC X(48)       VALUE SPACES.
           05  FILLER                  PIC X(6)        VALUE "PAGE: ".
           05  O-PCTR                  PIC Z9.

       01  COLLUMN-HEADINGS1.
           05  FILLER                  PIC X(8)        VALUE "ESTIMATE".
           05  FILLER                  PIC X(23)       VALUE SPACES.
           05  FILLER                  PIC X(4)        VALUE "WALL".
           05  FILLER                  PIC X(7)        VALUE SPACES.
           05  FILLER                  PIC X(4)        VALUE "DOOR".
           05  FILLER                  PIC X(6)        VALUE SPACES.
           05  FILLER                  PIC X(5)        VALUE "TOTAL".
           05  FILLER                  PIC X(6)        VALUE SPACES.
           05  FILLER                  PIC X(7)        VALUE "GALLONS".
           05  FILLER                  PIC X(6)        VALUE SPACES.
           05  FILLER                  PIC X(6)        VALUE "PRICE/".
           05  FILLER                  PIC X(11)       VALUE SPACES.
           05  FILLER                  PIC X(5)        VALUE "PAINT".
           05  FILLER                  PIC X(12)       VALUE SPACES.
           05  FILLER                  PIC X(5)        VALUE "LABOR".
           05  FILLER                  PIC X(12)       VALUE SPACES.
           05  FILLER                  PIC X(5)        VALUE "TOTAL".

       01  COLLUMN-HEADINGS2.
           05  FILLER                  PIC X           VALUE SPACE.
           05  FILLER                  PIC X(11)       VALUE "NUMBER".
           05  FILLER                  PIC X(18)
                                       VALUE 'ESTIMATE DATE'.
           05  FILLER                  PIC X(11)       VALUE "SQ/FT".
           05  FILLER                  PIC X(11)       VALUE "SQ/FT".
           05  FILLER                  PIC X(12)       VALUE "SQ/FT".
           05  FILLER                  PIC X(12)       VALUE "NEEDED".
           05  FILLER                  PIC X(14)       VALUE "GALLON".
           05  FILLER                  PIC X(17)       VALUE "ESTIMATE".
           05  FILLER                  PIC X(17)       VALUE "ESTIMATE".
           05  FILLER                  PIC X(8)        VALUE "ESTIMATE".

       01  DETAIL-LINE.
           05  FILLER                  PIC XX          VALUE SPACES.
           05  O-PAINT-EST-NO          PIC X(4).
           05  FILLER                  PIC X(7)        VALUE SPACES.
           05  O-PAINT-DATE.
               10  O-PAINT-MM          PIC 99.
               10  FILLER              PIC X           VALUE '/'.
               10  O-PAINT-DD          PIC 99.
               10  FILLER              PIC X           VALUE '/'.
               10  O-PAINT-YY          PIC 9(4).
           05  FILLER                  PIC X(7)        VALUE SPACES.
           05  O-WALL-SQ-FT            PIC Z,ZZ9.
           05  FILLER                  PIC X(7)        VALUE SPACES.
           05  O-DOOR-SQ-FT            PIC ZZ9.
           05  FILLER                  PIC X(7)        VALUE SPACES.
           05  O-TOTAL-SQ-FT           PIC Z,ZZ9.
           05  FILLER                  PIC X(7)        VALUE SPACES.
           05  O-GAL                   PIC ZZZ.99.
           05  FILLER                  PIC X(7)        VALUE SPACES.
           05  O-PRICE-GAL             PIC ZZ.99       VALUE SPACES.
           05  FILLER                  PIC X(6)        VALUE SPACES.
           05  O-PAINT-EST             PIC $ZZ,ZZZ.99.
           05  FILLER                  PIC X(7)        VALUE SPACES.
           05  O-LABOR-EST             PIC $ZZ,ZZZ.99.
           05  FILLER                  PIC X(6)        VALUE SPACES.
           05  O-TOTAL-EST             PIC $ZZZ,ZZZ.99.

       01  GT-LINE.
           05  FILLER                  PIC X(34)
                                       VALUE "GRAND TOTALS:".
           05  FILLER                  PIC X(17)
                                       VALUE "TOTAL ESTIMATES:".
           05  O-GT-CTR                PIC ZZ9.
           05  FILLER                  PIC X(7)        VALUE SPACES.
           05  O-GT-GAL                PIC ZZ,ZZZ.99.
           05  FILLER                  PIC X(15)       VALUE SPACES.
           05  O-GT-PAINT-EST          PIC $$,$$$,$$$.99.
           05  FILLER                  PIC X(4)        VALUE SPACES.
           05  O-GT-LABOR-EST          PIC $$,$$$,$$$.99.
           05  FILLER                  PIC X(3)        VALUE SPACES.
           05  O-GT-TOTAL-EST          PIC $$$,$$$,$$$.99.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS = 'NO'.
           PERFORM 3000-CLOSING.
           STOP RUN.

       1000-INIT.
           OPEN INPUT PAINT-MASTER.
           OPEN OUTPUT PRTOUT.

           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE I-YY TO O-YY.
           MOVE I-DD TO O-DD.
           MOVE I-MM TO O-MM.

           PERFORM 9000-READ.
           PERFORM 9100-HDGS.

       2000-MAINLINE.
           PERFORM 2100-CALCS.
           PERFORM 2200-OUTPUT.
           PERFORM 9000-READ.

       2100-CALCS.
           SUBTRACT PAINT-DOOR-SQ-FT FROM PAINT-WALL-SQ-FT  GIVING 
           C-TOTAL-SQ-FT.
           DIVIDE C-TOTAL-SQ-FT BY 115 GIVING C-GAL ROUNDED.
           MULTIPLY C-GAL BY PAINT-PRICE-GAL GIVING C-PAINT-EST.
           COMPUTE C-LABOR-EST = C-GAL * 3 * 23.55.
           ADD C-PAINT-EST TO C-LABOR-EST GIVING C-TOTAL-EST.

           ADD 1 TO C-GT-CTR.
           ADD C-GAL TO C-GT-GAL.
           ADD C-PAINT-EST TO C-GT-PAINT-EST.
           ADD C-LABOR-EST TO C-GT-LABOR-EST.
           ADD C-TOTAL-EST TO C-GT-TOTAL-EST.

       2200-OUTPUT.
           MOVE PAINT-EST-NO TO O-PAINT-EST-NO.
           MOVE PAINT-YY TO O-PAINT-YY.
           MOVE PAINT-MM TO O-PAINT-MM.
           MOVE PAINT-DD TO O-PAINT-DD.
           MOVE PAINT-WALL-SQ-FT TO O-WALL-SQ-FT.
           MOVE PAINT-DOOR-SQ-FT TO O-DOOR-SQ-FT.
           MOVE C-TOTAL-SQ-FT TO O-TOTAL-SQ-FT.
           MOVE C-GAL TO O-GAL.
           MOVE PAINT-PRICE-GAL TO O-PRICE-GAL.
           MOVE C-PAINT-EST TO O-PAINT-EST.
           MOVE C-LABOR-EST TO O-LABOR-EST.
           MOVE C-TOTAL-EST TO O-TOTAL-EST.

           WRITE PRTLINE FROM DETAIL-LINE
               AFTER ADVANCING 1 LINE
                   AT EOP
                       PERFORM 9100-HDGS.

       3000-CLOSING.
           MOVE C-GT-CTR TO O-GT-CTR.
           MOVE C-GT-GAL TO O-GT-GAL.
           MOVE C-GT-PAINT-EST TO O-GT-PAINT-EST.
           MOVE C-GT-LABOR-EST TO O-GT-LABOR-EST.
           MOVE C-GT-TOTAL-EST TO O-GT-TOTAL-EST.

           WRITE PRTLINE FROM GT-LINE
               AFTER ADVANCING 3 LINES.
           CLOSE PAINT-MASTER.
           CLOSE PRTOUT.

       9000-READ.
           READ PAINT-MASTER
               AT END
                   MOVE 'NO' TO MORE-RECS.

       9100-HDGS.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.
           WRITE PRTLINE FROM COMPANY-TITLE
               AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM COLLUMN-HEADINGS1
               AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM COLLUMN-HEADINGS2
               AFTER ADVANCING 1 LINE.







           
       END PROGRAM COBDOA01.