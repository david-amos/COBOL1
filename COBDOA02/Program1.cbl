       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COBDOA02.
       DATE-WRITTEN.   12/12/2019.
       AUTHOR.         DAVID AMOS.
       DATE-COMPILED.
      *-----------------------------------------------------
      *    THIS PROGRAM READS A FILE AND MAKES A       |
      *    REPORT ABOUT BOAT SALES                     |
      *    IT WILL BREAK ON THE BOAT TYPE.             |
      *-----------------------------------------------------

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BOAT-CONTROL
               ASSIGN TO "C:\IHCC\COBOL\CBLBOAT1.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRTOUT
               ASSIGN TO "C:\IHCC\COBOL\BOATRPT1.PRT"
               ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  BOAT-CONTROL
           LABEL RECORD IS STANDARD
           DATA RECORD IS BOAT-REC
           RECORD CONTAINS 42 CHARACTERS.
       01  BOAT-REC.
           05 I-LAST-NAME              PIC X(15).
           05 I-STATE                  PIC XX.
           05 I-BOAT-COST              PIC 9(6)V99.
           05 I-PURCHASE-DATE.
               10 I-P-YY               PIC X(4).
               10 I-P-MM               PIC XX.
               10 I-P-DD               PIC XX.
           05 I-BOAT-TYPE              PIC X.
           05 I-ACCESSORY-PACKAGE      PIC 9.
           05 I-PREP-COST              PIC 9(5)V99.

       FD  PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 50 WITH FOOTING AT 44.

       01 PRTLINE                      PIC X(132).

       WORKING-STORAGE SECTION.
       01  WORK-AREA.
           05 C-TOTAL-COST         PIC 9(7)V99.
           05 C-BOAT-CTR           PIC 9(4)        VALUE 0.
           05 C-MJ-TOTAL-COST      PIC 9(9)V99     VALUE 0.
           05 C-GT-BOAT-CTR        PIC 9(5)        VALUE 0.
           05 C-GT-TOTAL-COST      PIC 9(12)V99    VALUE 0.
           05 C-PCTR               PIC 99          VALUE 0.
           05 MORE-RECS            PIC XXX         VALUE 'YES'.
           05 H-BOAT-TYPE          PIC X.
           05 C-BOAT-TYPE          PIC X(13).

       01  CURRENT-DATE-AND-TIME.
           05  THIS-DATE.
               10 I-YY             PIC 9(4).
               10 I-MM             PIC 99.
               10 I-DD             PIC 99.
           05 I-TIME               PIC X(11).

       01  COMPANY-TITLE.
           05  FILLER                  PIC X(6)        VALUE "DATE: ".
           05  O-MM                  PIC 99.
           05  FILLER                  PIC X           VALUE "/".
           05  O-DD                  PIC 99.
           05  FILLER                  PIC X           VALUE '/'.
           05  O-YY                  PIC 9(4).
           05  FILLER                  PIC X(41)       VALUE SPACES.
           05  FILLER                  PIC X(17)
                                       VALUE "AMOS'S BOATS INC.".
           05 FILLER                   PIC X(50)       VALUE SPACES.
           05 FILLER                   PIC X(6)        VALUE "PAGE:".
           05 O-PCTR                   PIC XX.

       01 COLLUMN-HEADINGS1.
           05 FILLER                   PIC X(43)  VALUE "CUSTOMER".
           05 FILLER                   PIC X(13)  VALUE "BOAT".
           05 FILLER                   PIC X(19)  VALUE "PURCHASE".
           05 FILLER                   PIC X(30)  VALUE 'ACCESSORY'.
           05 FILLER                   PIC X(22)  VALUE 'PREP'.
           05 FILLER                   PIC X(5)   VALUE 'TOTAL'.

       01 COLLUMN-HEADINGS2.
           05 FILLER                   PIC X(23)   VALUE "LAST NAME".
           05 FILLER                   PIC X(20)   VALUE "STATE".
           05 FILLER                   PIC X(13)   VALUE "COST".
           05 FILLER                   PIC X(19)   VALUE "DATE".
           05 FILLER                   PIC X(30)   VALUE "PACKAGE".
           05 FILLER                   PIC X(23)   VALUE "COST".
           05 FILLER                   PIC X(4)    VALUE "COST".

       01 BOAT-TYPE-LINE.
           05 FILLER                   PIC X(11)   VALUE "BOAT TYPE:".
           05 O-BOAT-TYPE              PIC X(13).
           05 FILLER                   PIC X(108)  VALUE SPACES.

       01 DETAIL-LINE.
           05 O-LAST-NAME              PIC X(15).
           05 FILLER                   PIC X(8)    VALUE SPACES.
           05 O-STATE                  PIC XX.
           05 FILLER                   PIC X(12)   VALUE SPACES.
           05 O-BOAT-COST              PIC ZZZ,ZZZ.99.
           05 FILLER                   PIC X(9)    VALUE SPACES.
           05 O-PURCHASE-DATE.
               10 O-P-MM               PIC 99.
               10 FILLER               PIC X       VALUE '/'.
               10 O-P-DD               PIC 99.
               10 FILLER               PIC X       VALUE '/'.
               10 O-P-YY               PIC 99.
           05 FILLER                   PIC X(11)   VALUE SPACES.
           05 O-ACCESSORY-PACKAGE      PIC X(15).
           05 FILLER                   PIC X(9)    VALUE SPACES.
           05 O-PREP-COST              PIC ZZZ,ZZZ.99.
           05 FILLER                   PIC X(11)   VALUE SPACES.
           05 O-TOTAL-COST             PIC Z,ZZZ,ZZZ.99.

       01 MAJOR-BREAK-LINE.
           05 FILLER                   PIC X(23)   VALUE SPACES.
           05 FILLER                   PIC X(14) VALUE "SUBTOTALS FOR".
           05 O-MJ-BOAT-TYPE              PIC X(13).
           05 FILLER                   PIC X(11)   VALUE SPACES.
           05 FILLER                   PIC X(14)   VALUE "NUMBER SOLD:".
           05 O-BOAT-CTR               PIC Z,ZZ9.
           05 FILLER                   PIC X(37)   VALUE SPACES.
           05 O-MJ-TOTAL-PRICE         PIC $$$$,$$$,$$$.99.

       01 GRAND-TOTAL-LINE.
           05 FILLER                   PIC X(23)   VALUE SPACES.
           05 FILLER                   PIC X(38)   VALUE "GRAND TOTALS".
           05 FILLER                   PIC X(13)   VALUE "NUMBER SOLD:".
           05 O-GT-BOAT-CTR            PIC ZZ,ZZ9.
           05 FILLER                   PIC X(34)   VALUE SPACES.
           05 O-GT-TOTAL-COST          PIC $$$,$$$,$$$,$$$.99.

       01 BLANK-LINE.
           05 FILLER                   PIC X(132)  VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS = 'NO'.
           PERFORM 3000-CLOSING.
           STOP RUN.

       1000-INIT.
           OPEN INPUT BOAT-CONTROL.
           OPEN OUTPUT PRTOUT.

           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE I-YY TO O-YY.
           MOVE I-DD TO O-DD.
           MOVE I-MM TO O-MM.

           PERFORM 9100-READ.
           MOVE I-BOAT-TYPE TO H-BOAT-TYPE.
           PERFORM 9200-HEADINGS.

       2000-MAINLINE.
           IF H-BOAT-TYPE NOT = I-BOAT-TYPE
               PERFORM 9000-MAJOR-BREAK.
               
           PERFORM 2100-CALCS.
           PERFORM 2200-OUTPUT.
           PERFORM 9100-READ.

       2100-CALCS.
           ADD I-BOAT-COST TO I-PREP-COST GIVING C-TOTAL-COST.
           ADD C-TOTAL-COST TO C-MJ-TOTAL-COST.
           ADD 1 TO C-BOAT-CTR.

       2200-OUTPUT.
           MOVE I-LAST-NAME TO O-LAST-NAME.
           MOVE I-STATE TO O-STATE.
           MOVE I-BOAT-COST TO O-BOAT-COST.
           MOVE I-P-YY TO O-P-YY.
           MOVE I-P-DD TO O-P-DD.
           MOVE I-P-MM TO O-P-MM.
           MOVE I-PREP-COST TO O-PREP-COST.
           MOVE C-TOTAL-COST TO O-TOTAL-COST.

           EVALUATE I-ACCESSORY-PACKAGE
               WHEN 1
                   MOVE 'ELECTRONICS' TO O-ACCESSORY-PACKAGE
               WHEN 2
                   MOVE 'SKI PACKAGE' TO O-ACCESSORY-PACKAGE
               WHEN 3
                   MOVE 'FISHING PACKAGE' TO O-ACCESSORY-PACKAGE.

           WRITE PRTLINE FROM DETAIL-LINE
               AFTER ADVANCING 1 LINE
                   AT EOP
                       PERFORM 9200-HEADINGS.

       3000-CLOSING.
           PERFORM 9000-MAJOR-BREAK.
           PERFORM 3100-GRANDTOTALS.
           CLOSE BOAT-CONTROL.
           CLOSE PRTOUT.

       3100-GRANDTOTALS.
           MOVE C-GT-BOAT-CTR TO O-GT-BOAT-CTR.
           MOVE C-GT-TOTAL-COST TO O-GT-TOTAL-COST.
           WRITE PRTLINE FROM GRAND-TOTAL-LINE
               AFTER ADVANCING 2 LINES.

       9000-MAJOR-BREAK.
           MOVE C-BOAT-CTR TO O-BOAT-CTR.
           MOVE C-MJ-TOTAL-COST TO O-MJ-TOTAL-PRICE.
           WRITE PRTLINE FROM MAJOR-BREAK-LINE
               AFTER ADVANCING 2 LINES
                   AT EOP
                       PERFORM 9200-HEADINGS.
           
           EVALUATE I-BOAT-TYPE
               WHEN 'B'
                   MOVE 'BASS BOAT' TO C-BOAT-TYPE
               WHEN 'P'
                   MOVE 'PONTOON' TO C-BOAT-TYPE
               WHEN 'S'
                   MOVE 'SKI BOAT' TO C-BOAT-TYPE
               WHEN 'J'
                   MOVE 'JOHN BOAT' TO C-BOAT-TYPE
               WHEN 'C'
                   MOVE 'CANOE' TO C-BOAT-TYPE
               WHEN 'R'
                   MOVE 'CABIN CRUISER' TO C-BOAT-TYPE.
               MOVE C-BOAT-TYPE TO O-MJ-BOAT-TYPE.
           EVALUATE I-BOAT-TYPE
               WHEN 'B'
                   MOVE 'BASS BOAT' TO C-BOAT-TYPE
               WHEN 'P'
                   MOVE 'PONTOON' TO C-BOAT-TYPE
               WHEN 'S'
                   MOVE 'SKI BOAT' TO C-BOAT-TYPE
               WHEN 'J'
                   MOVE 'JOHN BOAT' TO C-BOAT-TYPE
               WHEN 'C'
                   MOVE 'CANOE' TO C-BOAT-TYPE
               WHEN 'R'
                   MOVE 'CABIN CRUISER' TO C-BOAT-TYPE.
           MOVE C-BOAT-TYPE TO O-BOAT-TYPE.
           
           IF MORE-RECS = 'YES'
               WRITE PRTLINE FROM BOAT-TYPE-LINE
               AFTER ADVANCING 2 LINES.

           WRITE PRTLINE FROM BLANK-LINE
               AFTER ADVANCING 1 LINE.

           ADD C-BOAT-CTR TO C-GT-BOAT-CTR.
           ADD C-TOTAL-COST TO C-GT-TOTAL-COST.
           MOVE I-BOAT-TYPE TO H-BOAT-TYPE.

           MOVE 0 TO C-BOAT-CTR.
           MOVE 0 TO C-MJ-TOTAL-COST.
                  
       9100-READ.
           READ BOAT-CONTROL
               AT END
                   MOVE 'NO' TO MORE-RECS.

       9200-HEADINGS.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.
           EVALUATE I-BOAT-TYPE
               WHEN 'B'
                   MOVE 'BASS BOAT' TO C-BOAT-TYPE
               WHEN 'P'
                   MOVE 'PONTOON' TO C-BOAT-TYPE
               WHEN 'S'
                   MOVE 'SKI BOAT' TO C-BOAT-TYPE
               WHEN 'J'
                   MOVE 'JOHN BOAT' TO C-BOAT-TYPE
               WHEN 'C'
                   MOVE 'CANOE' TO C-BOAT-TYPE
               WHEN 'R'
                   MOVE 'CABIN CRUISER' TO C-BOAT-TYPE.
           MOVE C-BOAT-TYPE TO O-BOAT-TYPE.
           MOVE C-BOAT-TYPE TO O-MJ-BOAT-TYPE.

           WRITE PRTLINE FROM COMPANY-TITLE
               AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM COLLUMN-HEADINGS1
               AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM COLLUMN-HEADINGS2
               AFTER ADVANCING 1 LINE.
               WRITE PRTLINE FROM BOAT-TYPE-LINE
                   AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM BLANK-LINE
               AFTER ADVANCING 1 LINE.

