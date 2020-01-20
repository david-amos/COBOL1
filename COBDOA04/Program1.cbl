       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COBDOA04.
       DATE-WRITTEN.   1/10/2020.
       DATE-COMPILED.
      *---------------------------------------------------------
      *THIS PROGRAM READ RECORDS ABOUT CONDO RENTALS
      *IT WILL CALCULATE AND OUTPUT THE CHARGES FOR THE RENTALS
      *---------------------------------------------------------

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OZARK
               ASSIGN TO "C:\IHCC\COBOL\OZARK.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRTOUT
               ASSIGN TO "C:\IHCC\COBOL\VACATION.PRT"
               ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  OZARK
           LABEL RECORD IS STANDARD
           DATA RECORD IS OZARK-REC
           RECORD CONTAINS 31 CHARACTERS.
       01  OZARK-REC.
           05 I-GUEST          PIC X(20).
           05 I-CONDO          PIC XX.
           05 I-BEDROOMS       PIC 9.
           05 I-NIGHTS         PIC 99.
           05 I-PETS           PIC X.
           05 I-HOTTUB         PIC X.
           05 I-DOCKSLIP       PIC 99V99.

       FD  PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 54.

       01  PRTLINE             PIC X(132).

       WORKING-STORAGE SECTION.
       01  WORK-AREA.
           05 MORE-RECS            PIC X           VALUE 'Y'.
           05 C-NIGHTLY-FEE        PIC 9(5)V99.
           05 C-CLEANING-FEE       PIC 9(4)V99.
           05 C-CONDO-FEE          PIC 9(6)V99.
           05 C-DOCKSLIP-FEE       PIC 9(4)V99.
           05 C-SUBTOTAL           PIC 9(6)V99.
           05 C-DEAL-AMOUNT        PIC S9(5)V99.
           05 C-AMOUNT-DUE         PIC 9(6)V99.
           05 C-GT-SUBTOTAL        PIC 9(8)V99     VALUE 0.
           05 C-GT-DEAL-AMOUNT     PIC S9(7)V99    VALUE 0.
           05 C-GT-AMOUNT-DUE      PIC 9(8)V99     VALUE 0.
           05 C-RENTAL-CTR         PIC 999         VALUE 0.
           05 C-FREE-NIGHTS        PIC 99V9        VALUE 0.
           05 C-FREE-CLEANING      PIC 99          VALUE 0.
           05 C-PET-FEES           PIC 9(5)V99     VALUE 0.
           05 C-HOTTUB-FEES        PIC 9(5)V99     VALUE 0.
           05 C-PCTR               PIC 99          VALUE 0.
           

       01  CURRENT-DATE-AND-TIME.
           05 THIS-DATE.
               10 I-YY             PIC 9(4).
               10 I-MM             PIC 99.
               10 I-DD             PIC 99.
           05 I-TIME               PIC X(11).

       01  COMPANY-TITLE.
           05 FILLER               PIC X(6)        VALUE 'DATE:'.
           05 O-MM                 PIC 99.
           05 FILLER               PIC X           VALUE '/'.
           05 O-DD                 PIC 99.
           05 FILLER               PIC X           VALUE '/'.
           05 O-YY                 PIC 9(4).
           05 FILLER               PIC X(42)       VALUE SPACE.
           05 FILLER               PIC X(66)
                                   VALUE 'PMG MANAGEMENT'.
           05 FILLER               PIC X(6)        VALUE 'PAGE:'.
           05 O-PCTR               PIC Z9.

       01  REPORT-LINE.
           05 FILLER               PIC X(50)       VALUE 'COBDOA04'.
           05 FILLER               PIC X(21)
                                   VALUE 'LAKE OF THE OZARKS - '.
           05 O-MONTH              PIC X(61).

       01  COLLUMN-HEADINGS-1.
           05 FILLER               PIC X(38)       VALUE SPACES.
           05 FILLER               PIC X(10)       VALUE 'STAY'.
           05 FILLER               PIC X(12)       VALUE 'NIGHT'.
           05 FILLER               PIC X(48)
                                   VALUE 'CONDO CLEANING  DOCK SLIP'.
           05 FILLER               PIC X(14)       VALUE 'DEAL'.
           05 FILLER               PIC X(10)        VALUE 'AMOUNT'.

       01  COLLUMN-HEADINGS-2.
           05 FILLER               PIC X(17)       VALUE 'CONDOMINIUM'.
           05 FILLER               PIC X(20)       VALUE 'GUEST NAME'.
           05 FILLER               PIC X(13)       VALUE 'NIGHTS'.
           05 FILLER               PIC X(12)       VALUE 'FEE'.
           05 FILLER               PIC X(9)       VALUE 'FEE'.
           05 FILLER               PIC X(11)       VALUE 'FEE'.
           05 FILLER               PIC X(8)        VALUE 'FEE'.
           05 FILLER               PIC X(16)       VALUE 'SUBTOTAL'.
           05 FILLER               PIC X(19)       VALUE 'AMOUNT'.
           05 FILLER               PIC X(7)        VALUE 'DUE'.

       01  BLANK-LINE              PIC X(132)      VALUE SPACES.

       01  DETAIL-LINE.
           05 O-CONDO              PIC X(15).
           05 FILLER               PIC XX          VALUE SPACES.
           05 O-GUEST              PIC X(20).
           05 FILLER               PIC XX          VALUE SPACES.
           05 O-NIGHTS             PIC Z9.
           05 FILLER               PIC XXX         VALUE SPACES.
           05 O-NIGHTLY-FEE        PIC $$,$$$.99.
           05 FILLER               PIC X           VALUE SPACES.
           05 O-CONDO-FEE          PIC $$$$,$$$.99.
           05 FILLER               PIC XX          VALUE SPACES.
           05 O-CLEANING-FEE       PIC $$$$.99.
           05 FILLER               PIC XX          VALUE SPACES.
           05 O-DOCKSLIP-FEE       PIC $$,$$$.99.
           05 FILLER               PIC XX          VALUE SPACES.
           05 O-SUBTOTAL           PIC $$$$,$$$.99.
           05 FILLER               PIC X(4)        VALUE SPACES.
           05 O-DEAL-AMOUNT        PIC $$$,$$$.99+.
           05 FILLER               PIC X(4)        VALUE SPACES.
           05 O-AMOUNT-DUE         PIC $$$$,$$$.99.
           05 FLAG                 PIC X(4).

       01  GRANDTOTALS.
           05 FILLER               PIC X(84)       VALUE 
                                   'GRAND TOTALS:'.
           05 O-GT-SUBTOTAL        PIC $$$,$$$,$$$.99.
           05 FILLER               PIC X           VALUE SPACES.
           05 O-GT-DEAL-AMOUNT     PIC $$,$$$,$$$.99+.
           05 FILLER               PIC X           VALUE SPACES.
           05 O-GT-AMOUNT-DUE      PIC $$$,$$$,$$$.99.
           05 FILLER               PIC X(4)        VALUE SPACES.

       01 GT-COUNTERS.
           05 FILLER               PIC X(12)       VALUE SPACES.
           05 FILLER               PIC X(19)       VALUE
                                   'NUMBER OF RENTALS: '.
           05 O-RENTAL-CTR         PIC ZZ9.
           05 FILLER               PIC X(6)        VALUE SPACES.
           05 FILLER               PIC X(13)       VALUE 'FREE NIGHTS:'.
           05 O-FREE-NIGHT-CTR     PIC ZZ.9.
           05 FILLER               PIC XXX         VALUE SPACES.
           05 FILLER               PIC X(15)       VALUE
                                   'FREE CLEANING:'.
           05 O-FREE-CLEANING-CTR  PIC Z9.
           05 FILLER               PIC X(55)       VALUE SPACES.

       01  GT-ACCUMULATORS.
           05 FILLER               PIC X(14)       VALUE SPACES.
           05 FILLER               PIC X(10)       VALUE 'PET FEES:'.
           05 O-GT-PET-FEES        PIC $$$,$$$.99.
           05 FILLER               PIC X(19)       VALUE SPACES.
           05 FILLER               PIC X(14)       VALUE
                                   'HOT TUB FEES:'.
           05 O-GT-HOTTUB-FEES     PIC $$$,$$$.99.
           05 FILLER               PIC X(55).

       PROCEDURE DIVISION.

       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS = 'N'.
           PERFORM 3000-CLOSING.
           STOP RUN.

       1000-INIT.
           OPEN INPUT OZARK.
           OPEN OUTPUT PRTOUT.

           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE I-YY TO O-YY.
           MOVE I-DD TO O-DD.
           MOVE I-MM TO O-MM.

           EVALUATE I-MM
               WHEN 01
                   MOVE 'JANUARY' TO O-MONTH
               WHEN 02
                   MOVE 'FEBRUARY' TO O-MONTH
               WHEN 03
                   MOVE 'MARCH' TO O-MONTH
               WHEN 04
                   MOVE 'APRIL' TO O-MONTH
               WHEN 05
                   MOVE 'MAY' TO O-MONTH
               WHEN 06
                   MOVE 'JUNE' TO O-MONTH
               WHEN 07
                   MOVE 'JULY' TO O-MONTH
               WHEN 08
                   MOVE 'AUGUST' TO O-MONTH
               WHEN 09
                   MOVE 'SEPTEMBER' TO O-MONTH
               WHEN 10
                   MOVE 'OCTOBER' TO O-MONTH
               WHEN 11
                   MOVE 'NOVEMBER' TO O-MONTH
               WHEN 12
                   MOVE 'DECEMBER' TO O-MONTH.
           PERFORM 9000-READ.
           PERFORM 9100-HEADINGS.

       2000-MAINLINE.
           PERFORM 2100-CALCS.
           PERFORM 2200-OUTPUT.
           PERFORM 9000-READ.

       2100-CALCS.
           MOVE 0 TO C-DEAL-AMOUNT.
           MOVE SPACES TO FLAG.

           EVALUATE I-CONDO
               WHEN 'HB'
                   PERFORM 2110-HORSE
               WHEN 'OB'
                   PERFORM 2120-BEACH
               WHEN 'PP'
                   PERFORM 2130-PISTOL
               WHEN 'RB'
                   PERFORM 2140-BAY
               WHEN 'SB'
                   PERFORM 2150-SHAWNEE
               WHEN 'L '
                   PERFORM 2160-LEDGES
               WHEN 'HT'
                   PERFORM 2170-HARBOUR
               WHEN 'CP'
                   PERFORM 2180-COMPASSE.

           MULTIPLY I-DOCKSLIP BY I-NIGHTS GIVING C-DOCKSLIP-FEE 
               ROUNDED.
           ADD C-CONDO-FEE C-CLEANING-FEE TO C-DOCKSLIP-FEE
               GIVING C-SUBTOTAL ROUNDED.
           ADD C-SUBTOTAL TO C-DEAL-AMOUNT GIVING C-AMOUNT-DUE ROUNDED.

           ADD 1 TO C-RENTAL-CTR.
           ADD C-SUBTOTAL TO C-GT-SUBTOTAL.
           ADD C-DEAL-AMOUNT TO C-GT-DEAL-AMOUNT.
           ADD C-AMOUNT-DUE TO C-GT-AMOUNT-DUE.

       2110-HORSE.
           MOVE 'HORSESHOE BEND' TO O-CONDO.
           MULTIPLY 99.5 BY I-BEDROOMS GIVING C-NIGHTLY-FEE.
           MOVE 100 TO C-CLEANING-FEE.
           MULTIPLY C-NIGHTLY-FEE BY I-NIGHTS GIVING C-CONDO-FEE 
               ROUNDED.
           IF I-HOTTUB = 'Y'
               MULTIPLY .05 BY C-CONDO-FEE GIVING
                   C-DEAL-AMOUNT ROUNDED
               COMPUTE C-HOTTUB-FEES ROUNDED = C-HOTTUB-FEES + (.05 * 
                   C-CONDO-FEE).
           IF I-PETS = 'Y'
               COMPUTE C-DEAL-AMOUNT = C-DEAL-AMOUNT +
                   (C-CONDO-FEE * .1)
               COMPUTE C-PET-FEES ROUNDED = C-PET-FEES + (
                   C-CONDO-FEE * .1).

       2120-BEACH.
           MOVE 'OSAGE BEACH' TO O-CONDO.
           MULTIPLY 188 BY I-BEDROOMS GIVING C-NIGHTLY-FEE ROUNDED.
           MOVE 150 TO C-CLEANING-FEE.
           MULTIPLY C-NIGHTLY-FEE BY I-NIGHTS GIVING C-CONDO-FEE.
           IF I-NIGHTS >= 7
               MULTIPLY -1 BY C-NIGHTLY-FEE GIVING C-DEAL-AMOUNT ROUNDED
               ADD 1 TO C-FREE-NIGHTS.
           IF I-PETS = 'Y'
               COMPUTE C-DEAL-AMOUNT ROUNDED = C-DEAL-AMOUNT + 
                   C-CONDO-FEE * .1
               COMPUTE C-PET-FEES ROUNDED = C-PET-FEES + 
                   C-CONDO-FEE * .1.

       2130-PISTOL.
           MOVE 'PISTOL POINT' TO O-CONDO.
           MULTIPLY 50 BY I-BEDROOMS GIVING C-NIGHTLY-FEE ROUNDED.
           MOVE 75 TO C-CLEANING-FEE.
           MULTIPLY I-NIGHTS BY C-NIGHTLY-FEE GIVING C-CONDO-FEE 
               ROUNDED.

       2140-BAY.
           MOVE 'REGATTA BAY' TO O-CONDO.
           MULTIPLY 62.1 BY I-BEDROOMS GIVING C-NIGHTLY-FEE ROUNDED.
           MOVE 75 TO C-CLEANING-FEE.
           MULTIPLY I-NIGHTS BY C-NIGHTLY-FEE GIVING C-CONDO-FEE 
               ROUNDED.
           IF I-NIGHTS > 5
               MOVE -75 TO C-DEAL-AMOUNT
               ADD 1 TO C-FREE-CLEANING.

       2150-SHAWNEE.
           MOVE 'SHAWNEE BEND' TO O-CONDO.
           MULTIPLY 100 BY I-BEDROOMS GIVING C-NIGHTLY-FEE ROUNDED.
           MOVE 150 TO C-CLEANING-FEE.
           MULTIPLY I-NIGHTS BY C-NIGHTLY-FEE GIVING C-CONDO-FEE 
               ROUNDED.
           IF I-PETS = 'Y'
               COMPUTE C-DEAL-AMOUNT ROUNDED = C-CONDO-FEE * .1
               COMPUTE C-PET-FEES ROUNDED = C-PET-FEES + 
                   C-CONDO-FEE * .1.

       2160-LEDGES.
           MOVE 'LEDGES' TO O-CONDO.
           MULTIPLY 76.35 BY I-BEDROOMS GIVING C-NIGHTLY-FEE ROUNDED.
           MOVE 0 TO C-CLEANING-FEE.
           MULTIPLY I-NIGHTS BY C-NIGHTLY-FEE GIVING C-CONDO-FEE 
               ROUNDED.
           IF I-HOTTUB = 'Y'
               MULTIPLY .075 BY C-CONDO-FEE GIVING
                   C-DEAL-AMOUNT ROUNDED
               COMPUTE C-HOTTUB-FEES ROUNDED = C-HOTTUB-FEES + (.075 * 
                   C-CONDO-FEE).

       2170-HARBOUR.
           MOVE 'HARBOUR TOWNE' TO O-CONDO.
           MULTIPLY 50 BY I-BEDROOMS GIVING C-NIGHTLY-FEE ROUNDED.
           MOVE 100 TO C-CLEANING-FEE.
           MULTIPLY I-NIGHTS BY C-NIGHTLY-FEE GIVING C-CONDO-FEE 
               ROUNDED.
           IF I-NIGHTS >= 3
               MULTIPLY -.5 BY C-NIGHTLY-FEE GIVING C-DEAL-AMOUNT
               ADD .5 TO C-FREE-NIGHTS.

       2180-COMPASSE.
           MOVE 'COMPASSE POINTE' TO O-CONDO.
            MULTIPLY 125 BY I-BEDROOMS GIVING C-NIGHTLY-FEE ROUNDED.
           MOVE 0 TO C-CLEANING-FEE.
           MULTIPLY I-NIGHTS BY C-NIGHTLY-FEE GIVING C-CONDO-FEE 
               ROUNDED.
           IF I-NIGHTS >= 5
               MULTIPLY -1 BY C-NIGHTLY-FEE GIVING C-DEAL-AMOUNT
               ADD 1 TO C-FREE-NIGHTS.

       2200-OUTPUT.
           MOVE I-GUEST TO O-GUEST.
           MOVE I-NIGHTS TO O-NIGHTS.
           MOVE C-NIGHTLY-FEE TO O-NIGHTLY-FEE.
           MOVE C-CONDO-FEE TO O-CONDO-FEE.
           MOVE C-CLEANING-FEE TO O-CLEANING-FEE.
           MOVE C-DOCKSLIP-FEE TO O-DOCKSLIP-FEE.
           MOVE C-SUBTOTAL TO O-SUBTOTAL.
           MOVE C-DEAL-AMOUNT TO O-DEAL-AMOUNT.
           MOVE C-AMOUNT-DUE TO O-AMOUNT-DUE.
           IF C-AMOUNT-DUE > 750
               MOVE '****' TO FLAG.
           WRITE PRTLINE FROM DETAIL-LINE
               AFTER ADVANCING 1 LINE
                   AT EOP
                       PERFORM 9100-HEADINGS.

       3000-CLOSING.
           MOVE C-GT-SUBTOTAL TO O-GT-SUBTOTAL.
           MOVE C-GT-DEAL-AMOUNT TO O-GT-DEAL-AMOUNT.
           MOVE C-GT-AMOUNT-DUE TO O-GT-AMOUNT-DUE.
           MOVE C-RENTAL-CTR TO O-RENTAL-CTR.
           MOVE C-FREE-NIGHTS TO O-FREE-NIGHT-CTR.
           MOVE C-FREE-CLEANING TO O-FREE-CLEANING-CTR.
           MOVE C-PET-FEES TO O-GT-PET-FEES.
           MOVE C-HOTTUB-FEES TO O-GT-HOTTUB-FEES.

           WRITE PRTLINE FROM GRANDTOTALS
               AFTER ADVANCING 3 LINES.
           WRITE PRTLINE FROM GT-COUNTERS
               AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM GT-ACCUMULATORS
               AFTER ADVANCING 1 LINE.

           CLOSE OZARK.
           CLOSE PRTOUT.


       9000-READ.
           READ OZARK
               AT END
                   MOVE 'N' TO MORE-RECS.

       9100-HEADINGS.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.
           WRITE PRTLINE FROM COMPANY-TITLE
               AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM REPORT-LINE
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM COLLUMN-HEADINGS-1
               AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM COLLUMN-HEADINGS-2
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM BLANK-LINE
               AFTER ADVANCING 1 LINE.

