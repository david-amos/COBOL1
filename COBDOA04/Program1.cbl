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
