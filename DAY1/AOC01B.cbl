      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    AOC01B
       AUTHOR.        L. JAKS
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATAOUT ASSIGN TO AOCOUT.
           SELECT DATAIN   ASSIGN TO AOCIN.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  DATAOUT RECORDING MODE F.
       01  DATA-OUT.
           05  MAX               PIC 9(18).
      *
       FD  DATAIN RECORDING MODE F.
       01  DATA-IN.
           05  CALORIES            PIC X(170).
      *
       WORKING-STORAGE SECTION.
       01 FLAGS.
         05 LASTREC           PIC X VALUE SPACE.

       01 TEMP.
         05 SUMUP             PIC 9(18) VALUE 0.
         05 TMAX1             PIC 9(18) VALUE 0.
         05 TMAX2             PIC 9(18) VALUE 0.
         05 TMAX3             PIC 9(18) VALUE 0.
      *------------------
       PROCEDURE DIVISION.
      *------------------
       OPEN-FILES.
           OPEN INPUT  DATAIN.
           OPEN OUTPUT DATAOUT.
      *
       READ-NEXT-RECORD.
           PERFORM READ-RECORD
           PERFORM UNTIL LASTREC = 'Y'
               PERFORM CHECK-RECORD
               PERFORM READ-RECORD
           END-PERFORM
           .
      *
       CLOSE-STOP.
           PERFORM WRITE-RECORD
           CLOSE DATAIN.
           CLOSE DATAOUT.
           GOBACK.
      *
       READ-RECORD.
           READ DATAIN
               AT END MOVE 'Y' TO LASTREC
           END-READ.
      *
       CHECK-RECORD.
           IF CALORIES = SPACES 
               EVALUATE TRUE
                   WHEN SUMUP > TMAX1
                       MOVE TMAX2 TO TMAX3 
                       MOVE TMAX1 TO TMAX2
                       MOVE SUMUP TO TMAX1 
                   WHEN SUMUP > TMAX2
                       MOVE TMAX2 TO TMAX3 
                       MOVE SUMUP TO TMAX2 
                   WHEN SUMUP > TMAX3
                       MOVE SUMUP TO TMAX3 
               END-EVALUATE 
               MOVE 0 TO SUMUP
           ELSE
               COMPUTE SUMUP = FUNCTION NUMVAL(CALORIES) + SUMUP
           END-IF.
      *
       WRITE-RECORD.
           COMPUTE MAX = TMAX1 + TMAX2 + TMAX3
           WRITE DATA-OUT.
      *
