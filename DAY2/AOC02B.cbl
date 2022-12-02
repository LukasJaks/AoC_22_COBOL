      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    AOC02B
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
           05  GAME            PIC X(170).
      *
       WORKING-STORAGE SECTION.
       01 FLAGS.
         05 LASTREC           PIC X VALUE SPACE.

       01 TEMP.
         05 SUMUP             PIC 9(18) VALUE 0.
         05  OP               PIC X(1).
         05  ME               PIC X(1).
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
           MOVE GAME(1:1) TO OP
           MOVE GAME(3:1) TO ME
           EVALUATE TRUE

               WHEN ME = 'X' AND OP = 'A'
                   ADD 3 TO SUMUP
               WHEN ME = 'X' AND OP = 'B'
                   ADD 1 TO SUMUP
               WHEN ME = 'X' AND OP = 'C'
                   ADD 2 TO SUMUP

               WHEN ME = 'Y' AND OP = 'A'
                   COMPUTE SUMUP = SUMUP + 1 + 3
               WHEN ME = 'Y' AND OP = 'B'
                   COMPUTE SUMUP = SUMUP + 2 + 3
               WHEN ME = 'Y' AND OP = 'C'
                   COMPUTE SUMUP = SUMUP + 3 + 3

               WHEN ME = 'Z' AND OP = 'A'
                   COMPUTE SUMUP = SUMUP + 2 + 6
               WHEN ME = 'Z' AND OP = 'B'
                   COMPUTE SUMUP = SUMUP + 3 + 6
               WHEN ME = 'Z' AND OP = 'C'
                   COMPUTE SUMUP = SUMUP + 1 + 6

           END-EVALUATE.

       WRITE-RECORD.
           MOVE SUMUP TO MAX
           WRITE DATA-OUT.
      *
