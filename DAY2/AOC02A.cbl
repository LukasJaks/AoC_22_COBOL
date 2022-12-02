      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    AOC02A
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
               WHEN ME = 'X' AND OP = 'C'
                   ADD 6 TO SUMUP
               WHEN ME = 'Y' AND OP = 'A'
                   ADD 6 TO SUMUP
               WHEN ME = 'Z' AND OP = 'B'
                   ADD 6 TO SUMUP
           END-EVALUATE

           EVALUATE TRUE
               WHEN ME = 'X' AND OP = 'A'
                   ADD 3 TO SUMUP
               WHEN ME = 'Y' AND OP = 'B'
                   ADD 3 TO SUMUP
               WHEN ME = 'Z' AND OP = 'C'
                   ADD 3 TO SUMUP
           END-EVALUATE

           EVALUATE TRUE
               WHEN ME = 'X'
                   ADD 1 TO SUMUP
               WHEN ME = 'Y'
                   ADD 2 TO SUMUP
               WHEN ME = 'Z'
                   ADD 3 TO SUMUP
           END-EVALUATE.

       WRITE-RECORD.
           MOVE SUMUP TO MAX
           WRITE DATA-OUT.
      *
