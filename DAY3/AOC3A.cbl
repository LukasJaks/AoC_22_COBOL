      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    AOC03A
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
           05  OSUM               PIC 9(18).

      *
       FD  DATAIN RECORDING MODE F.
       01  DATA-IN.
           05  RUCKSACK            PIC X(170).
      *
       WORKING-STORAGE SECTION.
       01 FLAGS.
         05 LASTREC           PIC X VALUE SPACE.

       01 TEMP.
         05  SUMUP                PIC 9(18) VALUE 0.
         05  LEN                  PIC 9(18) VALUE 0.
         05  HLEN                 PIC 9(18) VALUE 0.
         05  SLEN                 PIC 9(18) VALUE 0.
         05  PLEFT                PIC X(170).
         05  PRIGHT               PIC X(170).
         05  ALPHA                PIC X(52).
         05  I                    PIC 9(18) VALUE 1.
         05  II                   PIC 9(18) VALUE 1.
         05  DONE                 PIC 9(1) VALUE 0.
      *------------------
       PROCEDURE DIVISION.
      *------------------
       OPEN-FILES.
           OPEN INPUT  DATAIN.
           OPEN OUTPUT DATAOUT.
      *

       READ-NEXT-RECORD.
           MOVE 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
      -    TO ALPHA
           PERFORM READ-RECORD
           PERFORM UNTIL LASTREC = 'Y'
               MOVE 0 TO LEN
               PERFORM PREPARE
               PERFORM FIND-SAME-X
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

       PREPARE.
           INSPECT FUNCTION REVERSE(RUCKSACK) 
      -    TALLYING LEN FOR LEADING SPACES
           COMPUTE LEN = LENGTH OF RUCKSACK - LEN
           COMPUTE HLEN = LEN / 2
           COMPUTE SLEN = HLEN + 1
           MOVE RUCKSACK(1:HLEN) TO PLEFT
           MOVE RUCKSACK(SLEN:HLEN) TO PRIGHT.
      *      

       FIND-SAME-X.
           PERFORM FIND-SAME-Y VARYING I FROM 1 BY 1 UNTIL I = HLEN + 1.
      *
      
       FIND-SAME-Y.
           PERFORM LOOP VARYING II FROM 1 BY 1 UNTIL II = HLEN + 1.
      *

       LOOP.
           IF PLEFT(I:1) = PRIGHT(II:1)
               MOVE 0 TO DONE
               PERFORM GET-SCORE VARYING II FROM 1 BY 1 UNTIL DONE = 1
               MOVE HLEN TO I
               MOVE HLEN TO II
           END-IF.

       GET-SCORE.
           IF ALPHA(II:1) = PLEFT(I:1)
               ADD II TO SUMUP
               MOVE 1 TO DONE
               MOVE HLEN TO I
               MOVE HLEN TO II
           END-IF.
      *     
      
       WRITE-RECORD.
           MOVE SUMUP TO OSUM
           WRITE DATA-OUT.
      *
