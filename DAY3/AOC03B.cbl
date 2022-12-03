      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    AOC03B
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
         05  LEN1                 PIC 9(18) VALUE 0.
         05  LEN2                 PIC 9(18) VALUE 0.
         05  LEN3                 PIC 9(18) VALUE 0.
         05  PFIRST               PIC X(170).
         05  PSECOND              PIC X(170).
         05  PTHIRD               PIC X(170).
         05  PTEMP                PIC X(170).
         05  TMPSTR               PIC X(1).
         05  COUN                 PIC 9(1) VALUE 1.
         05  ALPHA                PIC X(52).
         05  I                    PIC 9(18) VALUE 1.
         05  II                   PIC 9(18) VALUE 1.
         05  III                  PIC 9(18) VALUE 1.
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
               PERFORM PREPARE
               IF COUN = 4
                   PERFORM FIND-SAME-X
                   MOVE 1 TO COUN 
                   PERFORM RES
               END-IF 
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

       RES.
           MOVE 0 TO I
           MOVE 0 TO II
           MOVE 1 TO COUN
           MOVE 0 TO LEN1 
           MOVE 0 TO LEN2
           MOVE 0 TO LEN3.

       READ-RECORD.
           READ DATAIN
               AT END MOVE 'Y' TO LASTREC
           END-READ.
      *

       PREPARE.
           EVALUATE TRUE 
               WHEN COUN = 1
                   MOVE RUCKSACK TO PFIRST
                   INSPECT FUNCTION REVERSE(RUCKSACK) 
      -            TALLYING LEN1 FOR LEADING SPACES
                   COMPUTE LEN1 = LENGTH OF RUCKSACK - LEN1
               WHEN COUN = 2
                   MOVE RUCKSACK TO PSECOND 
                   INSPECT FUNCTION REVERSE(RUCKSACK) 
      -            TALLYING LEN2 FOR LEADING SPACES
                   COMPUTE LEN2 = LENGTH OF RUCKSACK - LEN2
               WHEN COUN = 3
                   MOVE RUCKSACK TO PTHIRD 
                   INSPECT FUNCTION REVERSE(RUCKSACK) 
      -            TALLYING LEN3 FOR LEADING SPACES
                   COMPUTE LEN3 = LENGTH OF RUCKSACK - LEN3
           END-EVALUATE
           ADD 1 TO COUN.
      *      

       FIND-SAME-X.
           MOVE 0 TO DONE
           PERFORM FIND-SAME-Y VARYING I FROM 1 BY 1 UNTIL I = LEN1 + 1.


      *
      
       FIND-SAME-Y.
           PERFORM LOOP_F VARYING II FROM 1 BY 1 UNTIL II = LEN2 + 1.
      *

       LOOP_F.
           IF PFIRST (I:1) = PSECOND (II:1)
               PERFORM LOOP VARYING III FROM 1 BY 1 UNTIL III = LEN3 + 1
           END-IF.

       LOOP.
           IF PSECOND(II:1) = PTHIRD(III:1)
               MOVE 0 TO DONE
               PERFORM GET-SCORE VARYING II FROM 1 BY 1 UNTIL DONE = 1
               COMPUTE I = LEN1
               COMPUTE II = LEN2
               COMPUTE III = LEN3
           END-IF.

       GET-SCORE.
           IF ALPHA(II:1) = PTHIRD(III:1)
               ADD II TO SUMUP
               MOVE 1 TO DONE
           END-IF.
      *     
      
       WRITE-RECORD.
           MOVE SUMUP TO OSUM
           WRITE DATA-OUT.
      *
