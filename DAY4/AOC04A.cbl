      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    AOC04A
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
           05  DAT            PIC X(170).
      *
       WORKING-STORAGE SECTION.
       01 FLAGS.
         05 LASTREC           PIC X VALUE SPACE.

       01 TEMP.
         05  NUM11                 PIC 9(18) VALUE 0.
         05  NUM12                 PIC 9(18) VALUE 0.
         05  NUM21                 PIC 9(18) VALUE 0.
         05  NUM22                 PIC 9(18) VALUE 0.
         05  I                     PIC 9(18) VALUE 0.
         05  II                    PIC 9(18) VALUE 0.
         05  TMP                   PIC 9(18) VALUE 0.
         05  FDON                  PIC 9(18) VALUE 0.
         05  TMPS                  PIC X(170) VALUE SPACES.
         05  PLEND                PIC 9(1) VALUE 0.

         05 SUMUP                  PIC 9(18) VALUE 0.
         
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
               MOVE 0 TO PLEND
               PERFORM PREPARE
               PERFORM CHECK-IF
               PERFORM READ-RECORD
           END-PERFORM.
      *

       PREPARE.
           PERFORM DIV VARYING I FROM 1 BY 1 UNTIL I = 170 OR PLEND = 1.

      *    2-4,6-8
       DIV.
           EVALUATE TRUE
               WHEN DAT(I:1) = '-'
                   COMPUTE TMP = I - II
                   IF FDON = 0
                       MOVE DAT(TMP:II) TO TMPS
                       COMPUTE NUM11 = FUNCTION NUMVAL(TMPS)
                       MOVE 1 TO FDON
                   ELSE 
                       MOVE DAT(TMP:II) TO TMPS
                       COMPUTE NUM21 = FUNCTION NUMVAL(TMPS)
                       MOVE 0 TO FDON
                   END-IF
                   MOVE 0 TO II
               WHEN DAT(I:1) = ','
                   COMPUTE TMP = I - II
                   MOVE DAT(TMP:II) TO TMPS
                   COMPUTE NUM12 = FUNCTION NUMVAL(TMPS)
                   MOVE 0 TO II
               WHEN DAT(I:1) = SPACE
                   COMPUTE TMP = I - II
                   MOVE DAT(TMP:II) TO TMPS
                   COMPUTE NUM22 = FUNCTION NUMVAL(TMPS)
                   MOVE 0 TO II
                   MOVE 1 TO PLEND
               WHEN OTHER 
                   ADD 1 TO II
           END-EVALUATE.
      *

       CHECK-IF.
           EVALUATE TRUE
               WHEN NUM11 >= NUM21 AND NUM12 <= NUM22
                   ADD 1 TO SUMUP
               WHEN NUM21 >= NUM11 AND NUM22 <= NUM12
                   ADD 1 TO SUMUP
           END-EVALUATE.
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

       WRITE-RECORD.
      *    THE PEFORM PERFORMS ONE MORE TIME AT THE END, SO -1 IT IS 
           COMPUTE OSUM = SUMUP - 1
           WRITE DATA-OUT.
      *
