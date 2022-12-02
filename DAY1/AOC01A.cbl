      *-----------------------
      * Copyright Contributors to the COBOL Programming Course
      * SPDX-License-Identifier: CC-BY-4.0
      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    AOC01A
       AUTHOR.        L. JAKS
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATAOUT ASSIGN TO AOCOUT.
           SELECT DATAIN   ASSIGN TO AOCIN.
      *SELECT clause creates an internal file name
      *ASSIGN clause creates a name for an external data source,
      *which is associated with the JCL DDNAME used by the z/OS
      *e.g. ACCTREC is linked in JCL file CBL0001J to &SYSUID..DATA
      *where &SYSUID. stands for Your z/OS user id
      *e.g. if Your user id is Z54321,
      *the data set used for ACCTREC is Z54321.DATA	
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  DATAOUT RECORDING MODE F.
       01  DATA-OUT.
           05  MAX            PIC 9(18).
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
         05 TMAX              PIC 9(18) VALUE 0.
      *------------------
       PROCEDURE DIVISION.
      *------------------
       OPEN-FILES.
           OPEN INPUT  DATAIN.
           OPEN OUTPUT DATAOUT.
      *
       READ-NEXT-RECORD.
           PERFORM READ-RECORD
      *     The previous statement is needed before entering the loop.
      *     Both the loop condition LASTREC = 'Y'
      *     and the call to WRITE-RECORD depend on READ-RECORD having
      *     been executed before.
      *     The loop starts at the next line with PERFORM UNTIL
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
               IF SUMUP > TMAX 
                   MOVE SUMUP TO TMAX 
               END-IF 
               MOVE 0 TO SUMUP
           ELSE
               COMPUTE SUMUP = FUNCTION NUMVAL(CALORIES) + SUMUP
           END-IF.
      *
       WRITE-RECORD.
           MOVE TMAX TO MAX
           WRITE DATA-OUT.
      *

