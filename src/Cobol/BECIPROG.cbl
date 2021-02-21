       IDENTIFICATION DIVISION.
       PROGRAM-ID. BETACICS.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *  -----------------------------------------------------------
      *  GENERAL WORK AREAS
      *  -----------------------------------------------------------
       01  WORK-AREAS.
           05  RECLEN           PIC S9(4) COMP VALUE 185.

       01 WS-X-LINE.
           05 WS-X-KEY  PIC 9(5) COMP.
           05 FILLER    PIC X(4) VALUE SPACES.
           05 WS-X-VAL  PIC X(12).
           05 FILLER    PIC X(160) VALUE SPACES.

       01 WS-Y-LINE.
           05 WS-Y-KEY PIC 9(5) COMP.
           05 FILLER   PIC X(4) VALUE SPACES.
           05 WS-Y-VAL PIC X(12).
           05 FILLER   PIC X(160) VALUE SPACES.

       01 WS-ITER  PIC 9(5) VALUE 0.
       01 WS-INC   PIC S9(3)V9(12) VALUE +0.000000
           SIGN IS LEADING SEPARATE CHARACTER.
       01 WS-X-SUM   PIC S9(3)V9(12) VALUE +0.000000
           SIGN IS LEADING SEPARATE CHARACTER.
       01 WS-Y-SUM   PIC S9(3)V9(12) VALUE +0.000000
           SIGN IS LEADING SEPARATE CHARACTER.
       01 WS-X-AVG PIC S9(3)V9(12) VALUE +0.000000
           SIGN IS LEADING SEPARATE CHARACTER.
       01 WS-Y-AVG PIC S9(3)V9(12) VALUE +0.000000
           SIGN IS LEADING SEPARATE CHARACTER.
       01 WS-COVAR PIC S9(3)V9(12) VALUE +0.000000
           SIGN IS LEADING SEPARATE CHARACTER.
       01 WS-X-VAR PIC S9(3)V9(12) VALUE +0.000000
           SIGN IS LEADING SEPARATE CHARACTER.
       01 WS-BETA  PIC S9(3)V9(12) VALUE +0.000000
           SIGN IS LEADING SEPARATE CHARACTER.
       01 WS-EOF   PIC X(1) VALUE '0'.

       01 WS-KEY    PIC 9(5) VALUE 1.
       01 WS-KEY-LEN PIC S9(4) COMP VALUE 5.
       01 WS-INPUT1 PIC X(8) VALUE 'BECIIN01'.
       01 WS-INPUT2 PIC X(8) VALUE 'BECIIN02'.
       01 RESPCODE  PIC S9(8) COMP-4 VALUE 0.
       01 RESPCODE2 PIC S9(8) COMP-4 VALUE 0.
       01 OUTPUTSTRING PIC X(10) VALUE 'MYOUTPUT12'.

      *  -----------------------------------------------------------
      * Container name declarations
      *  -----------------------------------------------------------
       01 OUTPUT-CONT PIC X(16) VALUE 'OUTPUTDATA'.
       01 BEGIN-CONT  PIC X(16) VALUE 'BEGINDATA1'.
       01 END-CONT    PIC X(16) VALUE 'ENDDATA001'.
       01 RESP-CONT   PIC X(16) VALUE 'CICSRC'.
       01 INPUTX-CONT PIC X(16) VALUE 'INPUTXDATA'.
       01 INPUTY-CONT PIC X(16) VALUE 'INPUTYDATA'.

      *  -----------------------------------------------------------
      * Data fields used by the program
      *  -----------------------------------------------------------
       01 RC-RECORD          PIC S9(8) COMP-4 VALUE 0.
       01 INPUTLENGTH        PIC S9(8) COMP-4.
       01 CHANNELNAME PIC X(16) VALUE SPACES.
       01 ABENDCODE          PIC X(4) VALUE SPACES.
       01 WS-X-NUM      PIC S9(3)V9(12)
           SIGN IS LEADING SEPARATE CHARACTER.
       01 WS-Y-NUM      PIC S9(3)V9(12)
           SIGN IS LEADING SEPARATE CHARACTER.
       01 WS-POS-HOLDER PIC X(5) VALUE '00001'.
       01 WS-INP-HOLDER PIC X(8) VALUE 'BECIIN01'.
       01 WS-BEGIN-NUM  PIC 9(5).
       01 WS-END-NUM    PIC 9(5).
       01 WS-ERROR-MSG  PIC X(8).

      *************
       PROCEDURE DIVISION.
      *  -----------------------------------------------------------
      *  Get the name of channel
      *  -----------------------------------------------------------
           EXEC CICS ASSIGN CHANNEL(CHANNELNAME)
                            END-EXEC.
      *  -----------------------------------------------------------
      *  If no channel is passed in, terminate with abend code NOCH
      *  -----------------------------------------------------------
           IF CHANNELNAME = SPACES THEN
               MOVE 'NOCH' TO ABENDCODE
               PERFORM ABEND-ROUTINE
           END-IF.

      *  -----------------------------------------------------------
      *  Read position arguments from the begin and end containers.
      *  -----------------------------------------------------------
           MOVE LENGTH OF WS-POS-HOLDER TO INPUTLENGTH.
           EXEC CICS GET CONTAINER(BEGIN-CONT)
               CHANNEL(CHANNELNAME)
               FLENGTH(INPUTLENGTH)
               INTO(WS-POS-HOLDER)
               RESP(RESPCODE)
               RESP2(RESPCODE2)
           END-EXEC.
           PERFORM ERROR-CHECK.

           COMPUTE WS-BEGIN-NUM = FUNCTION NUMVAL(WS-POS-HOLDER).

           MOVE '00010' TO WS-POS-HOLDER.
           MOVE LENGTH OF WS-POS-HOLDER TO INPUTLENGTH.
           EXEC CICS GET CONTAINER(END-CONT)
               CHANNEL(CHANNELNAME)
               FLENGTH(INPUTLENGTH)
               INTO(WS-POS-HOLDER)
               RESP(RESPCODE)
               RESP2(RESPCODE2)
           END-EXEC.
           PERFORM ERROR-CHECK.
           
           COMPUTE WS-END-NUM = FUNCTION NUMVAL(WS-POS-HOLDER).
      *  -----------------------------------------------------------
      *  Read input files arguments from containers.
      *  -----------------------------------------------------------
           MOVE 'BECIIN01' TO WS-INP-HOLDER.
           MOVE LENGTH OF WS-INP-HOLDER TO INPUTLENGTH.
           EXEC CICS GET CONTAINER(INPUTX-CONT)
               CHANNEL(CHANNELNAME)
               FLENGTH(INPUTLENGTH)
               INTO(WS-INP-HOLDER)
               RESP(RESPCODE)
               RESP2(RESPCODE2)
           END-EXEC.
           PERFORM ERROR-CHECK.
           MOVE WS-INP-HOLDER TO WS-INPUT1.

           MOVE 'BECIIN01' TO WS-INP-HOLDER.
           MOVE LENGTH OF WS-INP-HOLDER TO INPUTLENGTH.
           EXEC CICS GET CONTAINER(INPUTY-CONT)
               CHANNEL(CHANNELNAME)
               FLENGTH(INPUTLENGTH)
               INTO(WS-INP-HOLDER)
               RESP(RESPCODE)
               RESP2(RESPCODE2)
           END-EXEC.
           PERFORM ERROR-CHECK.
           MOVE WS-INP-HOLDER TO WS-INPUT2.

           MOVE RESPCODE TO RC-RECORD.
           EXEC CICS PUT CONTAINER(RESP-CONT)
                            FROM(RC-RECORD)
                            FLENGTH(LENGTH OF RC-RECORD)
                            BIT
                            RESP(RESPCODE)
           END-EXEC.
           PERFORM ERROR-CHECK.
           
      *  -----------------------------------------------------------
      *  Firstly, calculate the means
      *  -----------------------------------------------------------
           COMPUTE WS-ITER = WS-BEGIN-NUM - 1.
           PERFORM READ-RECORD
            PERFORM UNTIL WS-EOF = 'Y'
            PERFORM AVERAGE-PROC
            PERFORM READ-RECORD
            END-PERFORM
           .

           COMPUTE WS-X-AVG = WS-X-SUM / WS-INC.
           COMPUTE WS-Y-AVG = WS-Y-SUM / WS-INC.

      *  -----------------------------------------------------------
      *  Then calculate the beta coefficient
      *  -----------------------------------------------------------

           MOVE '0' TO WS-EOF.
           COMPUTE WS-ITER = WS-BEGIN-NUM - 1.
           PERFORM READ-RECORD
            PERFORM UNTIL WS-EOF = 'Y'
            PERFORM VARCOVAR-PROC
            PERFORM READ-RECORD
            END-PERFORM
           .

           COMPUTE WS-BETA = WS-COVAR / WS-X-VAR.

           EXEC CICS PUT CONTAINER(OUTPUT-CONT)
               FROM(WS-BETA)
               CHAR
               RESP(RESPCODE)
           END-EXEC.

           PERFORM ERROR-CHECK.

       PROGRAM-DONE.
           PERFORM END-PGM.

      *  -----------------------------------------------------------
      *  Update calculations of the means
      *  -----------------------------------------------------------
       AVERAGE-PROC.
           ADD 1 TO WS-INC.
           COMPUTE WS-X-NUM = FUNCTION NUMVAL-C(WS-X-VAL).
           COMPUTE WS-Y-NUM = FUNCTION NUMVAL-C(WS-Y-VAL).

           ADD WS-X-NUM TO WS-X-SUM.
           ADD WS-Y-NUM TO WS-Y-SUM.
      *  -----------------------------------------------------------
      *  Update calculations of the covariance
      *  -----------------------------------------------------------
       VARCOVAR-PROC.
           COMPUTE WS-X-NUM = FUNCTION NUMVAL-C(WS-X-VAL).
           COMPUTE WS-Y-NUM = FUNCTION NUMVAL-C(WS-Y-VAL).

           COMPUTE WS-COVAR = WS-COVAR + (WS-X-NUM - WS-X-AVG)
             * (WS-Y-NUM - WS-Y-AVG).
           COMPUTE WS-X-VAR = WS-X-VAR + (WS-X-NUM - WS-X-AVG) ** 2.
      *  -----------------------------------------------------------
      *  Read the next records from each file
      *  -----------------------------------------------------------
       READ-RECORD.
           COMPUTE WS-ITER = WS-ITER + 1.

           IF WS-ITER > WS-END-NUM
              MOVE 'Y' TO WS-EOF
           ELSE
              MOVE 185 TO RECLEN
              MOVE 5 TO WS-KEY-LEN
              MOVE WS-ITER TO WS-KEY
              EXEC CICS READ
                 FILE (WS-INPUT1)
                 INTO (WS-X-LINE)
                 RIDFLD (WS-KEY)
                 LENGTH (RECLEN)
                 RESP (RESPCODE)
                 RESP2 (RESPCODE2)
              END-EXEC

              PERFORM ERROR-CHECK

              MOVE 185 TO RECLEN
              MOVE 5 TO WS-KEY-LEN
              MOVE WS-ITER TO WS-KEY
              EXEC CICS READ
                 FILE (WS-INPUT2)
                 INTO (WS-Y-LINE)
                 RIDFLD (WS-KEY)
                 LENGTH (RECLEN)
                 RESP (RESPCODE)
                 RESP2 (RESPCODE2)
              END-EXEC

              PERFORM ERROR-CHECK
           END-IF.

      *  -----------------------------------------------------------
      *  Handle an unrecognized response error
      *  -----------------------------------------------------------
       RESP-ERROR.
           MOVE 'EDUC' TO ABENDCODE
           PERFORM ABEND-ROUTINE.
      *  -----------------------------------------------------------
      *  Check response codes for errors
      *  -----------------------------------------------------------
       ERROR-CHECK.
           EVALUATE RESPCODE
             WHEN DFHRESP(NORMAL)
                CONTINUE
             WHEN DFHRESP(NOTFND)
                MOVE 'NOTFND' TO WS-ERROR-MSG
                PERFORM ERROR-SEND
             WHEN DFHRESP(FILENOTFOUND)
                MOVE 'FILENFND' TO WS-ERROR-MSG
                PERFORM ERROR-SEND
             WHEN DFHRESP(INVREQ)
                MOVE 'INVREQ' TO WS-ERROR-MSG
                PERFORM ERROR-SEND
             WHEN OTHER
                PERFORM RESP-ERROR
           END-EVALUATE.
      *  -----------------------------------------------------------
      *  Send an error
      *  -----------------------------------------------------------
       ERROR-SEND.
           EXEC CICS PUT CONTAINER(OUTPUT-CONT)
               FROM(WS-ERROR-MSG)
               CHAR
               RESP(RESPCODE)
           END-EXEC.

           IF RESPCODE NOT = DFHRESP(NORMAL)
             PERFORM RESP-ERROR
           END-IF.

           PERFORM END-PGM.
      *  -----------------------------------------------------------
      *  Abnormal end
      *  -----------------------------------------------------------
       ABEND-ROUTINE.
           EXEC CICS ABEND ABCODE(ABENDCODE) END-EXEC.

      *  -----------------------------------------------------------
      *  Finish
      *  -----------------------------------------------------------
       END-PGM.
           EXEC CICS RETURN END-EXEC.
