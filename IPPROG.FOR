C=======================================================================
C  IPPROG, Subroutine
C
C  This subroutine reads pest observations from the time series file T
C  for the experiment being simulated.
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       2-28-93
C-----------------------------------------------------------------------
C  INPUT  : IPLT,TRTNO,FILET
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called :
C
C  Calls  :
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  A      : The first character plucked off of the header line.
C  C200   : Data read in from the .XXT file.
C  FILET  : Filename containing pest progress data.
C  HEAD   : Temporary variable that reads in headers from *.XXT file.
C  IPLT   : Julian day of planting.
C  NCOL   : number of columns in the .XXT file.
C  PSTHD  : Pest headers read in from pest parameter file.
C  TIERPN : Number of first pest in next tier of *.XXT file
C  TR     : Treatment number read in from .XXT file.
C  TRTNO  : Treatment number of CERES run.
C=======================================================================

      SUBROUTINE IPPROG (YRPLT,TRTNO,FILET)

      IMPLICIT      NONE

      INCLUDE      'PEST.BLK'

C-----------------------------------------------------------------------
C     Variable Declaration
C-----------------------------------------------------------------------

      CHARACTER*1 A
      CHARACTER*6 ERRKEY,HEAD
      CHARACTER*12 FILET
      CHARACTER*200 C200,AMPSAVE

      INTEGER COUNT,COLNUM,ERRNUM
      INTEGER ISECT,I
      INTEGER J,LINEXP,LUN,NCOL, PCOL(6),POBS(6),PCN
      INTEGER TR,TRTNO,TIERPN
      INTEGER YRPLT,YRDOY,TIMDIF,DAPP

      REAL CDATA,REP(6)

      PARAMETER (ERRKEY = 'IPPROG')
C-----------------------------------------------------------------------
C     Initialize variables
C-----------------------------------------------------------------------
      DO 10 J=1,100
        DO 5 I=1,6
          IDAP(I,J) = 0
          YPL(I,J) = 0
    5   CONTINUE
   10 CONTINUE
      DO 15 I=1,6
        PCOL(I) = 0
        POBS(I) = 1
        REP(I) = 1
        PSTHD(I) = '     '
   15 CONTINUE
      TIERPN = 0.0
      COUNT = 1
C-----------------------------------------------------------------------
C     Find header line
C-----------------------------------------------------------------------
      LUN=56
      OPEN(LUN,FILE=FILET,STATUS='OLD',IOSTAT=ERRNUM)
      REWIND(LUN)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILET,0)
40    CONTINUE
         READ(LUN,50,END=130,ERR=100) C200
50       FORMAT(A200)
         READ(C200,80) A
80       FORMAT(A1)
         IF(A.EQ.'@') GOTO 200
         GOTO 40
100   CONTINUE
      WRITE(*,120)
120   FORMAT(' FILE DOES NOT HAVE THE @ CHARACTER IN COL #1')
      STOP
130   CONTINUE
      RETURN
C-----------------------------------------------------------------------
C     Read headers from file T and compare them to pest headers in pest
C     coefficient file. If a header matches a header in the coefficient
C     file, then record the data column number in the PCOL array.
C-----------------------------------------------------------------------
200   CONTINUE
      AMPSAVE = C200
      COLNUM = 0
      TIERPN = COUNT
      NCOL = 0
      DO 300 I=1,30
        COLNUM = COLNUM + 1
        READ(C200,220) HEAD
        IF (HEAD.EQ. '      ') GOTO 400
        DO 210 J=1,NPEST
          IF(HEAD(1:4) .EQ. PID(J)) THEN
             PCOL(COUNT) = COLNUM
             PSTHD(COUNT) = HEAD(1:4)
             COUNT = COUNT + 1
          ELSE IF(HEAD(2:5) .EQ. PID(J)) THEN
             PCOL(COUNT) = COLNUM
             PSTHD(COUNT) = HEAD(2:5)
             COUNT = COUNT + 1
          ELSE IF(HEAD(3:6) .EQ. PID(J)) THEN
             PCOL(COUNT) = COLNUM
             PSTHD(COUNT) = HEAD(3:6)
             COUNT = COUNT + 1
          ENDIF
 210  CONTINUE
      C200 = C200(7:200)
      NCOL = NCOL + 1
 220  FORMAT(A6)
 300  CONTINUE
C-----------------------------------------------------------------------
C     Read in data for each column
C-----------------------------------------------------------------------
400   CONTINUE
500   CONTINUE
      CALL IGNORE2(LUN,LINEXP,ISECT,C200)
      IF(ISECT.EQ.0) GOTO 600
      IF(ISECT.EQ.3) THEN
         IF(C200.NE.AMPSAVE) GOTO 200
         GOTO 500
         ENDIF
C    -------------------------------
C    ISECT=1 Good line found
C    -------------------------------
      READ(C200,510,END=2000) TR,YRDOY
      DAPP = MAX(0,TIMDIF(YRPLT,YRDOY))
 510  FORMAT(1X,I5,1X,I5)
      IF(TR.NE.TRTNO) GOTO 500
      C200 = C200(13:200)
      PCN = TIERPN
      DO 550 I=3,NCOL
        READ(C200,520) CDATA
 520    FORMAT(1X,F5.1)
        C200 = C200(7:200)
        IF(I.EQ.PCOL(PCN) .AND. CDATA .GE. 0.0) THEN
C-----------------------------------------------------------------
C          Check for replicated values and average the values
C          with the previous data if there are replicated values
C-----------------------------------------------------------------
      IF(POBS(PCN)-1 .GT.0 .AND. DAPP .EQ. IDAP(PCN,POBS(PCN)-1)) THEN
            REP(PCN) = REP(PCN) + 1
            POBS(PCN) = POBS(PCN) - 1
            YPL(PCN,POBS(PCN))=
     &        (YPL(PCN,POBS(PCN))*(REP(PCN)-1)+CDATA)/REP(PCN)
C-----------------------------------------------------------------
           ELSE
             YPL(PCN,POBS(PCN)) = CDATA
             IDAP(PCN,POBS(PCN)) = DAPP
             REP(PCN) = 1
           ENDIF
           POBS(PCN) = POBS(PCN) + 1
        ENDIF
        IF(I.EQ.PCOL(PCN)) PCN = PCN + 1
  550 CONTINUE
      GOTO 500
  600 CONTINUE
 2000 CONTINUE
      CLOSE(LUN)
      END


      SUBROUTINE IGNORE2(LUN,LINEXP,ISECT,CHARTEST)
C
C----------------------------------------------------------------------------
C       PURPOSE: To read lines as an 80-character variable and check it
C                for a blank line or for a comment line denoted by ! in col 1.
C       INPUTS:  LUN - Logical unit number of the file to be read
C                LINEXP - Starting line number at which this routine begins to
C                         read the file
C       OUTPUTS: LINEXP - Line number last read by the routine
C                ISECT - Indicator of completion of IGNORE2 routine
C                        0 - End of file encountered
C                        1 - Found a good line to read
C                        2 - End of Section in file encountered, denoted by *
C                            in column 1
C                CHARTEST - 80-character variable containing the contents of
C                           the last line read by the IGNORE2 routine
C----------------------------------------------------------------------------
C
      CHARACTER BLANK*80,CHARTEST*(*)
      INTEGER LUN,LINEXP,ISECT
      DATA BLANK/'                                                    '/
      ISECT = 1
 30   READ(LUN,'(A)',ERR=70,END=70)CHARTEST
      LINEXP = LINEXP + 1
C     CHECK TO SEE IF ALL OF THIS SECTION HAS BEEN READ

      IF(CHARTEST(1:1) .EQ. '*' )THEN
C     INTERMEDIATE HEADER FOUND.  SKIP AND GOTO NEXT DATA
      ISECT = 2
      GOTO 30
C      RETURN
      ENDIF

      IF(CHARTEST(1:1) .EQ.'@') THEN
C     NEXT TIER ENCOUNTERED
      ISECT = 3
      RETURN
      ENDIF
C
C     CHECK FOR BLANK LINES AND COMMENTS (DENOTED BY ! IN COLUMN 1)
      IF(CHARTEST(1:1).NE.'!' .AND. CHARTEST(1:1).NE.'@') THEN
      IF(CHARTEST.NE.BLANK)THEN
C     FOUND A GOOD LINE TO READ
      RETURN
      ENDIF
      ENDIF
      GO TO 30
C       TO READ THE NEXT LINE
 70   ISECT = 0
      END
