C=======================================================================
C  IPPARM, Subroutine
C
C  This subroutine reads the pest damage coefficient datafile
C  passed in by the FILEP variable string.  The pest damage coefficient
C  datafile is created by the DSSAT shell and provided with the crop models
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       W.D.B.      7-24-91
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       2-28-93
C-----------------------------------------------------------------------
C  INPUT  : FILEP
C
C  LOCAL  : LUN,LIEXP,PNAME,LN
C
C  OUTPUT : PID,PCTID,PCPID,PDCF1,PDCF2,NPEST
C-----------------------------------------------------------------------
C  Called : CERES
C
C  Calls  : ERROR IGNORE
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  FILEP  : Name of the file containing pest damage coefficient data
C  PID    : Pest identifier header for PESTIN2.XX
C  PCTID  : Pest coupling point identification
C  PCPID  : Pest curve type identification
C  PDCF1  : First pest damage coef. multiplier of pest curve
C  PDCF2  : Second pest damage coef.
C  NPEST  : Number of pest damage rows in PESTIN1.XX
C  LUN    : Logical unit number
C  LIEXP  : Last line number read by subroutine IGNORE.FOR
C  PNAME  : Pest name
C  LN     : Line number in pest input file #1
C=======================================================================

      SUBROUTINE IPPARM (FILEP,PATHPE)

      IMPLICIT      NONE

      INCLUDE      'PEST.BLK'

      CHARACTER*1   BLANK
      CHARACTER*2   NEWLIN
      CHARACTER*6   ERRKEY
      CHARACTER*12  FILEP
      CHARACTER*80  PATHPE
      CHARACTER*92  FILEPP
      CHARACTER*200 C200

      INTEGER ERRNUM,PATHL
      INTEGER LN,LINEXP,ISECT
      INTEGER LUN,I,J,CPN

      PARAMETER (ERRKEY = 'IPPARM')
      PARAMETER (BLANK  = ' ')
C----------------------------------------------------------------------
C     Initialize variables
C----------------------------------------------------------------------
      NPEST = 0
      LUN = 38
      LINEXP = 0
      LN=0
      DO 10, I=1,40
      PCTID(I) = 0
 10   CONTINUE
      DO 20 I=1,40
      DO 30 J=1,6
      PCPID(I,J) = 'xxxxx'
      PDCF1(I,J) = 0.0
 30   CONTINUE
 20   CONTINUE
C----------------------------------------------------------------------
C     Open pest coefficient file
C----------------------------------------------------------------------
      PATHL  = INDEX(PATHPE,BLANK)
      IF (PATHL .LE. 1) THEN
        FILEPP = FILEP
      ELSE
        FILEPP = PATHPE(1:(PATHL-1)) // FILEP
      ENDIF
      OPEN (LUN,FILE = FILEPP, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEP,0)
C----------------------------------------------------------------------
C     Read each line of coefficient file, checking for comments
C     or blank lines using the subroutine IGNORE.
C----------------------------------------------------------------------
 100  CONTINUE
      CALL IGNORE(LUN,LINEXP,ISECT,C200)
      IF (ISECT .EQ. 0 ) GO TO 200
      IF (ISECT .EQ. 2 ) GO TO 100
      IF(ISECT.EQ.1) THEN
        READ(C200,40,ERR=150) NEWLIN
40      FORMAT(A2)
C     --------------------------------------------
C      Read in first line of data for new pest
C     --------------------------------------------
      IF (NEWLIN .NE.'  ') THEN
             READ(C200,50,ERR=150) LN,PID(LN),PNAME(LN),PCTID(LN),
     &                      PCPID(LN,1),PDCF1(LN,1)
             NPEST = LN
             CPN = 1
             ENDIF
C     --------------------------------------------
C     Read in subsequent coupling point data
C     --------------------------------------------
      IF (NEWLIN .EQ.'  ') THEN
          CPN = CPN + 1
          READ(C200,60,ERR=150) PCPID(LN,CPN),PDCF1(LN,CPN)
          ENDIF
 50   FORMAT(I2,1X,A5,1X,A20,1X,I1,1X,A5,1X,F10.5)
 60   FORMAT(32X,A5,1X,F10.5)
      ENDIF
      GO TO 100
C----------------------------------------------------------------------
C     Completed reading pest coefficient file
C----------------------------------------------------------------------
150   CONTINUE
      WRITE(*,160)
160   FORMAT(' ERRED WHEN TRYING TO READ 6 REPLICATES')
170   CONTINUE
      CLOSE(LUN)
      RETURN
C----------------------------------------------------------------------
C     End of file encountered, break out of reading loop
C----------------------------------------------------------------------
200   CONTINUE
      CLOSE(LUN)
      RETURN
      END
