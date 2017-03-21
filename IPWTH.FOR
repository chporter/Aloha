C=======================================================================
C  IPWTH, Subroutine, N.B. Pickering, 08/30/91
C  Input weather data and check for errors.
C-----------------------------------------------------------------------
C  Input : FILEW,N,CCO2
C  Output: SRAD,TMAX,TMIN,RAIN,PAR,XLAT,XLONG,YR
C  Local :
C  Ifile :
C  Sub/Fn: DOC
C=======================================================================

      SUBROUTINE IPWTH(YRDOY,FILEW,YRSIM,LINWTH,XELEV,PAR,TAMP,TAV,
     &  TDEW,TMAX,TMIN,RAIN,REFHT,SRAD,WINDSP,XLAT,XLONG,WINDHT,CCO2,
     &  PATHWT,RHUM)

      IMPLICIT NONE

      CHARACTER ERRKEY*6,FILEW*12,HDRFMT*80,INSI*4,LINE*80,RECFMT*80
      CHARACTER*1  BLANK
      CHARACTER*80 PATHWT
      CHARACTER*92 FILEWW

      INTEGER
     &  ERRNUM,YRSIM,LINWTH,LUNWTH,TIMDIF,YRDOY,YRDOYW,YRDOYY,WYEAR
      INTEGER FOUND,PATHL,YRSIMM

      REAL
     &  XELEV,PAR,RAIN,REFHT,SRAD,TAV,TAMP,TDEW,TMAX,TMIN,WINDHT,
     &  WINDSP,XLAT,XLONG,CCO2,RHUM

      PARAMETER
     &  ( HDRFMT = '(2X,A4,2(1X,F8.3),1X,F5.0,4(1X,F5.1),1X,F5.0)',
     &    RECFMT = '(I5,8(1X,F5.0))',
     &    ERRKEY = 'IPWTH ' , LUNWTH = 11 )
       PARAMETER (BLANK = ' ')

C     First simulation day.

      YRSIMM = MOD(YRSIM,100000)
      IF (YRDOY .EQ. YRSIM) THEN

        LINWTH = 1
        PATHL  = INDEX(PATHWT,BLANK)
        IF (PATHL .LE. 1) THEN
          FILEWW = FILEW
        ELSE
          FILEWW = PATHWT(1:(PATHL-1)) // FILEW
        ENDIF
        OPEN (LUNWTH,FILE=FILEWW,STATUS='OLD',IOSTAT=ERRNUM)
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LINWTH)

C       Read in weather file header.

   20   CONTINUE
        CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
        IF (FOUND .EQ. 2) GO TO 20
        IF (FOUND .EQ. 0) CALL ERROR(ERRKEY,-1,FILEW,LINWTH)
        READ (LINE,HDRFMT,IOSTAT=ERRNUM)
     &    INSI,XLAT,XLONG,XELEV,TAV,TAMP,REFHT,WINDHT,CCO2
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LINWTH-1)

C       Substitute default values if REFHT or WINDHT are missing.

        IF (REFHT .LE. 0.) REFHT = 1.5
        IF (WINDHT .LE. 0.) WINDHT = 2.0


C       Read in first weather record.

        CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
        IF (FOUND .NE. 1) CALL ERROR(ERRKEY,-1,FILEW,LINWTH)
        READ (LINE,RECFMT,IOSTAT=ERRNUM) YRDOYW
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LINWTH-1)
        IF (YRDOYW .GT. YRSIMM) CALL ERROR(ERRKEY,1,FILEW,LINWTH-1)

        IF (YRDOYW .NE. YRSIMM-1) BACKSPACE(LUNWTH)

C       Read in weather records until day before simulation.

   10   IF (YRDOYW .LT. YRSIMM-1) THEN
          CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
          IF (FOUND .NE. 1) CALL ERROR(ERRKEY,-1,FILEW,LINWTH)
          READ (LINE,RECFMT,IOSTAT=ERRNUM) YRDOYW
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LINWTH-1)
        GOTO 10
        ENDIF

      ENDIF

C     Remember yesterday's date to check for sequential data.

      YRDOYY = YRDOY

C     Read in today's data and check.

      CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
      IF (FOUND .EQ. 0) THEN
  100    CLOSE(LUNWTH)
         READ(FILEW(5:6),200) WYEAR
  200    FORMAT(I2.2)
         WYEAR = WYEAR + 1
         WRITE(FILEW(5:6),200) MOD(WYEAR,100)
         LINWTH = 1
         PATHL  = INDEX(PATHWT,BLANK)
         IF (PATHL .LE. 1) THEN
            FILEWW = FILEW
          ELSE
            FILEWW = PATHWT(1:(PATHL-1)) // FILEW
          ENDIF

          OPEN (LUNWTH,FILE=FILEWW,STATUS='OLD',IOSTAT=ERRNUM)
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LINWTH)
C         Read in weather file header.
  300     CONTINUE
          CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
          IF (FOUND .EQ. 2) GO TO 300
          IF (FOUND .EQ. 0) CALL ERROR(ERRKEY,-1,FILEW,LINWTH)
C         Read in first weather record.
          CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
          IF (FOUND .NE. 1) CALL ERROR(ERRKEY,-1,FILEW,LINWTH)
      ENDIF
      READ (LINE,RECFMT,IOSTAT=ERRNUM) YRDOYW,SRAD,TMAX,TMIN,
     &  RAIN,TDEW,WINDSP,PAR,RHUM
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LINWTH-1)
C-----------------------------------------------------------------------
C     Error checking
C-----------------------------------------------------------------------
      IF (TIMDIF(YRDOYY,YRDOYW) .GT. 1)
     &  CALL ERROR(ERRKEY,2,FILEW,LINWTH-1)
      IF (SRAD .LE. 0. .OR. RAIN .LT. 0.)
     &  CALL ERROR(ERRKEY,3,FILEW,LINWTH-1)
      IF (NINT(TMAX) .EQ. 0 .AND. NINT(TMIN) .EQ. 0)
     &  CALL ERROR(ERRKEY,4,FILEW,LINWTH-1)
      IF (TMAX .EQ. TMIN)
     &  CALL ERROR(ERRKEY,5,FILEW,LINWTH-1)
      IF (TMAX .LT. TMIN)
     &  CALL ERROR(ERRKEY,6,FILEW,LINWTH-1)

C-----------------------------------------------------------------------
C     Substitute default values if TDEW or WINDSP are missing.
C-----------------------------------------------------------------------
      IF (TDEW .LE. 0.0)  TDEW = TMIN + 2.0
      IF (WINDSP .LE. 0.0) WINDSP = 86.4
      RETURN
      END
