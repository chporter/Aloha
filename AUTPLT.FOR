C=======================================================================
C  AUTPLT, Subroutine
C
C  Determines automatic planting date
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                           W.T.B. & J.W.J   3-01-91
C  2. Header revision and minor changes P.W.W.           2-07-93
C  3. Modified to plant on last day of window    W.T.B.  June 94
C-----------------------------------------------------------------------
C  INPUT  : JUL,DLAYR,LL,DUL,SW,ST,PWDINF,PWDINL,SWPLTL,
C           SWPLTH,SWPLTD,PTX,PTTN,YRPLT
C
C  LOCAL  : ANS,AVGSW,CUMSW,DTRY,I,TSDEP,XDEP,XDEPL
C
C  OUTPUT : YRPLT
C---------------------------------------------------------------------
C  Called : PINE
C
C  Calls  : SIMEND YR_DOY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  PWDINF : First day of year for planting window
C  PWDINL : Last day of year for planting window
C  PTTN   : Minimum soil temperature limits, centigrade
C  PTX    : Maximum soil temperature limits, centigrade
C  TSDEP  : Computed soil temperature to 10 cm depth
C  SWPLTL : Lower limits on soil moisture, percent
C  SWPLTH : Upper limits on soil moisture, percent
C  SWPLTD : Depth (cm) to which average soil moisture is determined
C  CUMSW  : Cumulative soil moisture
C  AVGSW  : Average soil moisture percent to depth SWPLTD
C  YRPLT  : Computed planting date
C=====================================================================

      SUBROUTINE AUTPLT (DLAYR,LL,DUL,SW,ST,PWDINF,PWDINL,
     &     SWPLTL,SWPLTH,SWPLTD,PTX,PTTN,YRPLT,YEAR,YRDOY,IPLT,STGDOY,
     &     MDATE,IDETO,NOUTDO)

      IMPLICIT  NONE

      CHARACTER IDETO*1
      INTEGER   I,PWDINF,PWDINL,YRPLT,YEAR,YRDOY,IPLT,YR,STGDOY(20)
      INTEGER   MDATE,NOUTDO
      REAL      DLAYR(20),LL(20),DUL(20),SW(20),ST(20),SWPLTL,XDEP,XDEPL
      REAL      SWPLTH,SWPLTD,PTX,PTTN,CUMSW,TSDEP,DTRY,AVGSW
C
C     Initialize soil temperature and soil water variables
C
      TSDEP     = 0.0
      CUMSW     = 0.0
      AVGSW     = 0.0
      YRPLT     = 999999
      STGDOY(9) = -99
C
C     Check window for automatic planting, PWDINF < YRPLT < PWDINL
C
      IF (PWDINF .LT. 1000) THEN
          PWDINF = YEAR*1000 + PWDINF
      ENDIF
      IF (PWDINL .LT. 1000) THEN
          PWDINL = YEAR*1000 + PWDINL
      ENDIF

      IF (YRDOY .LT. PWDINF) RETURN

      IF (YRDOY .GT. PWDINL) THEN
         YRPLT = -99
         MDATE = YRDOY
         WRITE (*,100) MOD(PWDINF,100000),MOD(PWDINL,100000)
         IF (IDETO .EQ. 'Y') THEN
            WRITE (NOUTDO,100) MOD(PWDINF,100000),MOD(PWDINL,100000)
         ENDIF
         RETURN
      ENDIF
C
C     Compute soil temperature, TSDEP, to 10 cm depth
C
      I    = 1
      XDEP = 0.0
  10  IF (XDEP .LT. 10.0) THEN
         XDEP  = XDEP + DLAYR(I)
         TSDEP = ST(I)
         I     = I + 1
         GO TO 10
      ENDIF
C
C     Compute average soil moisture as percent, AVGSW
C
      I     = 1
      XDEP  = 0.0
      CUMSW = 0.0

  20  IF (XDEP .LT. SWPLTD) THEN
         XDEPL = XDEP
         XDEP  = XDEP  + DLAYR(I)
         DTRY  = MIN(DLAYR(I),SWPLTD - XDEPL)
         CUMSW = CUMSW + DTRY *
     &           (MAX(SW(I) - LL(I),0.0)) / (DUL(I) - LL(I))
         I     = I + 1
         GO TO 20
      ENDIF

      AVGSW = (CUMSW / SWPLTD) * 100.0
C
C     Determine YRPLT according to soil temperature and soil moisture
C
      IF (TSDEP .GE. PTTN .AND. TSDEP .LE. PTX) THEN
         IF (AVGSW .GE. SWPLTL .AND. AVGSW .LE. SWPLTH) THEN
            YRPLT      = YRDOY
            STGDOY(9)  = YRPLT
            CALL YR_DOY (YRPLT,YR,IPLT)
         ENDIF
      ENDIF
      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 100  FORMAT (/,10X,'Conditions not met during defined window for',
     &        /,10X,'planting between DAY ',I6,' and DAY ',I6,/)

      END
