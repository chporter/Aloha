C=======================================================================
C  AUTHAR, Subroutine
C
C  Determines when automatic harvest occurs
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                     W.T. Bowen  SEPT 1991
C  2. Header revision and minor changes           P.W.W.      2-7-93
C-----------------------------------------------------------------------
C  INPUT  : DLAYR,DUL,HAREND,HDATE,HDLAY,HLATE,HSTG,IHARI,
C           LL,N,NR0,NR1,NPOD0,NR4,NR5,NDSET,NR7,NR8,
C           SW,SWPLTD,SWPLTH,SWPLTL
C
C  LOCAL  : ANS,AVGSW,CUMSW,DTRY,HEARLY,I,XDEP,XDEPL
C
C  OUTPUT : HAREND
C-----------------------------------------------------------------------
C  Called : PINE
C
C  Calls  : SIMEND
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  : Earliest day after harvest maturity (R8)
C  HEARLY : Computed from HDLAY; sets first day of harvest window
C  HLATE  : Latest day of the year to harvest; last day of window
C  SWPLTL : Lower limits on soil moisture, percent
C  SWPLTL : Upper limits on soil moisture, percent
C  SWPLTD : Depth (cm) to which average soil moisture is determined
C  CUMSW  : Cumulative soil moisture
C  AVGSW  : Average soil moisture percent to depth SWPLTD
C  HAREND : Computed harvest date
C=======================================================================

      SUBROUTINE AUTHAR (DLAYR,DUL,HAREND,HDATE,HDLAY,HLATE,HSTG,IHARI,
     &               LL,SW,SWPLTD,SWPLTH,SWPLTL,YRDOY,MDATE,STGDOY,DAP,
     &               IDETO,NOUTDO)

      IMPLICIT  NONE

      CHARACTER IHARI*1,IDETO*1
      INTEGER   HSTG(3),HAREND,HDATE(3),HDLAY,HEARLY,HLATE,I
      INTEGER   YRDOY,MDATE,STGDOY(20),DAP,NOUTDO

      REAL      AVGSW,CUMSW,DLAYR(20),DTRY,DUL(20),LL(20),SW(20),SWPLTD
      REAL      SWPLTH,SWPLTL,XDEP,XDEPL

      HAREND = - 99

C-----------------------------------------------------------------------
C     Harvest at maturity, MDATE
C-----------------------------------------------------------------------

      IF (IHARI .EQ. 'M') THEN
         HAREND     = MDATE
         ! STGDOY(20) = HAREND
         RETURN

C-----------------------------------------------------------------------
C     Harvest on specified day of year, HDATE
C-----------------------------------------------------------------------

       ELSE IF (IHARI .EQ. 'R') THEN
         IF (YRDOY .GE. HDATE(1)) THEN
            HAREND     = YRDOY
            ! STGDOY(20) = YRDOY
         ENDIF
         RETURN

C-----------------------------------------------------------------------
C     Harvest on specified day after planting, HDATE
C-----------------------------------------------------------------------

       ELSE IF (IHARI .EQ. 'D') THEN
         IF (DAP .GE. HDATE(1)) THEN
            HAREND     = YRDOY
            ! STGDOY(20) = YRDOY
         ENDIF
         RETURN

C-----------------------------------------------------------------------
C     Harvest at specified growth stage, HSTG
C-----------------------------------------------------------------------

       ELSE IF (IHARI .EQ. 'G') THEN
         DO I = 1, 13
            IF (HSTG(1) .EQ. I .AND. YRDOY .EQ. STGDOY(I)) THEN
               HAREND     = YRDOY
               ! STGDOY(20) = YRDOY
               RETURN                                ! Was EXIT, same result
            ENDIF
         END DO
         RETURN

C-----------------------------------------------------------------------
C     Harvest within specified window if SW conditions are met
C-----------------------------------------------------------------------

      ELSE IF (IHARI .EQ. 'A') THEN
         IF (YRDOY .LT. MDATE .OR. MDATE .EQ. -99) THEN
            RETURN
          ELSE IF (YRDOY .EQ. MDATE) THEN
            HEARLY = YRDOY + HDLAY
            !
            ! Added so that harvest window is in correct year when simulation
            ! starts in one year but harvest occurs in following year -- WTB
            !
            IF (HEARLY .GT. HLATE) HLATE = HLATE + 1000

          ELSE IF (YRDOY .GT. MDATE) THEN

C-----------------------------------------------------------------------
C          Check window for automatic harvest, HEARLY < JUL < HLATE
C-----------------------------------------------------------------------

           IF (YRDOY .LT. HEARLY) RETURN
           IF (YRDOY .GT. HLATE) THEN
              WRITE (*,100) MOD(HEARLY,100000),MOD(HLATE,100000)
              IF (IDETO .EQ. 'Y') THEN
                 WRITE(NOUTDO,100) MOD(HEARLY,100000),MOD(HLATE,100000)
              ENDIF

C-----------------------------------------------------------------------
C           Assume harvest to occur on the first day after the defined
C           window to terminate the simulation. This needs to be changed
C           to account for  harvest loss of the crop
C-----------------------------------------------------------------------

            HAREND     = YRDOY
            ! STGDOY(20) = YRDOY
            RETURN
          ENDIF

C-----------------------------------------------------------------------
C         Compute average soil moisture as percent, AVGSW
C-----------------------------------------------------------------------

           I     = 1
           XDEP  = 0.0
           CUMSW = 0.0
  20       IF (XDEP .LT. SWPLTD) THEN
              XDEPL = XDEP
              XDEP  = XDEP + DLAYR(I)
              DTRY  = MIN(DLAYR(I),SWPLTD - XDEPL)
              CUMSW = CUMSW + DTRY *
     &               (MAX(SW(I) - LL(I),0.0)) / (DUL(I) - LL(I))
              I     = I + 1
              GO TO 20
           ENDIF
           AVGSW = (CUMSW / SWPLTD) * 100.0
           IF (AVGSW .GE. SWPLTL .AND. AVGSW .LE. SWPLTH) THEN
              HAREND     = YRDOY
              ! STGDOY(20) = YRDOY
           ENDIF
         ENDIF
         RETURN

C-----------------------------------------------------------------------
C        Error message if an incorrect code has been specified
C-----------------------------------------------------------------------

       ELSE
         IF (IHARI .NE. 'A' .OR. IHARI .NE. 'M') THEN
            IF (IHARI .NE. 'G' .OR. IHARI .NE. 'R') THEN
               WRITE (*,25)
            END IF
         END IF
         STOP
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  25  FORMAT (/3X,'An incorrect code for harvest was written',
     &       /,3X,'into the simulation control file; you will have',
     &       /,3X,'to stop and correct')
 100  FORMAT (/,10X,'Conditions not met during defined window for',
     &        /,10X,'harvesting between DAY ',I6,' and DAY ',I6,/)

      END
