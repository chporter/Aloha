C=======================================================================
C  IPTILL, Subroutine
C
C  Determines tillage operations for a simulation
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       G.P.F       7-18-95
C  2. Modified and included in DSSAT v3.1           G.H.        4-01-96
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNCHE,CDATE,CHCOD,CHAMT,CHMET,CHDEP,CHT
C           YRSIM,ISWWAT,NCHEM,FOLFR,SOLFR
C
C  LOCAL  : ERRKEY,CHARTEST,ISECT,LINEXP,ERRNUM,J,IFIND,LN
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : FIND IGNORE ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C=======================================================================

      SUBROUTINE IPTILL (LUNEXP,FILEX,LNTIL,YRSIM,ISWTIL,NTIL,TDATE,
     &    TIMPL,TDEP,LNSIM)

      IMPLICIT     NONE

      CHARACTER*5  TIMPL(10)
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*12 FILEX
      CHARACTER*1  ISWTIL
      CHARACTER*80 CHARTEST

      INTEGER      LNTIL,LUNEXP,ISECT,LINEXP,TDATE(10),NTIL
      INTEGER      ERRNUM,J,IFIND,LN,YRSIM,LNSIM
      REAL         TDEP(10)

      PARAMETER   (ERRKEY ='IPTILL')
      FINDCH ='*TILLA'

      DO J = 1, 10
         TIMPL(J)  = '     '
         TDATE(J)  = 0
         TDEP(J) = 0.0
      END DO

      NTIL = 0
      IF (LNTIL .GT. 0) THEN
         IF (ISWTIL .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
            ISWTIL = 'Y'
         ENDIF
         NTIL = 1
         CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)

         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (LN .NE. LNTIL) GO TO 50
C
C           Read tillage operations
C
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN,TDATE(NTIL),
     &            TIMPL(NTIL),TDEP(NTIL)
            IF ((TDATE(NTIL) .LT. 1)  .OR.
     &         (MOD(TDATE(NTIL),1000) .GT. 366)) THEN
               CALL ERROR (ERRKEY,10,FILEX,LINEXP)
            ENDIF
            IF (TDATE(NTIL) .LT. YRSIM) GO TO 50
            IF (TDEP(NTIL) .LT. 0) CALL ERROR (ERRKEY,11,FILEX,LINEXP)
            NTIL = NTIL + 1
            IF (NTIL .GE. 11) GO TO 120
          ELSE
            GO TO 120
         ENDIF
         GO TO 50
      ENDIF

 120  REWIND (LUNEXP)
      NTIL = MAX((NTIL-1),0)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,I5,1X,A5,1X,F5.0)

      END

