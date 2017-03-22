C=======================================================================
C  IPCHEM, Subroutine
C
C  Determines chemical application for a simulation
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       G.P.F       7-13-95
C  2  Accepted and included in DSSAT v3.1           G.H.        4-01-96
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNCHE,CDATE,CHCOD,CHAMT,CHMET,CHDEP,CHT
C           YRSIM,ISWWAT,NCHEM
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
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPCHEM(LUNEXP,FILEX,LNCHE,YRSIM,ISWWAT,NCHEM,CDATE,
     &    CHCOD,CHAMT,CHMET,CHDEP,CHT,ISWCHE,LNSIM,CHEXTR)
C    &    CHCOD,CHAMT,CHMET,CHDEP,CHT,ISWCHE,LNSIM)

      IMPLICIT     NONE

      CHARACTER*1  ISWWAT,ISWCHE
      CHARACTER*5  CHCOD(10),CHMET(10),CHT(10)
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*12 FILEX
      CHARACTER*42 CHEXTR(10)
      CHARACTER*80 CHARTEST

      INTEGER      LNCHE,LUNEXP,ISECT,LINEXP,CDATE(10),NCHEM
      INTEGER      ERRNUM,J,IFIND,LN,YRSIM,LNSIM
      REAL         CHAMT(10),CHDEP(10)

      PARAMETER   (ERRKEY ='IPCHE ')
      FINDCH ='*CHEMI'

      NCHEM = 0

      DO J = 1, 10
         CHCOD(J)  = '     '
         CDATE(J)  = 0
         CHAMT(J)  = 0.0
         CHDEP(J)  = 0.0
      END DO

      IF ( ISWWAT .NE. 'N' .AND. LNCHE .GT. 0) THEN
         IF (ISWCHE .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
            ISWCHE = 'Y'
         ENDIF
         NCHEM = 1
         CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)

         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (LN .NE. LNCHE) GO TO 50
C
C           Read different chemical types and amounts
C
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN,CDATE(NCHEM),
     &            CHCOD(NCHEM),CHAMT(NCHEM),CHMET(NCHEM),
     &            CHDEP(NCHEM),CHT(NCHEM),CHEXTR(NCHEM)
C    &            CHDEP(NCHEM),CHT(NCHEM)

            IF ((CDATE(NCHEM) .LT. 1)  .OR.
     &         (MOD(CDATE(NCHEM),1000) .GT. 366)) THEN
               CALL ERROR (ERRKEY,10,FILEX,LINEXP)
            ENDIF
            IF (CDATE(NCHEM) .LT. YRSIM) GO TO 50
            IF ((CHAMT(NCHEM) .LT. 0.0) .OR.
     &          (CHAMT(NCHEM) .GT. 99999.)) THEN
               CALL ERROR (ERRKEY,11,FILEX,LINEXP)
            ENDIF
            NCHEM = NCHEM + 1
            IF (NCHEM .GE. 11) GO TO 120
          ELSE
            GO TO 120
         ENDIF
         GO TO 50
      ENDIF

 120  REWIND (LUNEXP)
      NCHEM = MAX((NCHEM-1),0)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

C60   FORMAT (I3,I5,1X,A5,1X,F5.0,1X,A5,1X,F5.0,1X,A5)
 60   FORMAT (I3,I5,1X,A5,1X,F5.0,1X,A5,1X,F5.0,1X,A5,A42)
      END

