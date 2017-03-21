C=======================================================================
C  OPDONE, Subroutine
C
C  Closes all open files
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P. Wilkens  2-8-93
C  3. Added switch block, etc.                      P. Wilkens  2-8-93
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : CERES
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPDONE

      IMPLICIT  NONE

      INCLUDE  'SWITCH.BLK'

C-----------------------------------------------------------------------
C     Close all files, adding CR/LF
C-----------------------------------------------------------------------

      IF (IDETO .EQ. 'Y') THEN
         WRITE (NOUTDO,100)
         CLOSE (NOUTDO)
      ENDIF
      IF (IDETG .EQ. 'Y') THEN
         WRITE (NOUTDG,100)
         CLOSE (NOUTDG)
      ENDIF
      IF (IDETW .EQ. 'Y') THEN
         WRITE (NOUTDW,100)
         CLOSE (NOUTDW)
      ENDIF
      IF (IDETN .EQ. 'Y') THEN
         WRITE (NOUTDN,100)
         CLOSE (NOUTDN)
      ENDIF
      IF (IDETC .EQ. 'Y') THEN
         WRITE (NOUTDC,100)
         CLOSE (NOUTDC)
      ENDIF
      IF (IDETD .EQ. 'Y') THEN
         WRITE (NOUTDD,100)
         CLOSE (NOUTDD)
      ENDIF
      IF (IDETP .EQ. 'Y') THEN
         WRITE (NOUTDP,100)
         CLOSE (NOUTDP)
      ENDIF
      IF (IDETH .EQ. 'Y') THEN
         WRITE (NOUTDH,100)
         CLOSE (NOUTDH)
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  100 FORMAT (1X)
      END
