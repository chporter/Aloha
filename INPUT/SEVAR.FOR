C=======================================================================
C  SEVAR, Subroutine
C
C  Determines cultivar sensitivity analysis options
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : FILEE,FILEG,NSENS,VARNO,VARTY,VRNAME,IVRGRP,IVRTEM,CLDVAR
C           THVAR,SDPRO,TRIFOL,SIZELF,THRESH,LNGSH,RWIDTH,RHGHT,PATHGE
C           PATHEC,ECOTYP,ECONAM,ECONO,CROP
C
C  LOCAL  : BLANK,ERRKEY,PATHGE,FILEGG,MENU,NLOOP,PATHL,FEXIST
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SENS
C
C  Calls  : ERROR CLEAR IPVAR IPECO IDGEN INVAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SEVAR (FILEE,FILEG,NSENS,RNMODE,VARNO,VARTY,VRNAME,
     &           IVRGRP,PATHGE,PATHEC,ECOTYP,ECONAM,ECONO,CROP)

      IMPLICIT     NONE

      CHARACTER*1  RNMODE,BLANK
      CHARACTER*2  CROP
      CHARACTER*6  VARTY,VARNO,ERRKEY,ECOTYP,ECONO
      CHARACTER*12 FILEE,FILEG
      CHARACTER*16 VRNAME,ECONAM
      CHARACTER*80 PATHGE,PATHEC
      CHARACTER*92 FILEGG

      INTEGER      MENU,NSENS,IVRGRP,NLOOP,PATHL

      LOGICAL      FEXIST

      PARAMETER (ERRKEY = 'SEVAR ')
      PARAMETER (BLANK  = ' ')

      NLOOP = 0
      FEXIST = .TRUE.

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)
      IF (RNMODE .EQ. 'I')  THEN
         CALL CLEAR
         WRITE (*, 200) VARTY,VRNAME,FILEG
         IF (INDEX ('BNPNSBTMPECHPPPRC3C4G0G1G2G3G4G5G6G7G8G9SCVB',
     &                CROP) .GT. 0) THEN
            WRITE (*, 210) ECOTYP,ECONAM
         ENDIF
         IF (.NOT. FEXIST) THEN
            WRITE (*,250) FILEG
          ELSE
            IF (FILEG(8:8) .NE. '0') WRITE (*,260) FILEG
         ENDIF
         WRITE (*,270)
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 0) THEN
          RETURN
      ELSE IF (MENU .EQ. 1) THEN
          CALL IPVAR (FILEG,NSENS,RNMODE,VARNO,
     &         VARTY,VRNAME,PATHGE,ECONO,CROP)
          NSENS = 0
          IF (INDEX ('BNPNSBTMPECHPPPRC3C4G0G1G2G3G4G5G6G7G8G9SCVB',
     &               CROP) .GT. 0) THEN
             CALL IPECO (FILEE,NSENS,RNMODE,PATHEC,ECOTYP,ECONAM,
     &            ECONO,IVRGRP)
          ENDIF
          NSENS = 1
      ELSE IF (MENU .EQ. 2) THEN
          CALL IDGEN (FILEG,RNMODE,PATHGE)
          PATHL  = INDEX(PATHGE,BLANK)
          IF (PATHL .LE. 1) THEN
             FILEGG = FILEG
           ELSE
             FILEGG = PATHGE(1:(PATHL-1)) // FILEG
          ENDIF
          INQUIRE (FILE = FILEGG, EXIST = FEXIST)
          IF (FEXIST) THEN
             NSENS = 0
             CALL IPVAR (FILEG,NSENS,RNMODE,VARNO,
     &            VARTY,VRNAME,PATHGE,ECONO,CROP)
             IF (INDEX ('BNPNSBTMPECHPPPRC3C4G0G1G2G3G4G5G6G7G8G9SCVB',
     &                   CROP) .GT. 0) THEN
                CALL IPECO(FILEE,NSENS,RNMODE,PATHEC,ECOTYP,ECONAM,
     &               ECONO,IVRGRP)
             ENDIF
             NSENS = 1
          ENDIF
      ELSE IF (MENU .EQ. 3) THEN
          IF (INDEX ('BNPNSBTMPECHPPPRC3C4G0G1G2G3G4G5G6G7G8G9VB',
     &                CROP) .GT. 0) THEN
             CALL INVRLE (FILEG,RNMODE,VARTY,VRNAME,PATHGE,ECONO)
           ELSEIF (INDEX ('MZWHMLSGBAPTRISUSCPITRTNCO',CROP) .GT. 0)
     &     THEN
             CALL INVRCE (CROP)
           ELSEIF (INDEX ('CS',CROP) .GT. 0) THEN
             CALL INVRCS
          ENDIF
          PATHL  = INDEX (PATHGE,BLANK)
          IF (PATHL .LE. 1) THEN
             FILEGG = FILEG
           ELSE
             FILEGG = PATHGE(1:(PATHL-1)) // FILEG
          ENDIF
          INQUIRE (FILE = FILEGG, EXIST = FEXIST)
          IF (.NOT. FEXIST) THEN
             GO TO 100
          ENDIF
      ELSE IF (MENU .EQ. 4) THEN
          IF (INDEX ('BNPNSBTMPECHPPPRC3C4G0G1G2G3G4G5G6G7G8G9SCVB',
     &       CROP) .GT. 0) THEN
             CALL IPECO (FILEE,NSENS,RNMODE,PATHEC,ECOTYP,ECONAM,
     &            ECONO,IVRGRP)
          ENDIF
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  200 FORMAT(//,15X,'CULTIVAR SELECTION AND MODIFICATION',/,
     &          15X,'===================================',//,
     & 5X,' 0. Return to Main Menu ',//,
     & 5X,' 1. Cultivar Selection ................[ ',A6,' - ',A16,/,
     & 5X,' 2. Cultivar Parameter File ...........[ ',A12,/,
     & 5X,' 3. Cultivar Parameter Modification ...| ')
  210 FORMAT(
     & 5X,' 4. Ecotype Selection .................[ ',A6,' - ',A16,/)
  250 FORMAT (/,9X,'File ',A12,' does not exist.',
     &        /,9X,'Please select another file.',/)
  260 FORMAT (9X,'The NON-STANDARD Cultivar File ',A12,' is selected.',
     &      /,9X,'Please reselect cultivar [Option 1].')
  270 FORMAT (//,9X,'SELECTION ? [ Default = 0 ] ===> ',$)

      END

C=======================================================================
C  IDGEN, Subroutine
C
C  Determines selection of appropriate genetics parameter file
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : FILEG,RNMODE,PATHGE
C
C  LOCAL  : LINE,BLANK,GENTEM,FILEGG,I,PATHL,FEXIST,CPID,FLAG
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEVAR
C
C  Calls  : CLEAR VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IDGEN (FILEG,RNMODE,PATHGE)

      IMPLICIT     NONE

      CHARACTER*1  LINE(80),RNMODE,BLANK
      CHARACTER*12 FILEG,GENTEM
      CHARACTER*80 PATHGE
      CHARACTER*92 FILEGG

      INTEGER      I,PATHL,CUL

      LOGICAL*4    FEXIST

      REAL         CPID,FLAG

      PARAMETER (BLANK = ' ')

      GENTEM = FILEG
      PATHL  = INDEX (PATHGE,BLANK)

      IF (RNMODE .EQ. 'I') THEN
         CALL CLEAR
         WRITE (*,100)
      ENDIF

      DO I = 1, 10
         WRITE (GENTEM(8:8),'(I1)') I-1
         IF (PATHL .LE. 1) THEN
            FILEGG = GENTEM
          ELSE
            FILEGG = PATHGE (1:(PATHL-1)) // GENTEM
         ENDIF
         INQUIRE (FILE = FILEGG, EXIST = FEXIST)
         IF (FEXIST) THEN
            IF (RNMODE .EQ. 'I') WRITE (*,30) I,GENTEM
         ENDIF
      END DO

 15   CONTINUE
      LINE(1) = ' '
      READ(FILEG(8:8),'(I1)') CUL
      IF (RNMODE .EQ. 'I') WRITE (*,50) CUL+1
      READ (5,'(80A1)') LINE
      CALL VERIFY (LINE,CPID,FLAG)
      IF (CPID .LE. 0.0) THEN
         RETURN
       ELSE IF (FLAG .GT. 0.0 .OR. CPID .GT. 10.0) THEN
         WRITE (*,70)
         GO TO 15
       ELSE IF (CPID .GT. 0 .AND. CPID .LE. 10.0) THEN
         WRITE (FILEG(8:8),'(I1)') (NINT(CPID)-1)
         IF (PATHL .LE. 1) THEN
            FILEGG = FILEG
          ELSE
            FILEGG = PATHGE (1:(PATHL-1)) // FILEG
         ENDIF
         INQUIRE (FILE = FILEGG, EXIST = FEXIST)
         IF (.NOT. FEXIST) THEN
            WRITE (*,80)
            GO TO 15
         ENDIF
       ELSE
         WRITE (*,70)
         GO TO 15
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

   30 FORMAT (2X,I2,')',5X,A12)
   50 FORMAT (/,10X,'GENETICS PARAMETER FILE SELECTED    ===>',1X,I2,
     &        /,10X,'ALTERNATE SELECTION ? [Default = 0] ---> ',$)
   70 FORMAT (  10X,'Please enter a number between 0 and 9 ! ')
   80 FORMAT (  10X,'You selected a file which does not exist.',/,
     &          10X,'Please select another file !')
  100 FORMAT (  10X,'GENETIC PARAMETER FILES AVAILABLE',/,10X,33('-'))

      END
