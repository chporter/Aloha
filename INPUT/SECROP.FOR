C=======================================================================
C  SECROP, Subroutine
C
C  Determines crop sensitivity analysis options
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : FILEG,IDETA,CROP,CROPD,PATHGE
C
C  LOCAL  : IDETA,BLANK,CROPC,ERRKEY,MENU,NLOOP
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SENS
C
C  Calls  : ERROR CLEAR SWCROP IDCROP
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SECROP (FILEC,FILEE,FILEG,RNMODE,CROP,CROPD,PATHCR)

      IMPLICIT NONE

      CHARACTER*1  RNMODE
      CHARACTER*2  CROP,CROPC
      CHARACTER*6  ERRKEY
      CHARACTER*10 CROPD
      CHARACTER*12 FILEC,FILEE,FILEG
      CHARACTER*80 PATHCR

      INTEGER      MENU,NLOOP

      PARAMETER (ERRKEY = 'SECROP')

      CROPC = CROP
      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)
      IF (RNMODE .EQ. 'I')  THEN
         CALL CLEAR
         WRITE (*,200) CROPD,FILEC
         IF (FILEC(8:8) .NE. '0') WRITE (*,250) FILEC
         IF (CROPC .NE. CROP) WRITE (*,260) CROPD
         WRITE (*,275)
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
          CALL SWCROP (FILEC,FILEE,FILEG,RNMODE,CROP,CROPD,PATHCR)
      ELSE IF (MENU .EQ. 2) THEN
          CALL IDCROP (FILEC,RNMODE,PATHCR)
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  200 FORMAT (//,
     &       15X,'CROP SELECTION AND MODIFICATION',/,
     &       15X,'===============================',//,
     & 5X,' 0. Return to Main Menu ',//,
     & 5X,' 1. Crop Selection ....................[ ',A10,/,
     & 5X,' 2. Crop Parameter File ...............[ ',A12,/)
  250 FORMAT (9X,
     & 'A NON-STANDARD crop parameter file is selected [',A12,']',/,9X,
     & 'Simulated results can potentially be different.')
  260 FORMAT (9X,
     & 'A new crop ',A10,' is selected !!',/,9X,
     & 'Please reselect cultivar and other management options ',
     & 'in the main menu.')
  275 FORMAT (//,9X,'SELECTION ? [ Default=0 ] ===> ',$)

      END

C=======================================================================
C  SWCROP, Subroutine
C
C  Determines crop sensitivity analysis options
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : FILEG,IDETA,CROP,CROPD,PATHGE
C
C  LOCAL  : LINE,BLANK,CROPID,ERRKEY,CROPS,CRPTEM,FILECC,I,NLOOP,PATHL,
C           FEXIST,CID,FLAG
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SECROP
C
C  Calls  : CLEAR ERROR VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SWCROP (FILEC,FILEE,FILEG,RNMODE,CROP,CROPD,PATHCR)

      IMPLICIT NONE

      CHARACTER*1  RNMODE,LINE(80),BLANK
      CHARACTER*2  CROP,CROPID(38)
      CHARACTER*6  ERRKEY
      CHARACTER*10 CROPD,CROPS(38)
      CHARACTER*12 FILEC,CRPTEM,FILEE,FILEG
      CHARACTER*80 PATHCR
      CHARACTER*92 FILECC

      INTEGER      I,NLOOP,PATHL
      LOGICAL      FEXIST
      REAL         CID,FLAG

      PARAMETER (ERRKEY = 'SWCROP')
      PARAMETER (BLANK  = ' ')

      DATA CROPID/'BN','PN','SB','CP','PE','CH','PP','PR','RI','FA',
     &            'MZ','WH','ML','SG','BA','CS','PT','TM','C3','C4',
     &            'G0','G1','G2','G3','G4','G5','G6','G7','G8','G9',
     &            'SC','AR','SU','PI','TR','TN','CO','VB'/
      DATA CROPS /'DRY BEAN  ','PEANUT    ','SOYBEAN   ','COWPEA    ',
     &            'PEA       ','CHICKPEA  ','PIGEONPEA ','PEPPER    ',
     &            'RICE      ','FALLOW    ','MAIZE     ','WHEAT     ',
     &            'MILLET    ','SORGHUM   ','BARLEY    ','CASSAVA   ',
     &            'POTATO    ','TOMATO    ','C3-CROPS  ','C4-CROPS  ',
     &            'BAHIA     ','GRASS-1   ','GRASS-2   ','GRASS-3   ',
     &            'GRASS-4   ','GRASS-5   ','GRASS-6   ','GRASS-7   ',
     &            'GRASS-8   ','GRASS-9   ','SUGARCANE ','AROIDS    ',
     &            'SUNFLOWER ','PINEAPPLE ','TARO      ','TANIER    ',
     &            'COTTON    ','VELVETBEAN'/

      CRPTEM = FILEC
      PATHL  = INDEX (PATHCR,BLANK)
      WRITE (CRPTEM(8:8),'(A1)') '0'

      NLOOP  = 0

      IF (RNMODE .EQ. 'I') THEN
         CALL CLEAR
         WRITE (*,400)
         DO I = 1, 35
            WRITE (CRPTEM(1:2),600) CROPID(I)
            IF (PATHL .LE. 1) THEN
               FILECC = CRPTEM
             ELSE
               FILECC = PATHCR(1:(PATHL-1)) // CRPTEM
            ENDIF
            INQUIRE (FILE = FILECC, EXIST = FEXIST)
            IF (FEXIST) THEN
               WRITE (*,700) I,CROPS(I)
            ENDIF
         END DO
      ENDIF

 1020 CONTINUE

      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,FILEC,0)
      LINE(1) = ' '
      IF (RNMODE .EQ. 'I') WRITE (*,900)
      READ (5,1000) LINE
      CALL VERIFY (LINE,CID,FLAG)
      IF (CID .LE. 0.0) THEN
         IF (PATHL .LE. 1) THEN
            FILECC = FILEC
          ELSE
            FILECC = PATHCR(1:(PATHL-1)) // FILEC
         ENDIF
         INQUIRE (FILE = FILECC,EXIST = FEXIST)
         IF (.NOT. FEXIST) THEN
            WRITE (*,1040)
            GO TO 1020
         ENDIF
       ELSE
         IF (FLAG .GT. 0 .OR. CID .GT. 35) THEN
            WRITE (*,1010)
            GO TO 1020
          ELSE
            WRITE (CRPTEM(1:2),'(A2)') CROPID(NINT(CID))
            IF (PATHL .LE. 1) THEN
               FILECC = CRPTEM
             ELSE
               FILECC = PATHCR(1:(PATHL-1)) // CRPTEM
            ENDIF
            INQUIRE (FILE = FILECC,EXIST = FEXIST)
            IF (.NOT. FEXIST) THEN
               WRITE (*,1040)
               GO TO 1020
            ENDIF
         ENDIF

         CROP  = CROPID (NINT(CID))
         CROPD = CROPS  (NINT(CID))
         IF (FILEC .NE. CRPTEM) THEN
            WRITE (FILEG(1:2),600) CROP
            WRITE (FILEE(1:2),600) CROP
         ENDIF
         FILEC = CRPTEM
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  400 FORMAT (/,30X,'DSSAT V3.5 CROPS',/,30X,16('='))
  600 FORMAT (A2)
  700 FORMAT (30X,I2,')',5X,A10)
  900 FORMAT (/,30X,'SELECT CROP (#) ====> ',$)
 1000 FORMAT (80A1)
 1010 FORMAT (10X,'Please enter a number between 1 and 20 ! ')
 1040 FORMAT (/,10X,'You selected a crop which does not exist.',/,
     &          10X,'Please select another crop !')

      END

C=======================================================================
C  IDCROP, Subroutine
C
C  Determines selection of appropriate crop parameter file
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : FILEC,PATHCR
C
C  LOCAL  : LINE,BLANK,CROPFL,FILECC,I,PATHL,FEXIST,CPID,FLAG
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SECROP
C
C  Calls  : CLEAR VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IDCROP (FILEC,RNMODE,PATHCR)

      IMPLICIT     NONE

      CHARACTER*1  LINE(80),RNMODE,BLANK
      CHARACTER*12 FILEC,CROPFL
      CHARACTER*80 PATHCR
      CHARACTER*92 FILECC

      INTEGER      I,PATHL
      LOGICAL      FEXIST
      REAL         CPID,FLAG

      PARAMETER (BLANK = ' ')

      CROPFL = FILEC
      PATHL  = INDEX (PATHCR,BLANK)

      IF (RNMODE .EQ. 'I') THEN
         CALL CLEAR
         WRITE (*,100)
      ENDIF

      DO I = 1, 10
         WRITE (CROPFL(8:8),'(I1)') I-1
         IF (PATHL .LE. 1) THEN
            FILECC = CROPFL
          ELSE
            FILECC = PATHCR(1:(PATHL-1)) // CROPFL
         ENDIF
         INQUIRE (FILE = FILECC, EXIST = FEXIST)
         IF (FEXIST) THEN
            IF (RNMODE .EQ. 'I') WRITE (*,30) I-1,CROPFL
         ENDIF
      END DO

 15   CONTINUE
      LINE(1) = ' '
      IF (RNMODE .EQ. 'I') WRITE (*,50) FILEC(8:8)

      READ (5,'(80A1)') LINE
      CALL VERIFY (LINE,CPID,FLAG)

      IF (CPID .LE. 0) THEN
          FILECC = FILEC
       ELSE IF (FLAG .GT. 0 .OR. CPID .GT. 20.0) THEN
         WRITE (*,70)
         GO TO 15
       ELSE
         WRITE (FILEC(8:8),'(I1)') NINT(CPID)
         IF (PATHL .LE. 1) THEN
            FILECC = FILEC
          ELSE
            FILECC = PATHCR (1:(PATHL-1)) // FILEC
         ENDIF
         INQUIRE (FILE = FILECC,EXIST = FEXIST)
         IF (.NOT. FEXIST) THEN
            WRITE (*,80)
            GO TO 15
         ENDIF
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

   30 FORMAT (2X,I2,')',5X,A12)
   50 FORMAT (/,10X,'CROP PARAMETER FILE SELECTED        ===>',1X,A1,
     &        /,10X,'ALTERNATE SELECTION ? [Default = 0] ---> ',$)
   70 FORMAT (10X,'Please enter a number between 0 and 20! ')
   80 FORMAT (10X,'You selected a file which does not exist.',/,
     &        10X,'Please select another file !')
  100 FORMAT (10X,'CROP PARAMETER FILES AVAILABLE',/,10X,30('-'))

      END
