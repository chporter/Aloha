C=======================================================================
C  SESIM, Subroutine
C
C  Determines simulation control of sensitivity analysis options
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       05-28-93
C  4. Check on MEPHO and MEEVP combination          NBP         02-05-94
C  5. Add hydrology method                          GH          04-02-96
C-----------------------------------------------------------------------
C  INPUT  : ISWNIT,ISWWAT,NFMANT,ISWSYM,ISWPHO,ISWDIS,MEWTH,MESIC,
C           MEEVP,MEPHO,FILEW,YEAR,PATHWT
C
C  LOCAL  : ERRKEY,WTMANT,PHMANT,INMANT,ETMANT,MENU,NLOOP
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SENS
C
C  Calls  : ERROR CLEAR SWFIX SWWTH SWINSC SWPHOT SWEVAP
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SESIM (RNMODE,ISWNIT,ISWWAT,NFMANT,ISWSYM,ISWPHO,
     &           ISWDIS,MEWTH,MESIC,MEEVP,MEPHO,FILEW,YEAR,PATHWT,
     &           NSWITCH,CROP,MEHYD,MESOM)

      IMPLICIT     NONE

      CHARACTER*1  RNMODE,ISWWAT,ISWNIT,ISWSYM,ISWPHO
      CHARACTER*1  ISWDIS,MEWTH,MESIC,MEEVP,MEPHO,MEHYD,MESOM
      CHARACTER*2  CROP
      CHARACTER*6  ERRKEY
      CHARACTER*12 FILEW
      CHARACTER*15 NFMANT,WTMANT,PHMANT,INMANT
C-PW  CHARACTER*16 ETMANT
C Add OMMANT
C-PW
      CHARACTER*16 ETMANT,OMMANT
      CHARACTER*17 HYMANT
      CHARACTER*80 PATHWT

      INTEGER      MENU,NLOOP,YEAR,NSWITCH

      PARAMETER (ERRKEY = 'SESIM ')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (RNMODE .EQ. 'I')  THEN
         CALL CLEAR
         IF (ISWWAT .EQ. 'Y') THEN
            IF (MEHYD .EQ. 'R') THEN
              HYMANT = 'RITCHIE HYDROLOGY'
            ELSE IF (MEHYD .EQ. 'A') THEN
              HYMANT = 'ADAPT HYDROLOGY  '
            ENDIF
         ELSE
            HYMANT   = 'NO WATER BALANCE '
         ENDIF
         IF (ISWNIT .EQ. 'Y') THEN
            IF (ISWSYM .EQ. 'Y') THEN
                NFMANT = 'N-FIX SIMULAT. '
            ELSE IF (ISWSYM .EQ. 'N') THEN
                NFMANT = 'NO N-FIX SIMUL.'
            ELSE IF (ISWSYM .EQ. 'U') THEN
                NFMANT = 'UNLIMITED N-FIX'
            ELSE
                NFMANT = 'N-FIX NON LIMIT'
            ENDIF
          ELSE
            NFMANT = '               '
         ENDIF

         IF (MEWTH .EQ. 'M') THEN
            WTMANT = 'OBSERVED DATA  '
          ELSE IF (MEWTH .EQ. 'G') THEN
            WTMANT = 'SIMULATED DATA '
          ELSE IF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W') THEN
            WTMANT = 'INTERNALLY SIM.'
         ENDIF

         IF (MEPHO .EQ. 'C') THEN
            PHMANT = 'CANOPY PHOT    '
          ELSE IF (MEPHO .EQ. 'L') THEN
            PHMANT = 'LEAF LEVEL PHOT'
         ENDIF

         IF (MESIC .EQ. 'M') THEN
            INMANT = 'AS REPORTED    '
          ELSE IF (MESIC .EQ. 'S') THEN
            INMANT = 'PREVIOUS RUN   '
         ENDIF

         IF (MEEVP .EQ. 'P') THEN
            ETMANT = 'FAO-PENMAN      '
         ELSE IF (MEEVP .EQ. 'R') THEN
            ETMANT = 'PRIESTLEY-TAYLOR'
         ELSE IF (MEEVP .EQ. 'Z') THEN
            ETMANT = 'ZONAL ENERGY BAL'
         ENDIF
C-PW     Add MESOM strings
         IF (MESOM .EQ. 'G') THEN
            OMMANT = 'Godwin         '
         ELSE IF (MESOM .EQ. 'P') THEN
            OMMANT = 'Parton         '
         ELSE 
            OMMANT = 'Other          '
         ENDIF
         WRITE (*,200) HYMANT,ISWNIT,NFMANT,ISWPHO,ISWDIS,
     &                 WTMANT,INMANT,PHMANT,ETMANT,OMMANT
         IF (ISWWAT .EQ. 'N') THEN
            IF (ISWNIT .EQ. 'Y') WRITE (*,280)
         ENDIF
         IF (MEEVP .EQ. 'Z' .AND. MEPHO .NE. 'L') WRITE(*,285)
         WRITE (*,290)
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
          CALL SWWATB (RNMODE,MEHYD,ISWWAT,ISWNIT,NSWITCH)
      ELSE IF (MENU .EQ. 2) THEN
          IF (ISWNIT .EQ. 'Y') THEN
             ISWNIT  = 'N'
             NSWITCH = 0
           ELSE
             ISWNIT  = 'Y'
             NSWITCH = 1
             ISWWAT  = 'Y'
          ENDIF
          IF (CROP .EQ. 'RI') THEN
             CALL SENIT (RNMODE,ISWNIT,NSWITCH)
          ENDIF
      ELSE IF (MENU .EQ. 3) THEN
          CALL SWFIX (RNMODE,ISWNIT,ISWWAT,ISWSYM,NFMANT)
      ELSE IF (MENU .EQ. 4) THEN
          WRITE (*,400)
          PAUSE
      ELSE IF (MENU .EQ. 5) THEN
          IF (ISWDIS .EQ. 'Y') THEN
             ISWDIS = 'N'
           ELSE
             ISWDIS = 'Y'
          ENDIF
      ELSE IF (MENU .EQ. 6) THEN
          CALL SWWTH (RNMODE,MEWTH,PATHWT)
          IF (MEWTH .EQ. 'M') THEN
             IF (FILEW(11:11) .EQ. ' ') THEN
                WRITE (FILEW(5:12),170) YEAR,'01.WTH'
              ELSE
                WRITE (FILEW(12:12),'(A1)') 'H'
             ENDIF
             RETURN
           ELSEIF (MEWTH .EQ. 'G') THEN
             IF (FILEW(11:11) .EQ. ' ') THEN
                WRITE (FILEW(5:12),170) YEAR,'01.WTG'
              ELSE
                WRITE (FILEW(12:12),'(A1)') 'G'
             ENDIF
             RETURN
           ELSEIF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W') THEN
             WRITE (FILEW(5:12),'(A8)') '.CLI    '
          ENDIF
      ELSE IF (MENU .EQ. 7) THEN
          CALL SWINSC (RNMODE,MESIC)
      ELSE IF (MENU .EQ. 8) THEN
          CALL SWPHOT (RNMODE,MEPHO)
      ELSE IF (MENU .EQ. 9) THEN
          CALL SWEVAP (RNMODE,MEEVP)
C-PW Handle SWSOM menu Item
C Also note modified 200 FORMAT statement below!
C The Subroutine SWSOM is appended to the bottom of SESIM
C PW, CIP, 8-11-97
C
      ELSE IF (MENU .EQ. 10) THEN
          CALL SWSOM (RNMODE,MESOM)
C-PW
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  170 FORMAT (I2,A6)
  200 FORMAT (//,9X,'SIMULATION CONTROL AND MODIFICATION',
     &         /,9X,'===================================',//,
     & 5X,' 0. Return to Main Menu ',//,
     & 5X,' 1. Water Balance Simulation .................] ',A17,/,
     & 5X,' 2. Nitrogen Balance Simulation ..............] ',A1,/,
     & 5X,' 3. Nitrogen Fixation Simulation .............] ',A15,/,
     & 5X,' 4. Phosphorus Balance Simulation ............] ',A1,/,
     & 5X,' 5. Pest and Disease Interaction Simulation ..] ',A1,/,
     & 5X,' 6. Weather Input Method .....................] ',A15,/,
     & 5X,' 7. Initial Conditions .......................] ',A15,/,
     & 5X,' 8. Photosynthesis Simulation Method .........] ',A15,/,
     & 5X,' 9. Evaporation Simulation Method ............] ',A16,/,
     & 5X,'10. Soil organic matter dynamic simulation ...] ',A16)
  280 FORMAT ( /,9X,'To Be Able To Simulate The Nitrogen Balance,',
     &         /,9X,'The Water Balance Has To BE Simulated !',/)
  285 FORMAT ( /,9X,'To Use the Zonal Energy Balance ET Method, ',
     &         /,9X,'the Leaf Photosynthesis Method MUST be used !',/)
  290 FORMAT (//,9X,'SELECTION ? [ Default = 0 ] ===> ',$)
  400 FORMAT (   9X,'Option Currently Not Available')

      END

C=======================================================================
C  SENIT, Subroutine
C
C  Determines N simulation control in Rice
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : ISWNIT,NSWITCH,RNMODE
C
C  LOCAL  : ERRKEY,WTMANT,PHMANT,INMANT,ETMANT,MENU,NLOOP
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SESIM SEFERT
C
C  Calls  : ERROR CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SENIT (RNMODE,ISWNIT,NSWITCH)

      IMPLICIT     NONE

      CHARACTER*1  RNMODE,ISWNIT,ISW(10)
      CHARACTER*6  ERRKEY
      CHARACTER*40 PHMANT

      INTEGER      NLOOP,MENU,NSWITCH,I

      PARAMETER (ERRKEY = 'SENIT ')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (RNMODE .EQ. 'I') THEN
         CALL CLEAR
         IF (NSWITCH .EQ. 0) THEN
             PHMANT = 'Switch off nitrogen balance totally'
         ELSE IF (NSWITCH .EQ. 1) THEN
             PHMANT = 'Switch on nitrogen balance'
         ELSE IF (NSWITCH .EQ. 5) THEN
             PHMANT = 'Switch off nitrification only'
         ELSE IF (NSWITCH .EQ. 6) THEN
             PHMANT = 'Switch off denitrification only'
         ELSE IF (NSWITCH .EQ. 7) THEN
             PHMANT = 'Switch off NH3 loss from floodwater'
         ELSE IF (NSWITCH .EQ. 8) THEN
             PHMANT = 'Switch off NH3 loss from drained soils'
         ELSE IF (NSWITCH .EQ. 9) THEN
             PHMANT = 'Switch off leaching'
         ELSE IF (NSWITCH .EQ. 10) THEN
             PHMANT = 'Switch off runoff losses'
         ENDIF

         DO I = 1, 8
            IF (ISWNIT .EQ. 'N') THEN
               ISW(2) = 'Y'
               ISW(I) = 'N'
             ELSE
               ISW(I) = 'Y'
               ISW(2) = 'N'
            ENDIF
         END DO
         IF (NSWITCH .GT. 1) THEN
            ISW (NSWITCH-2) = 'N'
         ENDIF
         WRITE (*,3400) (ISW(I),I=1,8)
         WRITE (*, 290) PHMANT
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
          ISWNIT  = 'Y'
          NSWITCH = 1
      ELSE IF (MENU .EQ. 2) THEN
          ISWNIT  = 'N'
          NSWITCH = 0
      ELSE IF (MENU .EQ. 3) THEN
          ISWNIT  = 'Y'
          NSWITCH = 5
      ELSE IF (MENU .EQ. 4) THEN
          ISWNIT  = 'Y'
          NSWITCH = 6
      ELSE IF (MENU .EQ. 5) THEN
          ISWNIT  = 'Y'
          NSWITCH = 7
      ELSE IF (MENU .EQ. 6) THEN
          ISWNIT  = 'Y'
          NSWITCH = 8
      ELSE IF (MENU .EQ. 7) THEN
          ISWNIT  = 'Y'
          NSWITCH = 9
      ELSE IF (MENU .EQ. 8) THEN
          ISWNIT  = 'Y'
          NSWITCH = 10
      ENDIF

      GOTO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  290 FORMAT (//,9X,'Current N simulation method  ===> ',A40,/
     &           9X,'Selection (#) ? [ ��� = 0 ] ---> ',$)
 3400 FORMAT (9X,'Nitrogen Simulation Methods',/,
     &        9X,'===========================',//,
     & 6X,'0. Return to Previous Menu ',//
     & 6X,'1. Switch on simulation of N balance ................| ',A,/,
     & 6X,'2. Switch off simulation of N balance totally........| ',A,/,
     & 6X,'3. Switch off nitrification only.....................| ',A,/,
     & 6X,'4. Switch off denitrification only...................| ',A,/,
     & 6X,'5. Switch off ammonia loss from floodwater only......| ',A,/,
     & 6X,'6. Switch off ammonia loss from drained soils only...| ',A,/,
     & 6X,'7. Switch off leaching only..........................| ',A,/,
     & 6X,'8. Switch off runoff loss............................| ',A,/)

      END

C=======================================================================
C  SWWTH, Subroutine
C
C  Determines weather input methods
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,MEWTH,PATHWT
C
C  LOCAL  : ERRKEY,WTMANT,NLOOP,MENU
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SESIM
C
C  Calls  : ERROR CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SWWTH (RNMODE,MEWTH,PATHWT)

      IMPLICIT     NONE

      CHARACTER*1  RNMODE,MEWTH
      CHARACTER*6  ERRKEY
      CHARACTER*15 WTMANT
      CHARACTER*80 PATHWT

      INTEGER      NLOOP,MENU

      PARAMETER (ERRKEY = 'SWWTH ')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (RNMODE .EQ. 'I') THEN
         CALL CLEAR
         IF (MEWTH .EQ. 'M') THEN
            WTMANT = 'OBSERVED DATA  '
          ELSE IF (MEWTH .EQ. 'G') THEN
            WTMANT = 'SIMULATED DATA '
          ELSE IF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W') THEN
            WTMANT = 'INTERNALLY SIM.'
         ENDIF
         WRITE (*,3400)
         WRITE (*, 290) WTMANT
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 1) THEN
          MEWTH  = 'M'
      ELSE IF (MENU .EQ. 2) THEN
          MEWTH  = 'G'
      ELSE IF (MENU .EQ. 3) THEN
          MEWTH  = 'S'
      ENDIF

      PATHWT = '                                                       '

      RETURN

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  290 FORMAT (//,
     &        9X,'CURRENT WEATHER INPUT CONDITIONS ===> ',A15,/
     &        9X,'SELECTION (#) ? [ Default = 0 ]  ---> ',$)
 3400 FORMAT (9X,'WEATHER INPUT METHODS',/,
     &        9X,'=====================',//,
     & 5X,' 0. Return to Previous Menu ',//
     & 5X,' 1. Field Observed Data ..............................|',/,
     & 5X,' 2. Externally Simulated Data ........................|',/,
     & 5X,' 3. Internally Generated Data ........................|',/)

      END

C=======================================================================
C  SWINSC, Subroutine
C
C  Determines initial soil conditions
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,MESIC
C
C  LOCAL  : ERRKEY,INMANT,NLOOP,MENU
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SESIM
C
C  Calls  : ERROR CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SWINSC (RNMODE,MESIC)

      IMPLICIT     NONE

      CHARACTER*1  RNMODE,MESIC
      CHARACTER*6  ERRKEY
      CHARACTER*15 INMANT

      INTEGER      NLOOP,MENU

      PARAMETER (ERRKEY = 'SWINSC')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (RNMODE .EQ. 'I') THEN
         CALL CLEAR
         IF (MESIC .EQ. 'M') THEN
            INMANT = 'AS REPORTED    '
          ELSE IF (MESIC .EQ. 'S') THEN
            INMANT = 'PREVIOUS RUN   '
         ENDIF
         WRITE (*,3400)
         WRITE (*, 290) INMANT
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 1) THEN
          MESIC  = 'M'
      ELSE IF (MENU .EQ. 2) THEN
          MESIC  = 'S'
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  290 FORMAT (//,9X,'CURRENT INITIAL SOIL CONDITIONS ===> ',A15,/
     &           9X,'SELECTION (#) ? [ Default = 0 ] ---> ',$)
 3400 FORMAT (9X,'INITIAL SOIL CONDITIONS',/,
     &        9X,'=======================',//,
     & 5X,' 0. Return to Previous Menu ',//
     & 5X,' 1. As Reported from Input Files .....................|',/,
     & 5X,' 2. Simulated Output Conditions from Previous Run ....|',/)

      END

C=======================================================================
C  SWPHOT, Subroutine
C
C  Determines photosynthesis calculation methods
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,MEPHO
C
C  LOCAL  : ERRKEY,PHMANT,NLOOP,MENU
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SESIM
C
C  Calls  : ERROR CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SWPHOT (RNMODE,MEPHO)

      IMPLICIT     NONE

      CHARACTER*1  RNMODE,MEPHO
      CHARACTER*6  ERRKEY
      CHARACTER*15 PHMANT

      INTEGER      NLOOP,MENU

      PARAMETER (ERRKEY = 'SWPHOT')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (RNMODE .EQ. 'I') THEN
         CALL CLEAR
         IF (MEPHO .EQ. 'C') THEN
            PHMANT = 'CANOPY PHOT    '
          ELSE IF (MEPHO .EQ. 'L') THEN
            PHMANT = 'LEAF LEVEL PHOT'
         ENDIF
         WRITE (*,3400)
         WRITE (*, 290) PHMANT
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 1) THEN
          MEPHO  = 'C'
      ELSE IF (MENU .EQ. 2) THEN
          MEPHO  = 'L'
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  290 FORMAT (//,9X,'CURRENT PHOTOSYNTHESIS METHOD   ===> ',A15,/
     &           9X,'SELECTION (#) ? [ Default = 0 ] ---> ',$)
 3400 FORMAT (9X,'PHOTOSYNTHESIS CALCULATION METHODS',/,
     &        9X,'==================================',//,
     & 5X,' 0. Return to Previous Menu ',//
     & 5X,' 1. Canopy Photosynthesis ............................|',/,
     & 5X,' 2. Leaf Level Photosynthesis ........................|',/)

      END

C=======================================================================
C  SWEVAP, Subroutine
C
C  Determines evaporation calculation methods
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,MEEVP
C
C  LOCAL  : ERRKEY,ETMANT,NLOOP,MENU
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SESIM
C
C  Calls  : ERROR CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SWEVAP (RNMODE,MEEVP)

      IMPLICIT     NONE

      CHARACTER*1  RNMODE,MEEVP
      CHARACTER*6  ERRKEY
      CHARACTER*16 ETMANT

      INTEGER      NLOOP,MENU

      PARAMETER (ERRKEY = 'SWEVAP')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (RNMODE .EQ. 'I') THEN
         CALL CLEAR
         IF (MEEVP .EQ. 'P') THEN
            ETMANT = 'FAO-PENMAN      '
         ELSE IF (MEEVP .EQ. 'R') THEN
            ETMANT = 'PRIESTLEY-TAYLOR'
         ELSE IF (MEEVP .EQ. 'Z') THEN
            ETMANT = 'ZONAL ENERGY BAL'
         ENDIF
         WRITE (*,3400)
         WRITE (*, 290) ETMANT
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 1) THEN
         MEEVP  = 'R'
      ELSE IF (MENU .EQ. 2) THEN
         MEEVP  = 'P'
      ELSE IF (MENU .EQ. 3) THEN
         MEEVP  = 'Z'
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  290 FORMAT (//,9X,'CURRENT EVAPOTRANSPIRATION METHOD   ===> ',A16,/
     &           9X,'SELECTION (#) ? [ Default = 0 ]     ---> ',$)
 3400 FORMAT (9X,'EVAPOTRANSPIRATION CALCULATION METHODS',/,
     &        9X,'======================================',//,
     & 5X,' 0. Return to Previous Menu ',//
     & 5X,' 1. Priestley-Taylor Evapotranspiration ...............|',/,
     & 5X,' 2. Penman-FAO Evapotranspiration .....................|',/)
C-AD & 5X,' 2. Penman-FAO Evapotranspiration .....................|',/,
C-AD & 5X,' 3. Zonal Energy-Balance Evapotranspiration ...........|',/)

      END

C=======================================================================
C  SWWATB, Subroutine
C
C  Determines water balance simulation methods
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       G.P.F       9-95
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,MEHYD
C
C  LOCAL  : ERRKEY,HYMANT,NLOOP,MENU
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SESIM
C
C  Calls  : ERROR CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C=======================================================================

      SUBROUTINE SWWATB (RNMODE,MEHYD,ISWWAT,ISWNIT,NSWITCH)

      IMPLICIT     NONE

      CHARACTER*1  RNMODE,MEHYD,ISWWAT,ISWNIT
      CHARACTER*6  ERRKEY
      CHARACTER*16 HYMANT

      INTEGER      NLOOP,MENU,NSWITCH

      PARAMETER (ERRKEY = 'SWWATB')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (RNMODE .EQ. 'I') THEN
         CALL CLEAR
         IF (ISWWAT .EQ. 'Y') THEN
            IF (MEHYD .EQ. 'R') THEN
              HYMANT = 'RITCHIE         '
            ELSE IF (MEHYD .EQ. 'A') THEN
              HYMANT = 'ADAPT HYDROLOGY '
            ENDIF
         ELSE
            HYMANT = 'NO WATER BALANCE'
         ENDIF

         WRITE (*,3400)
         WRITE (*, 290) HYMANT
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 1) THEN
          MEHYD  = 'R'
          ISWWAT = 'Y'
C-AD  ELSE IF (MENU .EQ. 2) THEN
C-AD      MEHYD  = 'A'
C-AD      ISWWAT = 'Y'
C-AD  ELSE IF (MENU .EQ. 3) THEN
C-AD      ISWWAT = 'N'
      ELSE IF (MENU .EQ. 2) THEN
          ISWWAT = 'N'
          ISWNIT = 'N'
          NSWITCH = 0
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  290 FORMAT (//,9X,'CURRENT WATER BALANCE METHOD    ===> ',A15,/
     &           9X,'SELECTION (#) ? [ Default = 0 ] ---> ',$)
 3400 FORMAT (9X,'WATER BALANCE SIMULATION METHODS',/,
     &        9X,'================================',//,
     & 5X,' 0. Return to Previous Menu ',//
     & 5X,' 1. Ritchie water balance ............................|',/,
C-AD & 5X,' 2. ADAPT hydrology  .................................|',/,
C-AD & 5X,' 3. No water balance simulation ......................|',/)
     & 5X,' 2. No water balance simulation ......................|',/)

      END

C=======================================================================
C  SWSOM, Subroutine
C
C  Determines soil organic matter calculation methods
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,MEEVP
C
C  LOCAL  : ERRKEY,ETMANT,NLOOP,MENU
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SESIM
C
C  Calls  : ERROR CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SWSOM (RNMODE,MESOM)

      IMPLICIT     NONE

      CHARACTER*1  RNMODE,MESOM
      CHARACTER*6  ERRKEY
      CHARACTER*16 OMMANT

      INTEGER      NLOOP,MENU

      PARAMETER (ERRKEY = 'SWSOM')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (RNMODE .EQ. 'I') THEN
         CALL CLEAR
         IF (MESOM .EQ. 'G') THEN
            OMMANT = 'Godwin          '
         ELSE IF (MESOM .EQ. 'P') THEN
            OMMANT = 'Parton          '
         ELSE 
            OMMANT = 'Other           '
         ENDIF
         WRITE (*,3400)
         WRITE (*, 290) OMMANT
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 1) THEN
         MESOM  = 'G'
      ELSE IF (MENU .EQ. 2) THEN
         MESOM  = 'P'
      ELSE IF (MENU .EQ. 3) THEN
         MESOM  = 'O'
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  290 FORMAT (//,9X,'CURRENT SOIL ORGANIC MATTER METHOD  ===> ',A16,/
     &           9X,'SELECTION (#) ? [ Default = 0 ]     ---> ',$)
 3400 FORMAT (9X,'SOIL ORGANIC MATTER CALCULATION METHODS',/,
     &        9X,'=======================================',//,
     & 5X,' 0. Return to Previous Menu ',//
     & 5X,' 1. Godwin (Ceres-based)...............................|',/,
     & 5X,' 2. Parton (Century-based).............................|',/,
     & 5X,' 3. Other (not implemented yet)........................|',/)

      END
