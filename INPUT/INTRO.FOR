C========================================================================
C  INTRO, Subroutine
C
C  Prints model opening screen
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/93 GH  Written
C  09/14/93 PWW Header modifications and header changes
C  12/31/97 GH  Change date to 12/31/97, DSSAT v3.1
C  05/06/98 GH  Change release version to DSSAT v3.5
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : None
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : INPUT
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  None   :
C=======================================================================

      SUBROUTINE INTRO

      IMPLICIT  NONE

      CHARACTER*1  ANS,BOX_SID
      CHARACTER*2  CG
      CHARACTER*6  ERRKEY
      CHARACTER*7  FILELS
      CHARACTER*54 ZCG(15),ZCE(15),ZRI(15),ZCS(15),ZSS(15),ZSC(15),
C-   &             ZSU(15),ZPI(15),ZAR(15),ZCO(15)
     &             ZSU(15)
      CHARACTER*56 BOX_TOP,BOX_BOT,BOX_DAT
      CHARACTER*80 CHARTEST

      INTEGER      LUNLST,ERRNUM,I,LINF,ISECT

      LOGICAL      CROPGRO,CERES,SUBSTOR,CASSAVA,RICE,CANEGRO,OILSUN
      LOGICAL      APPLIC,ALOHA,AROID,COTTON

C     COMMON /INT_COM/ ZCG,ZCE,ZRI,ZCS,ZSS,ZSC,ZSU,ZPI,ZAR,ZCO
      COMMON /INT_COM/ ZCG,ZCE,ZRI,ZCS,ZSS,ZSC,ZSU

      PARAMETER (LUNLST = 17)
      PARAMETER (ERRKEY = 'INTRO ')


      DATA BOX_TOP/
     & 'ีออออออออออออออออออออออออออออออออออออออออออออออออออออออธ'/

      DATA BOX_SID/'ณ'/

      DATA BOX_BOT/
     & 'ิออออออออออออออออออออออออออออออออออออออออออออออออออออออพ'/

      DATA BOX_DAT/
     & 'ณ                                      15-June-1998    ณ'/

      DATA ZCG/
     & '                 CROPGRO 3.5 (98.0)                   ',
     & '                                                      ',
     & 'G. Hoogenboom, J.W. Jones, K.J. Boote, N.B. Pickering,',
     & '       W.T. Bowen, W.D. Batchelor, and J.W. White     ',
     & '                                                      ',
     & '  The University of Georgia & University of Florida   ',
     & '                                                      ',
     & ' CROPGRO simulates crop growth and development, soil  ',
     & ' water dynamics, and soil nitrogen dynamics in        ',
     & ' response to weather, soil characteristics, cultivar  ',
     & ' characteristics and crop management.  This version   ',
     & ' simulates soybean, peanut, and dry bean crops, as    ',
     & ' well as chickpea, tomato and pasture (bahia grass).  ',
     & ' It uses the ICASA standard data formats and          ',
     & ' files for DSSAT Version 3.5.                         '/
      DATA ZSC/
     & '                 CANEGRO 3.5 (98.0)                   ',
     & '                                                      ',
     & '           N.G. Inman-Bamber and G.A. Kiker           ',
     & '  in collaboration with G. Hoogenboom and J.W. Jones  ',
     & '                                                      ',
     & '           South African Sugar Association            ',
     & '         Experiment Station, Mount Edgecombe;         ',
     & '   The University of Georgia & University of Florida  ',
     & '                                                      ',
     & '  CANEGRO simulates sugarcane growth, development,    ',
     & '  and soil water dynamics in response to weather,     ',
     & '  soil characteristics, cultivar characteristics,     ',
     & '  and crop management.  This version uses the ICASA   ',
     & '  standard data formats and files for DSSAT Version   ',
     & '  3.5.                                                '/
      DATA ZCE/
     & '              GENERIC CERES 3.5 (98.0)                ',
     & '                                                      ',
     & '   J.T. Ritchie, U. Singh, D.C. Godwin, W.T. Bowen,   ',
     & '  P.W. Wilkens, B. Baer, G. Hoogenboom and L.A. Hunt  ',
     & '                                                      ',
     & '      International Fertilizer Development Center,    ',
     & '  Michigan State University &  University of Georgia  ',
     & '                                                      ',
     & ' CERES simulates crop growth and development, soil    ',
     & ' water dynamics, and soil nitrogen dynamics in        ',
     & ' response to weather, soil characteristics, cultivar  ',
     & ' characteristics and crop management.  This version   ',
     & ' simulates barley, maize, millet, sorghum, and wheat  ',
     & ' crops.  It uses the ICASA standard data formats      ',
     & ' and files for DSSAT Version 3.5.                     '/
      DATA ZRI/
     & '             CERES - RICE 3.5 (98.0)                  ',
     & '                                                      ',
     & '   U. Singh, D.C. Godwin, J. T. Ritchie, W.T. Bowen,  ',
     & '  P.W. Wilkens, B. Baer, G. Hoogenboom and L.A. Hunt  ',
     & '                                                      ',
     & '      International Fertilizer Development Center,    ',
     & '   Michigan State University, University of Georgia   ',
     & '                                                      ',
     & ' CERES-Rice simulates crop growth and development,    ',
     & ' soil water dynamics, and soil nitrogen dynamics in   ',
     & ' response to weather, soil characteristics, cultivar  ',
     & ' characteristics and crop management.  This version   ',
     & ' simulates both lowland (flooded) and upland rice     ',
     & ' crops.  It uses the ICASA standard data formats      ',
     & ' and files for DSSAT Version 3.5.                     '/
      DATA ZCS/
     & '             CROPSIM - CASSAVA 3.5 (98.0)             ',
     & '                                                      ',
     & '  L.A. Hunt, R.B. Matthews, P.W. Wilkens, W.T. Bowen  ',
     & '                                                      ',
     & '             The University of Guelph &               ',
     & '     International Fertilizer Development Center      ',
     & '                                                      ',
     & '     This model is a FORTRAN conversion of the        ',
     & '  Gumcas cassava model (Field Crops Research,36       ',
     & '  (1994) 69-84). The number of genetic coefficients   ',
     & '  has been reduced by placing those that appear to    ',
     & '  vary little among cultivars in a "species" file.    ',
     & '  This file is stuctured to conform with that used    ',
     & '  by FAO to store crop environmental response data.   ',
     & '                                                      '/
      DATA ZSS/
     & '                 SUBSTOR 3.5 (98.0)                   ',
     & '                                                      ',
     & '       T.S. Griffin, B.S. Johnson, J.T. Ritchie,      ',
     & '             P.W. Wilkens and W.T. Bowen              ',
     & '                                                      ',
     & '            Michigan State University &               ',
     & '     International Fertilizer Development Center      ',
     & '                                                      ',
     & ' SUBSTOR-Potato simulates crop growth & development,  ',
     & ' soil water dynamics, and soil nitrogen dynamics in   ',
     & ' response to weather, soil characteristics, cultivar  ',
     & ' characteristics and crop management. It uses the     ',
     & ' ICASA standard data formats and files for DSSAT      ',
     & ' Version 3.5.                                         ',
     & '                                                      '/
      DATA ZSU/
     & '               OILCROP-SUN 3.5 (98.0)                 ',
     & '                                                      ',
     & '              F.J. Villalobos, A.J. Hall,             ',
     & '             J.T. Ritchie and P.W. Wilkens            ',
     & '                                                      ',
     & '    Cขrdoba University, University of Buenos Aires,   ',
     & '              Michigan State University, &            ',
     & '      International Fertilizer Development Center     ',
     & '                                                      ',
     & ' OILCROP-SUN simulates sunflower (Helianthus          ',
     & ' annuus L.) growth and development, yield, and soil   ',
     & ' water and nitrogen dynamics in response to weather,  ',
     & ' soil characteristics, cultivar characteristics and   ',
     & ' crop management.  It uses the ICASA standard data    ',
     & ' formats and files for DSSAT version 3.5.             '/
c     DATA ZPI/
c    & '             ALOHA-PINEAPPLE 3.5 (98.0)               ',
c    & '                                                      ',
c    & '             D. Bartholomew and J. Zhang,             ',
c    & 'in collaboration with P.W. Wilkens and G. Hoogenboom  ',
c    & '                                                      ',
c    & '              University of Hawaii,                   ',
c    & 'Int. Fert. Development Centerand the Univ. of Georgia ',
c    & '                                                      ',
c    & ' ALOHA-PINEAPPLE simulates pineapple (Anasas commosus)',
c    & ' growth and development, yield, and soil water        ',
c    & ' and nitrogen dynamics in response to weather, soil   ',
c    & ' characteristics, cultivar characteristics and crop   ',
c    & ' management.  The model predicts fresh pineapple and  ',
c    & ' it uses the ICASA standard data formats and files    ',
c    & ' for DSSAT version 3.5.                               '/
c     DATA ZAR/
c    & '           AROID-Taro/Tanier 3.5 (98.0)               ',
c    & '                                                      ',
c    & '                                                      ',
c    & '                                                      ',
c    & '                                                      ',
c    & '                                                      ',
c    & '                                                      ',
c    & '                                                      ',
c    & '                                                      ',
c    & '                                                      ',
c    & '                                                      ',
c    & '                                                      ',
c    & '                                                      ',
c    & '                                                      ',
c    & '                                                      '/
c     DATA ZCO/
c    & '             CSIRO-COTTON 3.5 (98.0)                  ',
c    & '                                                      ',
c    & '             S. Milroy, et al....                     ',
c    & '                .............                         ',
c    & '                                                      ',
c    & 'Cotton Research Unit, Division of Plant Industry,     ',
c    & 'CSIRO, Narrabri,NWS,Australia                         ',
c    & '                                                      ',
c    & ' CSIRO-COTTON simulates cotton (Go...................)',
c    & '                                                      ',
c    & '                                                      ',
c    & '                                                      ',
c    & '                                                      ',
c    & '                                                      ',
c    & '                                                      '/
      FILELS  = 'EXP.LST'
      CROPGRO = .FALSE.
      CANEGRO = .FALSE.
      CERES   = .FALSE.
      SUBSTOR = .FALSE.
      CASSAVA = .FALSE.
      OILSUN  = .FALSE.
      APPLIC  = .FALSE.
      ALOHA   = .FALSE.
      AROID   = .FALSE.
      COTTON  = .FALSE.

      OPEN (LUNLST, FILE = FILELS,STATUS = 'OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILELS,0)

      I = 0
  300 CONTINUE
      I = I + 1
      LINF = 0
  350 CONTINUE
      CALL IGNORE (LUNLST,LINF,ISECT,CHARTEST)
      IF (ISECT .EQ. 2) GO TO 350
      IF (ISECT .EQ. 1) THEN
         READ (CHARTEST,410,IOSTAT=ERRNUM) CG
         IF (INDEX ('SBPNBNCHPPPEVB',CG) .GT. 0) THEN
            CROPGRO = .TRUE.
         ELSEIF (INDEX ('TMPRC3C4G0G1G2G3G4G5G6G7G8G9',CG) .GT. 0) THEN
           CROPGRO = .TRUE.
         ELSEIF (INDEX ('MZWHBASGML',CG) .GT. 0) THEN
           CERES   = .TRUE.
         ELSEIF (INDEX ('RI',CG) .GT. 0) THEN
           RICE    = .TRUE.
         ELSEIF (INDEX ('CS',CG) .GT. 0) THEN
           CASSAVA = .TRUE.
         ELSEIF (INDEX ('PT',CG) .GT. 0) THEN
           SUBSTOR = .TRUE.
         ELSEIF (INDEX ('SC',CG) .GT. 0) THEN
           CANEGRO = .TRUE.
         ELSEIF (INDEX ('SU',CG) .GT. 0) THEN
           OILSUN  = .TRUE.
         ELSEIF (INDEX ('PI',CG) .GT. 0) THEN
           ALOHA   = .TRUE.
         ELSEIF (INDEX ('TNTA',CG) .GT. 0) THEN
            AROID   = .TRUE.
         ELSEIF (INDEX ('CO',CG) .GT. 0) THEN
            COTTON  = .TRUE.
         ELSEIF (INDEX ('SNSQGSFL',CG) .GT. 0) THEN
           APPLIC  = .TRUE.
        ENDIF
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,' ',0)
      ELSE
         GO TO 800
      ENDIF

      GO TO 300
  800 CONTINUE
      CLOSE (LUNLST)

      CALL HOME
      WRITE (*,250) BOX_TOP
      WRITE (*,100) BOX_SID,BOX_SID
      WRITE (*,250) BOX_BOT

      CALL CURPOS ('04')
      WRITE(*,250) BOX_TOP
      DO I = 1, 15
        IF (CROPGRO) THEN
          WRITE (*,255) BOX_SID,ZCG(I),BOX_SID
        ELSE IF (CANEGRO) THEN
          WRITE (*,255) BOX_SID,ZSC(I),BOX_SID
        ELSE IF (CERES) THEN
          WRITE (*,255) BOX_SID,ZCE(I),BOX_SID
        ELSE IF (RICE) THEN
          WRITE (*,255) BOX_SID,ZRI(I),BOX_SID
        ELSE IF (CASSAVA) THEN
          WRITE (*,255) BOX_SID,ZCS(I),BOX_SID
        ELSE IF (SUBSTOR) THEN
          WRITE (*,255) BOX_SID,ZSS(I),BOX_SID
        ELSE IF (OILSUN) THEN
          WRITE (*,255) BOX_SID,ZSU(I),BOX_SID
c       ELSE IF (ALOHA) THEN
c         WRITE (*,255) BOX_SID,ZPI(I),BOX_SID
c       ELSE IF (AROID) THEN
c         WRITE (*,255) BOX_SID,ZAR(I),BOX_SID
c       ELSE IF (COTTON) THEN
c         WRITE (*,255) BOX_SID,ZCO(I),BOX_SID
        ELSE IF (APPLIC) THEN
          RETURN
        ELSE
          WRITE (*,260)
 260       FORMAT   ( ' INPUT program has not been defined for this',
     &                ' crop simulation module !',/,
     &                ' Please contact INPUT Module developer.')
          STOP
        ENDIF
      END DO
      WRITE(*,250) BOX_DAT
      WRITE(*,250) BOX_BOT
      CALL CURPOS ('22')
      WRITE (*,250) BOX_TOP
      WRITE (*,150) BOX_SID,BOX_SID
      WRITE (*,250) BOX_BOT
      READ (5,'(A1)') ANS

      RETURN

  100 FORMAT (12X,A1,15X,'DSSAT 3.5 Generic Input',16X,A1)
  150 FORMAT (12X,A1,4X,
     &        'Please press < ENTER > key (ฤฤู) to continue',5X,A1)
  250 FORMAT (12X,A56)
  255 FORMAT (12X,A1,A54,A1)
  410 FORMAT (12X,A2)
      END
