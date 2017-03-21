C=======================================================================
C  INPHEN, Subroutine
C
C  Initialization of phenology
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2. Header revision and minor changes           P.W.W.      2-7-93
C  3. Added switch common block                   P.W.W.      2-7-93
C-----------------------------------------------------------------------
C  INPUT  :
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : PINE
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  STGDOY :
C=======================================================================

      SUBROUTINE INPHEN (CROP,STGDOY,MDATE,YRSIM,STNAME,ISWNIT,XSTAGE,
     &                   TANC,PLTPOP,STOVWT,SEEDNI,ROOTN,STOVN,GRAINN,
     &                   HAREND)

      IMPLICIT     NONE

      INCLUDE     'GEN2.BLK'
      INCLUDE     'GEN4.BLK'

      INTEGER      STGDOY(20),MDATE,YRSIM,I,HAREND
      REAL         XSTAGE,TANC,PLTPOP,STOVWT,SEEDN,SEEDNI
      REAL         ROOTN,STOVN,GRAINN
      CHARACTER    ISWNIT*1,CROP*2
      CHARACTER*10 PISTGNAM(20),FASTGNAM(20),STNAME(20)

      DATA PISTGNAM/'Zero Stem ','Forcing   ','SCY       ',
     1              'Early Flwr','Fruit Harv','Maturity  ',
     2              'Planting  ','Root Init.','Leaf Emerg',
     3              '          ','          ','          ',
     4              '          ','Start Sim ','End Sim   ',
     5              '          ','          ','          ',
     6              '          ','Harvest   '/
      DATA FASTGNAM/'          ','          ','          ',
     1              '          ','          ','          ',
     2              '          ','          ','          ',
     3              '          ','          ','          ',
     4              '          ','Start Sim ','End Sim   ',
     5              '          ','          ','          ',
     6              '          ','          '/

      ISTAGE = 7
      XSTAGE = 0.1

      DO I = 1, 20
         STNAME(I) = '          '
         STGDOY(I) = 999999
         !
         ! Define names for growth stages
         !
         IF (CROP .EQ. 'PI') THEN
            STNAME(I) = PISTGNAM (I)
            SEEDN     = 0.000540
          ELSEIF (CROP .EQ. 'FA') THEN
            STNAME(I) = FASTGNAM (I)
         ENDIF
      END DO

      STGDOY(14) = YRSIM
      MDATE      = -99
      HAREND     = -99

      TBASE      = 12.0
      LAI        = PLTPOP*PLA*0.0001
      BIOMAS     = WTINITIAL*PLTPOP
      PLA        = WTINITIAL*0.6*63.0
      LFWT       = WTINITIAL*0.53
      BASLFWT    = LFWT*0.66
      STMWT      = WTINITIAL*0.115
      STOVWT     = WTINITIAL

      IF (ISWNIT .NE. 'Y') THEN
         TANC = 0.0
      ENDIF
      !
      ! Calculate initial SEED N
      !
      SEEDNI = (ROOTN+STOVN+GRAINN+SEEDN)*PLTPOP

      RETURN
      END
