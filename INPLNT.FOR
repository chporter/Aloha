C=======================================================================
C  INPLNT, Subroutine
C
C  Determines program initialization
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2. Header revision and minor changes           P.W.W.      2-7-93
C  3. Added switch common block                   P.W.W.      2-7-93
C-----------------------------------------------------------------------
C  INPUT  : TOTIR,NAPNIT,TRUNOF,TDRAIN,CANNAA,CANWAA
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
C  NAPNIT :
C  I      : Loop counter
C  TOTIR  : Total irrigation (mm)
C  CANNAA :
C  CANWAA :
C  TRUNOF :
C  TDRAIN :
C  TLCH   :
C  TMINER :
C  TNOX   :
C=======================================================================

      SUBROUTINE INPLNT (TOTIR,NAPNIT,TRUNOF,TDRAIN,CANNAA,CANWAA,
     &           AMTNIT,NAP,TLCH,TNOX,CUMDTT,SUMDTT,DTT,SEEDNI,
     &           WTNLF,WTNST,WTNSH,WTNRT,WTNLO,BWAH,SDWTAH,
     &           TOPWT,SDWTAM,WTNUP,NAPRES)

      IMPLICIT  NONE

      INCLUDE  'GEN3.BLK'
      INCLUDE  'GEN4.BLK'
      INCLUDE  'NTRC1.BLK'
      INCLUDE  'PREDOB.BLK'

      INTEGER   NAPNIT,I,NAP,NAPRES
      REAL      TOTIR,CANNAA,CANWAA,TRUNOF,TDRAIN,AMTNIT,TLCH,TNOX
      REAL      CUMDTT,SUMDTT,DTT,SEEDNI
      REAL      SDWTAH,SDWTAM,WTNUP,TOPWT,BWAH
      REAL      WTNLF,WTNST,WTNSH,WTNRT,WTNLO

      LN     = 0.0
      FLRWT  = 0.0
      FRTWT  = 0.0
      CRWNWT = 0.0
      SKWT   = 0.0
      GROSK  = 0.0
      YIELD  = 0.0
      SENLA  = 0.0
      SLAN   = 0.0
      CARBO  = 0.0
      GRNWT  = 0.0
      RTWT   = 0.0
      LFWT   = 0.0
      STMWT  = 0.0
      SDWTAH = 0.0
      SDWTAM = 0.0
      WTNUP  = 0.0
      TOPWT  = 0.0
      BWAH   = 0.0
      WTNLF  = 0.0
      WTNST  = 0.0
      WTNSH  = 0.0
      WTNRT  = 0.0
      WTNLO  = 0.0
      GPSM   = 0.0
      GPP    = 0.0
      PTF    = 0.0

      DO I = 1, 8
         TMFAC1(I) = 0.931 + 0.114*I-0.0703*I**2+0.0053*I**3
      END DO

      DO I = 1, 20
         RLV(I) = 0.0
      END DO

      BIOMAS = 0.0
      LEAFNO = 0.0
      LAI    = 0.0
      XN     = 0.0
      SNOW   = 0.0
      SWFAC  = 1.0
      TURFAC = 1.0
      TRWU   = 0.0
      ICSDUR = 0
      NSTRES = 1.0
      AGEFAC = 1.0
      NDEF3  = 1.0
      NDEF4  = 1.0
      ANFAC  = 0.0
      NFAC   = 1.0
      ATANC  = 0.0
      TANC   = 0.044
      RANC   = 0.0
      VANC   = 0.0
      VMNC   = 0.0
      TMNC   = 0.0
      RCNP   = 0.0
      TCNP   = 0.0
      TLCH   = 0.0
      TNOX   = 0.0
      SEEDNI = 0.0
      STOVN  = 0.0
      ROOTN  = 0.0
      GRAINN = 0.0
      GNP    = 0.0
      XGNP   = 0.0
      APTNUP = 0.0
      GNUP   = 0.0
      TOTNUP = 0.0
      CUMDTT = 0.0
      SUMDTT = 0.0
      DTT    = 0.0
      PRECIP = 0.0
      CANNAA = 0.0
      CANWAA = 0.0
      !
      ! Water balance initialization
      !
      NAP    = 0
      CRAIN  = 0.0
      TRUNOF = 0.0
      TRON   = 0.0
      TDRAIN = 0.0
      CET    = 0.0
      CES    = 0.0
      CEP    = 0.0
      CRAIN  = 0.0
      CEO    = 0.0
      TOTIR  = 0.0
      SWDEF  = 0.0
      ATHETA = 1.0
      TSEDM  = 0.0
      !
      ! Nitrogen balance initialization
      !
      NAPNIT = 0
      AMTNIT = 0.0
      NAPRES = 0
      AMTRES = 0.0
      !
      ! Initialze stress indices
      !
      DO I = 1, 6
         SI1(I) = 0.0
         SI2(I) = 0.0
         SI3(I) = 0.0
         SI4(I) = 0.0
      END DO

      RETURN
      END
