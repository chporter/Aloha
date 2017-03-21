C=======================================================================
C  WATBAL, Subroutine, J.T. Ritchie
C  Calculates water balance components.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/89 GH  Written
C  10/18/95 GH  Standardized for CERES and CROPGRO applications.
C  04/03/96 GH  Split to handle different hydrology options
C  04/03/96 GH  Added erosion options
C  07/10/96 GH  Separated irrigation from precipiation prior to runoff
C               calculations
C-----------------------------------------------------------------------
C  Input  :
C  Output :
C  Local  :
C  Fn/Sub : WBFXD,WBINIT,WBSUM (all in file WBSUBS),RITCHIE
C
C=======================================================================

      SUBROUTINE WATBAL (TOTIR,YRDOY,TRUNOF,TDRAIN,NAP,IIRRI,ISWNIT,
     &           MEEVP,AIRAMT,DAP,YRSIM,YRPLT,MDATE,ISWWAT,EDATE,FDINT,
     &           CROP,ISWCHE,MEHYD)

      IMPLICIT  NONE

      INCLUDE  'GEN3.BLK'
      INCLUDE  'GEN4.BLK'
      INCLUDE  'NTRC2.BLK'

      CHARACTER IIRRI*1,ISWNIT*1,MEEVP*1,ISWWAT*1,ISWCHE*1,MEHYD*1
      CHARACTER CROP*2
      INTEGER   YRDOY,NAP,L,DAP,YRSIM,YRPLT,MDATE,EDATE,NL
      REAL      TOTIR,TRUNOF,TDRAIN,XHLAI
      REAL      PINF,WINF,AIRAMT,IRRAMT

      PARAMETER (NL=20)

      REAL      SMC(20,30),SWP(16)
      REAL      SWCON1,SWCON3,SWDF1,SWDF2,SWFCAB(5)
      REAL      DRN(NL),SWCN2(NL),SWI(NL),DRG(NL),RUNOFF
      REAL      FDINT,RATTP

      SAVE      DRN

C-----------------------------------------------------------------------
C     Initialize model specific variables (CERES)
C-----------------------------------------------------------------------

      XHLAI = LAI

C-----------------------------------------------------------------------
C     Initialize fixed variables for water balance (once only).
C-----------------------------------------------------------------------

      IF (YRDOY .EQ. YRSIM) THEN
         CALL WBFXD (LL,NLAYR,DSOIL,MU,SWCN2,SWCON1,SWCON3,
     &               SWDEF,SWFCAB,DLAYR)
         IF (ISWWAT .EQ. 'Y') THEN
            CALL HTHETA (NLAYR,LL,DUL,SAT,SMC,SWP)
         ENDIF
C-AD     NSCT = 16
C-AD     NDWT = NLAYR
C-AD     DWT(1) = DLAYR(1)
C-AD     SFX(1) = 0.0
C-AD     DO L = 2,NDWT
C-AD        DWT(L) = DWT(L-1)+DLAYR(L)
C-AD        SFX(L) = 0.0
C-----------------------------------------------------------------------
C        SFX is upward flux (cm/hr); need to find a procedure to
C        to calculate it
C-----------------------------------------------------------------------
C-AD     END DO
         DO L = 1,NLAYR
           SLKS(L) = SWCN(L)
         END DO
C-----------------------------------------------------------------------
C        Assume that laterial hydraulic conductivity is the same as
C        saturated hydraulic conductivity
C-----------------------------------------------------------------------
C-AD     KSIMP = 0.0
C-----------------------------------------------------------------------
C        Hydraulic conductivity of impermeable depth (cm/hr)
C-----------------------------------------------------------------------
      ENDIF

C-----------------------------------------------------------------------
C     Initialize variables for water balance (daily).
C-----------------------------------------------------------------------

      CALL WBINIT (NLAYR,MEEVP,DRN,EO,EP,ES,FLOW,L,PINF,RUNOFF,
     &             SWDF1,SWDF2,SWFAC,TURFAC,WINF,IRRAMT)

      IF (ISWWAT .EQ. 'Y') THEN

        IF (INDEX('AFRDPW',IIRRI) .GT. 0) THEN
           CALL IRRIG (IIRRI,YRDOY,AMIR,JULAPL,NAP,EFFIRR,YRPLT,MDATE,
     &                 ATHETA,THETAC,SWDEF,TOTIR,IRRAMT,AIRAMT,NAPW,DAP)
         ELSE
           IRRAMT = 0.0
        ENDIF
        !
        ! Compute PRECIP = irrigation amount + RAIN.
        !
        PRECIP = IRRAMT + RAIN
        !
        ! Convert snowfall into precip ( = rain)
        !
        IF (TMAX .LE. 1.0 .OR. SNOW .NE. 0.0) THEN
           CALL SNOWFALL (TMAX,RAIN,SNOW,PRECIP)
        ENDIF

C-----------------------------------------------------------------------
C       Option to run different hydrology simulations
C-----------------------------------------------------------------------

        IF (MEHYD .EQ. 'R') THEN
            DO L = 1, NLAYR
               SWI(L) = SW(L)
            ENDDO

            CALL RITCHIE (MEEVP,ISWNIT,DRN,PINF,WINF,RUNOFF,SWCN2,
     &           DRG,IRRAMT,SWCON1,SWCON3,XHLAI,FDINT,CROP,
     &           SWDF1,SWDF2,YRDOY,EDATE)

            IF (ISWCHE .EQ. 'Y') THEN
C-AD
C-AD          CALL EROSION(YRSIM,YRDOY,PRECIP,RUNOFF,SEDM,
C-AD &             ENRICH,AREA,SLOPE,SLEN,FLWR,CLAY(1),SILT(1),
C-AD &             OC(1),SSCL,TRFAC,STFAC,PRFAC,CFAC,NFCT)
C-AD
C-AD          CALL PESTCAL(NLAYR,YRSIM,YRDOY,PRECIP,RUNOFF,
C-AD &             SEDM,ENRICH,SWI,DRG,FLOW,RWU,ES,
C-AD &             BD,DLAYR,OC,SAT)

            ENDIF

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     Call ADAPT hydrology
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-AD    ELSE IF (MEHYD .EQ. 'A') THEN

C-AD       IF ( DAS .GE. NVEG0) IGROW = .TRUE.
C-AD       CALL ADAPTHYD(CROP,YRSIM,YRDOY,ISWNIT,ISWCHE,MEEVP,IGROW,
C-AD &           NLAYR,DLAYR,LL,DUL,SAT,SW,SWCN,WR,OC,CLAY,SILT,BD,PH,
C-AD &           CACO,TOTBAS,SLKS,TOTN,TOTP,ORGP,SSCL,CN2,WTDEP,SWCON,
C-AD &           XHLAI,PRECIP,SRAD,RHUM,WINDSP,TMAX,TMIN,TAVG,
C-AD &           ST,CLOUDS,SALB,TDEW,WINDHT,EO,ES,EP,ET,
C-AD &           RLV,RWUMX,SWCON1,SWCON3,RWU,TRWU,RWUEP1,SWDF1,
C-AD &           SWDF2,SWCN2,SWFAC,TURFAC,CO2,FDINT,DEPIR,
C-AD &           NDMTOT,TOPWT,WTNCAN,FON,SEDM,RUNOFF,DRAIN,
C-AD &           WRESR,ANO3,ANH4,NO3,NH4,SNO3,SNH4,ANFER,
C-AD &           DFERT,RESIDUE,RESN,DEPRES,FDAY,RESDAY,
C-AD &           NFERT,NRESAP,FERCOD,RESCOD,TLCH,TRON,
C-AD &           NTBL,JULWTB,WTABL,ELEV,SLOPE,AREA,SLEN,FLWR,PORMIN,
C-AD &           XELEV)
C-AD
        ENDIF

C-----------------------------------------------------------------------
C       Perform daily summation of water balance variables.
C-----------------------------------------------------------------------

        CALL WBSUM (DLAYR,DRAIN,DSOIL,DUL,EP,ES,LL,NLAYR,RAIN,
     &              RUNOFF,SW,SWDF1,TLL,ATHETA,CES,CEP,CET,CRAIN,ET,
     &              PESW,RATTP,SWDEF,SWFCAB,TDRAIN,TRUNOF,TSW,CEO,EO,
     &              SEDM,TSEDM)

C-----------------------------------------------------------------------
C     Return without performing the water balance if ISWWAT = 'N'
C-----------------------------------------------------------------------

      ELSE
        SWDEF = 0.0
        RATTP = 1.0
      ENDIF

      CSD1   = CSD1 + 1.0 - SWFAC
      CSD2   = CSD2 + 1.0 - TURFAC
      ICSDUR = ICSDUR + 1

      RETURN
      END

C=======================================================================
C  RITCHIE, Subroutine, J.T. Ritchie
C  Calculates water balance components.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/89 JR  Written
C  12/05/93 NBP Made into subroutines.
C  12/10/93 NBP Moved PHOTO and ETPHOT into WATBAL.
C  12/12/93 NBP Moved all subroutines to file WBSUBS incl. SOILEV.
C  12/12/93 NBP Moved ROOTWU, PHOTO, ETPHOT up, included ISWWAT logic.
C  01/27/94 NBP Added TCAN, TCAVG to WATBAL, ETPHOT args. and REAL.
C  03/11/94 NBP Moved ETPHOT to CROPGRO and PHOTO back to PLANT.
C  04/21/94 NBP Removed ETPHOT and PHOTO variables and ET from WBINIT.
C  04/29/95 NBP Changed FLOW to array in UPFLOW.  Passed back to NFLUX.
C  10/18/95 GH  Standardized for CERES and CROPGRO applications.
C  04/02/96 GH  Split into Ritchie hydrology
C  07/10/96 GH  Modified Runoff routine for rain versus rain+irrigation
C  2-JUL-97 BDB Changed call statement to RNOFF routine
C-----------------------------------------------------------------------
C  Input  :
C  Output :
C  Local  :
C  Fn/Sub : WBFXD,WBINIT,RNOFF,INFIL,SATFLO,ROOTWU,SOILEV,ESUP,TRANS,
C           SWFACS,UPFLOW,XTRACT,WBSUM (all in file WBSUBS)
C=======================================================================

      SUBROUTINE RITCHIE (MEEVP,ISWNIT,DRN,PINF,WINF,RUNOFF,SWCN2,
     &                    DRG,IRRAMT,SWCON1,SWCON3,XHLAI,FDINT,
     &                    CROP,SWDF1,SWDF2,YRDOY,EDATE)

      IMPLICIT    NONE

      INCLUDE    'GEN3.BLK'
      INCLUDE    'GEN4.BLK'
      INCLUDE    'NTRC2.BLK'

      CHARACTER   ISWNIT*1,MEEVP*1,CROP*2
      INTEGER     NL,I,YRDOY,EDATE

      PARAMETER  (NL=20)

      REAL        PINF,RUNOFF,WINF,PE,EOS,IRRAMT
      REAL        DRN(NL),SWCN2(NL),DRG(NL)
      REAL        SWDF1,SWCON3,SWDF2,FDINT,XHLAI,SWCON1

C-----------------------------------------------------------------------
C     Calculate runoff by Williams-SCS curve number (CN) technique.
C-----------------------------------------------------------------------

      IF (RAIN .GT. 0.0001) THEN
         CALL RNOFF (LL,RAIN,SAT,SW,PINF,RUNOFF,WINF,SMX)
      ENDIF

C-----------------------------------------------------------------------
C     Add irrigation to rainfall available for infiltration
C-----------------------------------------------------------------------

      IF (IRRAMT .GT. 0.0001) THEN
         WINF = WINF + IRRAMT
C-----------------------------------------------------------------------
C        Potential precipitation for infilitration (mm)
C-----------------------------------------------------------------------
         PINF = PINF + IRRAMT * 0.1
C-----------------------------------------------------------------------
C        Potential precipitation for infilitration (cm)
C-----------------------------------------------------------------------
      ENDIF

C-----------------------------------------------------------------------
C     Calculate infiltration rate on days with rain or irrigation OR
C     saturated flow on days with no rain or irrigation.
C-----------------------------------------------------------------------

      IF (PINF .GT. 0.0001) THEN
         CALL INFIL (DLAYR,DUL,NLAYR,SAT,SWCN,SWCON,DRAIN,DRN,
     &               PINF,RUNOFF,SW,IDRSW)
      ELSE
         CALL SATFLO (DLAYR,DUL,NLAYR,SAT,SWCN,SWCON,DRAIN,DRN,SW,IDRSW)
      ENDIF

      DO I = 1, NLAYR
        DRG(I) = DRN(I)
      END DO

C-----------------------------------------------------------------------
C     Compute nitrate and urea movement when any layer drains
C      (nitrate first, then urea, if present).
C-----------------------------------------------------------------------

      IF (ISWNIT .EQ. 'Y' .AND. IDRSW .EQ. 1) THEN
        CALL NFLUX (0,2,SNO3,NO3,FAC,MU,DRN,FLOW,TLCH,
     &              NLAYR,DLAYR,SW,BD,DUL,ADCOEF)
         IF (IUON) THEN
            CALL NFLUX (0,1,UREA,UPPM,FAC,MU,DRN,FLOW,TLCH,
     &                  NLAYR,DLAYR,SW,BD,DUL,ADCOEF)
         ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     Calculate potential root water uptake rate for each soil layer
C     and total potential water uptake rate.
C-----------------------------------------------------------------------

      IF (YRDOY .GT. EDATE) THEN
         CALL ROOTWU (DLAYR,LL,NLAYR,RLV,RWUMX,SW,SWCON1,SWCN2,
     &                SWCON3,RWU,TRWU,SAT,PORMIN,SATFAC)
      ENDIF

C-----------------------------------------------------------------------
C     Calculate FAO-Penman potential evapotranspiration (P) OR
C     Priestly-Taylor potential evapotranspiration (R).
C-----------------------------------------------------------------------

      IF (MEEVP .EQ. 'P' .OR. MEEVP .EQ. 'R') THEN
        IF (MEEVP .EQ. 'P') THEN
            CALL PETPEN (CLOUDS,SALB,SRAD,TAVG,TDEW,TMAX,TMIN,WINDHT,
     &                   WINDSP,XHLAI,EO)
         ELSE
            CALL PETPT (SALB,SRAD,TMAX,TMIN,XHLAI,EO)
        ENDIF

C-----------------------------------------------------------------------
C      Calculate potential soil evaporation
C-----------------------------------------------------------------------

        EOS = PE(EO,XHLAI)

C-----------------------------------------------------------------------
C      Calculate actual soil evaporation
C-----------------------------------------------------------------------

        CALL SOILEV (DLAYR,EOS,LL,SW,SWEF,U,WINF,SUMES1,SUMES2,T,ES)

C-----------------------------------------------------------------------
C      Calculate potential transpiration
C-----------------------------------------------------------------------

        IF (YRDOY .GT. EDATE) THEN
          CALL TRANS(CO2,EO,ES,FDINT,TAVG,WINDSP,XHLAI,EP,CROP)
        ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     Calculate daily SW stress factors.
C-----------------------------------------------------------------------

      IF (YRDOY .GT. EDATE) THEN
         CALL SWFACS (EP,TRWU,RWUEP1,SWDF1,SWDF2,SWFAC,TURFAC)
      ENDIF

C-----------------------------------------------------------------------
C     Calculate upward movement of water due to evaporation and
C     root extraction for each soil layer.
C-----------------------------------------------------------------------

      CALL UPFLOW (DLAYR,DUL,ES,ESW,LL,NLAYR,SAT,FLOW,SW)

      IF (ISWNIT .EQ. 'Y') THEN
        CALL NFLUX (1,2,SNO3,NO3,FAC,MU,DRN,FLOW,TLCH,
     &              NLAYR,DLAYR,SW,BD,DUL,ADCOEF)
        IF (IUON) THEN
           CALL NFLUX (1,1,UREA,UPPM,FAC,MU,DRN,FLOW,TLCH,
     &                 NLAYR,DLAYR,SW,BD,DUL,ADCOEF)
           ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     Calculate actual soil water uptake and transpiration rates
C-----------------------------------------------------------------------

      CALL XTRACT(DLAYR,EP,LL,NLAYR,TRWU,RWU,SW)

C-----------------------------------------------------------------------
C     Calculate soil water table depth
C-----------------------------------------------------------------------

      CALL WTDEPT (NLAYR,SW,SAT,WTDEP,DS,DLAYR,PORMIN)

      RETURN
      END
