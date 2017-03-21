C=======================================================================
C  WBSUBS, File of subroutines, N.B. Pickering
C  Contains all WATBAL subroutines
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  12/05/93 NBP Written
C  04/29/95 NBP Changed FLOW to array in UPFLOW.
C=======================================================================


C=======================================================================
C  WBFXD, Subroutine, J.T. Ritchie
C  Initializes fixed variables for water balance (once only).
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/89 JR  Written
C  12/05/93 NBP Made into subroutine
C-----------------------------------------------------------------------
C  Input  : LL,NLAYR
C  Output : DSOIL,MU,SWCN2,SWCON1,SWCON3,SWDEF,SWDF1,SWDF2,SWFCAB
C  Local  : I,NL
C=======================================================================

      SUBROUTINE WBFXD(LL,NLAYR,DSOIL,MU,SWCN2,SWCON1,SWCON3,
     &  SWDEF,SWFCAB,DLAYR)

      IMPLICIT NONE
      INTEGER I,NL,NLAYR,MU
      PARAMETER (NL=20)
      REAL DSOIL,LL(NL),SWCN2(NL),SWCON1,SWCON3,SWDEF,
     &  SWFCAB(5),DLAYR(NL)

C     Initialize.

      MU = NLAYR
      IF (DSOIL .LE. 0.0) DSOIL =
     &    (DLAYR(1) + DLAYR(2) + DLAYR(3))
      SWDEF = 0.0
      DO 10 I = 1,5
        SWFCAB(I) = 1.0
   10 CONTINUE

C     Compute SWCN2 for each soil layer.  Adjust SWCN2 for extremely
C     high LL to avoid water uptake limitations.

      DO 20 I = 1,NL
        SWCN2(I) = 120 - 250 * LL(I)
        IF (LL(I) .GT. 0.30) SWCN2(I) = 45.0
   20 CONTINUE

C     Set SWCON1 and SWCON3.

      SWCON1 = 1.32E-3
      SWCON3 = 7.01

      END

C=======================================================================
C  WBINIT, Subroutine, J.T. Ritchie
C  Initializes variables for water balance (daily).
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/89 JR  Written
C  12/05/93 NBP Made into subroutine
C  02/12/93 NBP Added code to avoid resetting ET parms. for MEEVP='Z'
C  04/21/94 NBP Removed ET variable.
C-----------------------------------------------------------------------
C  Input  : NLAYR,MEEVP
C  Output : DRN,EO,EP,ES,FLOW,L,PINF,RUNOFF,SWDF1,SWDF2,
C           SWFAC,TURFAC,WINF
C  Local  : L,NL
C=======================================================================

      SUBROUTINE WBINIT(NLAYR,MEEVP,DRN,EO,EP,ES,FLOW,L,PINF,RUNOFF,
     &  SWDF1,SWDF2,SWFAC,TURFAC,WINF,IRRAMT)

      IMPLICIT NONE
      CHARACTER MEEVP*1
      INTEGER L,NL,NLAYR
      PARAMETER (NL=20)
      REAL DRN(NL),EO,EP,ES,FLOW(NL),PINF,RUNOFF,SWDF1,SWDF2,
     &  SWFAC,TURFAC,WINF,IRRAMT

      IF (MEEVP .NE. 'Z') THEN
        ES = 0.0
        EP = 0.0
      ENDIF
      EO = 0.0
      SWDF1 = 1.0
      SWDF2 = 1.0
      SWFAC = 1.0
      TURFAC = 1.0
      PINF = 0.0
      RUNOFF = 0.0
      WINF = 0.0
      IRRAMT = 0.0
      DO 10 L = 1,NLAYR
        DRN(L) = 0.0
        FLOW(L) = 0.0
   10 CONTINUE

      END

C=======================================================================
C  ROOTWU, Subroutine, J.T. Ritchie
C  Calculates root water uptake rate for each soil layer and total rate.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/89 JR  Written
C  12/05/93 NBP Made into subroutine.
C  01/18/96 JWJ Added flooding effect on water uptake
C  01/06/96 GH  Added soil water excess stress
C-----------------------------------------------------------------------
C  Input  : DLAYR,LL,NLAYR,RLV,RWUMX,SW,SWCON1,SWCN2,SWCON3
C  Output : RWU,TRWU
C  Local  : L,NL
C=======================================================================

      SUBROUTINE ROOTWU(DLAYR,LL,NLAYR,RLV,RWUMX,SW,SWCON1,SWCN2,
     &  SWCON3,RWU,TRWU,SAT,PORMIN,SATFAC)

      IMPLICIT NONE
      INTEGER L,NL,NLAYR
      PARAMETER (NL=20)
      REAL DLAYR(NL),LL(NL),RLV(NL),RWUMX,SW(NL),SWCON1,SWCN2(NL)
      REAL SWCON3,RWU(NL),TRWU,SAT(NL),SATFAC,SUMEX,SUMRL,SWEXF,PORMIN

      TRWU   = 0.0
      SUMEX  = 0.0
      SUMRL  = 0.0

      DO L = 1,NLAYR
        IF (RLV(L) .LE. 0.00001 .OR. SW(L) .LE. LL(L)) THEN
          RWU(L) = 0.
        ELSE
          RWU(L) = SWCON1*EXP(MIN((SWCN2(L)*(SW(L)-LL(L))),40.))/
     &      (SWCON3-ALOG(RLV(L)))

C-----------------------------------------------------------------------
C     PORMIN = MINIMUM PORE SPACE  REQUIRED FOR SUPPLYING OXYGEN TO ROOTS
C              FOR OPTIMAL GROWTH AND FUNCTION
C-----------------------------------------------------------------------
          SWEXF = 1.0
          IF (SAT(L)-SW(L) .LT. PORMIN) THEN
             SWEXF = (SAT(L)-SW(L))/PORMIN
          ENDIF
          SWEXF = MIN(SWEXF,1.0)
          RWU(L) = MIN(RWU(L),RWUMX*SWEXF)
          RWU(L) = MIN(RWU(L),RWUMX)
        ENDIF
        RWU(L) = RWU(L)*DLAYR(L)*RLV(L)
        TRWU = TRWU+RWU(L)
        SUMEX = SUMEX + DLAYR(L)*RLV(L)*(1.0 - SWEXF)
        SUMRL = SUMRL + DLAYR(L)*RLV(L)
      ENDDO
C-----------------------------------------------------------------------
C SATFAC = Root length weighted soil water excess stress factor
C          ( 0 = no stress; 1.0 = saturated stress )
C SUMEX  = Sum over all layers of water excess factor times depth
C          times root length density
C SUMRL  = Sum of root length density (integral over depth)
C-----------------------------------------------------------------------
      IF (SUMRL .GT. 0.0) THEN
         SATFAC = SUMEX/SUMRL
      ELSE
         SATFAC = 0.0
      ENDIF
      END


C=======================================================================
C  RNOFF, Subroutine, J.T. Ritchie
C  Calculate runoff by Williams-SCS curve number (CN) technique.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/89 JR  Written
C  10/01/87 GH  Correction of RUNOFF calc. according to SCS curve no.
C  12/05/93 NBP Made into subroutine
C  07/12/96 GH  Changed Precipitation to RAIN
C  07/01/97 BDB Simplified CN calculations (Removed C1, C2, C3, WF, DUL
C               and made SMX input variables)
C-----------------------------------------------------------------------
C  Input  : CN,DUL,LL,RAIN,SAT,SW,SMX
C  Output : PINF,RUNOFF,WINF
C  Local  : PB,SWABI
C
C SWABI--A soil water abstraction index, a untiless indicator of the soil
C water condition at the time of a rainfall event.  This affects the
C intercept of the runoff axis when runoff starts to occur--later when drier
C and sooner when wetter.
C
C PB --A parameter that, when modified by SWABI,  determines the threshold
C amount of rainfall that will occur before runoff starts (mm/day)
C
C SWX--A parameter with the units of rainfall that influences the slope of
C the runoff-rainfall curve.  It's value is influenced only by the input
C runoff curve number (mm/day).
C
C=======================================================================

      SUBROUTINE RNOFF(LL,RAIN,SAT,SW,PINF,RUNOFF,WINF,SMX)

      IMPLICIT NONE
      INTEGER NL
      PARAMETER (NL=20)
      REAL LL(NL),PINF,RAIN,RUNOFF,SAT(NL),SMX,SW(NL),WINF
      REAL PB,SWABI

C     Runoff is related to the average soil water content of the top
C     two layers of soil

      SWABI = 0.15 * ((SAT(1) - SW(1)) / (SAT(1) - LL(1) * 0.5) +
     1                (SAT(2) - SW(2)) / (SAT(2) - LL(2) * 0.5))
      PB = RAIN - SWABI * SMX
      IF (PB .GT. 0) THEN
         RUNOFF = PB**2/(RAIN + (1.0-SWABI) * SMX)
      ELSE
         RUNOFF = 0.0
      END IF
      WINF = RAIN - RUNOFF
      PINF = WINF * 0.1

      END

C=======================================================================
C  INFIL, Subroutine, J.T. Ritchie
C  Calculates infiltration rate.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/89 JR  Written
C  01/11/87 GH  Infiltration distr. according to capacity of each horz.
C  12/05/93 NBP Made into subroutine
C-----------------------------------------------------------------------
C  Input  : DLAYR,DUL,NLAYR,SAT,SWCN,SWCON
C  In/Out : DRAIN,DRN,PINF,RUNOFF,SW
C  Output : IDRSW
C  Local  : L,LK,NL,EXCS,HOLD
C  Notes  : Infiltration is distributed according to the holding
C           capacity of each horizon (SAT(L)-DUL(L)).  If the soil
C           water content of a layer reaches saturation, the excess
C           amount of infiltration is added to the next soil layer(s),
C           depending on their water holding capacity and actual soil
C           water content.  Then saturated flow is calculated.
C=======================================================================

      SUBROUTINE INFIL(DLAYR,DUL,NLAYR,SAT,SWCN,SWCON,DRAIN,DRN,
     &  PINF,RUNOFF,SW,IDRSW)

      IMPLICIT NONE
      INTEGER IDRSW,L,LK,NL,NLAYR
      PARAMETER (NL=20)
      REAL DLAYR(NL),DRAIN,DRN(NL),DUL(NL),EXCS,HOLD,PINF,RUNOFF,
     &  SAT(NL),SW(NL),SWCN(NL),SWCON

      IDRSW = 1
      DO 700 L = 1,NLAYR
        HOLD = (SAT(L)-SW(L))*DLAYR(L)
        IF (PINF .GT. 0.0 .AND. PINF .GT. HOLD) THEN
          DRAIN = SWCON*(SAT(L)-DUL(L))*DLAYR(L)
          DRN(L) = PINF-HOLD+DRAIN
          IF (SWCN(L) .GT. 0.0 .AND. DRN(L) .GT. SWCN(L)*24.) THEN
            DRN(L) = SWCN(L)*24.
            DRAIN = DRN(L) + HOLD - PINF
          ENDIF
          SW(L) = SW(L) + (PINF-DRN(L))/DLAYR(L)
          IF (SW(L) .GT. SAT(L)) THEN
            EXCS = (SW(L)-SAT(L)) * DLAYR(L)
            SW(L) = SAT(L)
C           If there is excess water, redistribute it in layers above.
            IF (L .GT. 1) THEN
              DO 750 LK = L-1,1,-1
                IF (EXCS .LT. 0.0001) GOTO 760
                HOLD = MIN((SAT(LK)-SW(LK))*DLAYR(LK),EXCS)
                SW(LK) = SW(LK) + HOLD/DLAYR(LK)
                DRN(LK) = MAX((DRN(LK)-EXCS),0.0)
                EXCS = EXCS - HOLD
 750          CONTINUE
            ENDIF
            RUNOFF = RUNOFF + EXCS*10.0
 760        CONTINUE
          ENDIF
          PINF = DRN(L)
        ELSE
          SW(L) = SW(L)+PINF/DLAYR(L)
          IF (SW(L) .GE. DUL(L)+0.003) THEN
            DRAIN = (SW(L)-DUL(L))*SWCON*DLAYR(L)
            DRN(L) = DRAIN
            IF (SWCN(L) .GT. 0.0 .AND. DRN(L) .GT. SWCN(L)*24.0) THEN
               DRN(L) = SWCN(L)*24.0
               DRAIN = DRN(L)
               ENDIF
            SW(L) = SW(L)-DRAIN/DLAYR(L)
            PINF = DRAIN
          ELSE
            PINF = 0.0
            DRN(L) = 0.0
          ENDIF
        ENDIF
  700 CONTINUE
      DRAIN = PINF * 10.0

      END

C=======================================================================
C  SATFLO, Subroutine, J.T. Ritchie
C  Calculates saturated flow on days with no rain or irrigation.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/89 JR  Written
C  11/01/87 GH
C  12/05/93 NBP Made into subroutine
C-----------------------------------------------------------------------
C  Input  : DLAYR,DUL,NLAYR,SAT,SWCN,SWCON
C  In/Out : DRAIN,DRN,SW
C  Output : IDRSW
C  Local  : L,NL,DRMX,HOLD
C  Notes  : Saturated flow is calculated for days with no irr or rain.
C           Drainage is reduced when the flux exceeds the rate allowed
C           by the saturated soil hydraulic conductivity, assuming unit
C           gradient.  This allows for perched water tables in profile.
C           Prevents flux from exceeding the most limiting layer below
C           it.  If the sat. hyd. cond values are missing (neg) assume
C           no perching of water table.
C=======================================================================

      SUBROUTINE SATFLO(DLAYR,DUL,NLAYR,SAT,SWCN,SWCON,DRAIN,DRN,SW,
     &  IDRSW)

      IMPLICIT NONE
      INTEGER IDRSW,L,NL,NLAYR
      PARAMETER (NL=20)
      REAL DLAYR(NL),DRAIN,DRMX(NL),DRN(NL),DUL(NL),HOLD,SAT(NL),
     &  SW(NL),SWCN(NL),SWCON,SWOLD

      IDRSW = 0
      DO 900 L = 1,NLAYR
        DRN(L) = 0.0
        DRMX(L) = 0.0
        IF (SW(L) .GE. DUL(L)+0.003) THEN
          DRMX(L) = (SW(L)-DUL(L))*SWCON*DLAYR(L)
          DRMX(L) = MAX(0.0,DRMX(L))
        ENDIF
        IF (L .EQ. 1) THEN
          DRN(L) = DRMX(L)
        ELSE
          HOLD = 0.0
          IF(SW(L) .LT. DUL(L)) HOLD = (DUL(L) - SW(L))*DLAYR(L)
          DRN(L) = MAX(DRN(L-1) + DRMX(L) - HOLD,0.0)
        ENDIF
        IF (SWCN(L) .GT. 0.0 .AND. DRN(L) .GT. SWCN(L)*24.)
     &    DRN(L) = SWCN(L)*24.
        IF (DRN(L) .GT. 0.0) IDRSW = 1
  900 CONTINUE

C     Compute volumetric water contents after drainage in a day.
C     Prevent water content in any layer from exceeding saturation
C     as water drains down in the profile.

      DO 1000 L = NLAYR,2,-1
        SWOLD = SW(L)
        SW(L) = SW(L) +(DRN(L-1)-DRN(L))/DLAYR(L)
        IF (SW(L) .GT. SAT(L)) THEN
          DRN(L-1) = (SAT(L) - SWOLD)*DLAYR(L) +DRN(L)
          SW(L) = SAT(L)
        ENDIF
1000  CONTINUE
      SW(1) = SW(1) - DRN(1)/DLAYR(1)
      DRAIN = DRN(NLAYR) * 10.0

      END

C=======================================================================
C  SOILEV, Subroutine, J. Ritchie, B. Baer
C  Calculates actual soil evaporation (ES, mm/d).
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/89 JR  Written
C  08/15/93 BB  Revised into IF-THEN-ELSE structure
C  11/30/93 NBP Revised header and I/O order, removed ES1 from arg. list
C  12/10/93 NBP Changes to SW in UPFLOW, returns ES with SW unchanged.
C               Added variable AWEV1 to simplify limit on ES.
C  08/20/95 GH  Removed stage 1 limitations
C  07/12/96 GH  Rechecked; there are still some concept problems
C-----------------------------------------------------------------------
C  Input  : DLAYR,DUL,EOS,LL,SW,SWEF,U,WINF
C  In/Out : SUMES1,SUMES2,T
C  Output : ES
C=======================================================================

      SUBROUTINE SOILEV(DLAYR,EOS,LL,SW,SWEF,U,WINF,
     &  SUMES1,SUMES2,T,ES)

      IMPLICIT NONE
      INTEGER NL
      PARAMETER (NL=20)
      REAL AWEV1,DLAYR(20),EOS,ES,ESX,LL(20),SUMES1,
     &  SUMES2,SW(20),SWEF,T,U,WINF

C-----------------------------------------------------------------------
C  Adjust soil evaporation, and the sum of stage 1 (SUMES1) and stage 2
C    (SUMES2) evaporation based on infiltration (WINF), potential
C    soil evaporation (EOS), and stage 1 evaporation (U).
C-----------------------------------------------------------------------
      IF ((SUMES1 .GE. U) .AND. (WINF .GE. SUMES2)) THEN
        WINF = WINF-SUMES2
        SUMES1 = U-WINF
        SUMES2 = 0
        T = 0.0
        IF (WINF .GT. U) SUMES1 = 0.0
        CALL ESUP(EOS,SUMES1,SUMES2,U,ES,T)
      ELSE IF ((SUMES1 .GE. U) .AND. (WINF .LT. SUMES2)) THEN
        T = T+1.0
        ES = 3.5*T**0.5-SUMES2
        IF (WINF .GT. 0.0) THEN
          ESX = 0.8*WINF
          IF (ESX .LE. ES) ESX = ES+WINF
          IF (ESX .GT. EOS) ESX = EOS
          ES = ESX
        ELSE IF (ES .GT. EOS) THEN
          ES = EOS
        ENDIF
        SUMES2 = SUMES2+ES-WINF
        T = (SUMES2/3.5)**2
      ELSE IF (WINF.GE.SUMES1) THEN
        SUMES1 = 0.0
        CALL ESUP(EOS,SUMES1,SUMES2,U,ES,T)
      ELSE
        SUMES1=SUMES1-WINF
        CALL ESUP(EOS,SUMES1,SUMES2,U,ES,T)
      ENDIF


C-----------------------------------------------------------------------
C
C  Soil evaporation can not be larger than the current extractable soil
C    water in the top layer.
C    If available soil water is less than soil evaporation, adjust first
C    and second stage evaporation and soil evaporation accordingly
C
C-----------------------------------------------------------------------

      AWEV1 = (SW(1)-LL(1)*SWEF) * DLAYR(1) * 10.0
      AWEV1 = MAX(0.0,AWEV1)
      IF (AWEV1 .LT. ES) THEN
         IF (SUMES1 .GE. U .AND. SUMES2 .GT. ES) THEN
            SUMES2 = SUMES2 - ES + AWEV1

            T = (SUMES2/3.5)**2
            ES = AWEV1
         ELSE IF (SUMES1 .GE. U .AND. SUMES2 .LT. ES .AND.
     &            SUMES2 .GT. 0) THEN
            SUMES1 = SUMES1 - (ES - SUMES2)
            SUMES2 = MAX(SUMES1 + AWEV1 - U,0.0)
            SUMES1 = MIN(SUMES1 + AWEV1, U)
            T = (SUMES2/3.5)**2
            ES = AWEV1
         ELSE
            SUMES1 = SUMES1 - ES + AWEV1
            ES = AWEV1
         ENDIF
      ENDIF

      END

C=======================================================================
C  ESUP, Subroutine, J. Ritchie, B. Baer
C  Supplementary calculations for SOILEV
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/89 JR  Written
C  08/15/93 BB? Revised into IF-THEN-ELSE structure
C  11/30/93 NBP Revised header and I/O order
C  08/21/95 GH  Added calculations for SUMES2, second stage evaporation
C-----------------------------------------------------------------------
C  Input  : EOS,SUMES1,U
C  Output : ES,SUMES2,T
C=======================================================================

      SUBROUTINE ESUP(EOS,SUMES1,SUMES2,U,ES,T)

      IMPLICIT NONE
      REAL EOS,ES,SUMES1,U,SUMES2,T

C-----------------------------------------------------------------------
C  Calculate stage 1 soil evaporation
C    If the sum for stage 1 soil evaporation (SUMES1) is larger than
C    stage 1 evaporation (U), start stage 1 soil evaporation (SUMES2)
C    and adjust soil evaporation (ES)
C-----------------------------------------------------------------------
      SUMES1 = SUMES1 + EOS
      IF (SUMES1 .GT. U) THEN
        ES = EOS - 0.4*(SUMES1-U)
        SUMES2 = 0.6 * (SUMES1-U)
        T = (SUMES2/3.5)**2
        SUMES1 = U
      ELSE
        ES = EOS
      ENDIF

      END

C=======================================================================
C  TRANS, Subroutine, J.T. Ritchie
C  Calculates actual transpiration rate, EP (mm/d).
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/89 JR  Written
C  01/01/89 JWJ Modified for climate change using ETRATIO subroutine.
C  12/05/93 NBP Made into subroutine and changed to TRATIO function.
C-----------------------------------------------------------------------
C  Input  : CO2,EO,ES,FDINT,TAVG,WINDSP,XHLAI
C  Output : EP
C=======================================================================

      SUBROUTINE TRANS(CO2,EO,ES,FDINT,TAVG,WINDSP,XHLAI,EP,CROP)

      IMPLICIT NONE

      CHARACTER*2 CROP

      REAL CO2,EO,EP,ES,FDINT,TAVG,TRAT,TRATIO,WINDSP,XHLAI

      TRAT = TRATIO(XHLAI,TAVG,WINDSP,CO2,CROP)

      EP = EO*FDINT*TRAT
      IF (EP+ES .GT. EO*TRAT) EP = EO*TRAT - ES
      EP = MAX(EP,0.0)

      END

C=======================================================================
C  SWFACS, Subroutine, J.T. Ritchie
C  Calculates daily SW stress factors.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/89 JR  Written
C  12/05/93 NBP Made into subroutine
C-----------------------------------------------------------------------
C  Input  : EP,TRWU
C  Output : RWUEP1,SWDF1,SWDF2,SWFAC,TURFAC
C=======================================================================

      SUBROUTINE SWFACS(EP,TRWU,RWUEP1,SWDF1,SWDF2,SWFAC,TURFAC)

      IMPLICIT NONE
      REAL EP,EP1,RWUEP1,SWDF1,SWDF2,SWFAC,TRWU,TURFAC

      SWDF1 = 1.0
      SWDF2 = 1.0
      IF (EP .GT. 0.0) THEN
        EP1 = EP*0.1
        IF (TRWU/EP1 .LT. RWUEP1) SWDF2 = (1./RWUEP1)*TRWU/EP1
        IF (EP1 .GE. TRWU) THEN
          SWDF1 = TRWU / EP1
          EP = TRWU * 10.0
        ENDIF
      ENDIF
      SWFAC = SWDF1
      TURFAC = SWDF2

      END

C=======================================================================
C  UPFLOW, Subroutine, J.T. Ritchie
C  Calculates upward movement of water due to evaporation.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/89 JR  Written
C  12/05/93 NBP Made into subroutine.
C  04/29/95 NBP Changed FLOW to array in UPFLOW.  Passed back to NFLUX.
C-----------------------------------------------------------------------
C  Input  : DLAYR,DUL,ES,ESW,LL,NLAYR,SAT
C  In/Out : SW
C  Output : FLOW
C  Local  : DBAR,GRAD,IST,L,M,NL,SWOLD,THET1,THET2
C=======================================================================

      SUBROUTINE UPFLOW(DLAYR,DUL,ES,ESW,LL,NLAYR,SAT,FLOW,SW)

      IMPLICIT NONE
      INTEGER IST,L,M,NL,NLAYR
      PARAMETER (NL=20)
      REAL DBAR,DLAYR(NL),DUL(NL),ES,ESW(NL),FLOW(NL),GRAD,LL(NL),
     &  SAT(NL),SW(NL),SWOLD,THET1,THET2

      SW(1) = SW(1) - 0.1*ES/DLAYR(1)
      IF (DLAYR(1) .GE. 5.0) THEN
        IST = 1
      ELSE
        IST = 2
      ENDIF
      DO 10 L = IST,NLAYR-1
        M = L+1
        SWOLD = SW(L)
        THET1 = MIN(SW(L)-LL(L),ESW(L))
        THET1 = MAX(0.0,THET1)
        THET2 = MIN(SW(M)-LL(M),ESW(M))
        THET2 = MAX(0.0,THET2)
        DBAR = 0.88 * EXP(35.4*((THET1*DLAYR(L)+THET2*DLAYR(M))/
     +    (DLAYR(L)+DLAYR(M)))*0.5)
        DBAR = MIN(DBAR,100.0)
        GRAD = (THET2/ESW(M)-THET1/ESW(L)) * (ESW(M)*DLAYR(M)+ESW(L)*
     +    DLAYR(L)) / (DLAYR(M)+DLAYR(L))
        FLOW(L) = DBAR * GRAD / ((DLAYR(L)+DLAYR(M))*0.5)
        IF (FLOW(L) .GT. 0.0 .AND. SW(L) .LE. DUL(L)) THEN
          SW(L) = SW(L) + FLOW(L)/DLAYR(L)
          IF (SW(L) .GT. DUL(L)) THEN
            FLOW(L) = FLOW(L) - (SW(L)-DUL(L)) * DLAYR(L)
            SW(L) = DUL(L)
          ENDIF
          SW(M) = SW(M) - FLOW(L)/DLAYR(M)
          IF (SW(M) .LT. LL(M)) THEN
            FLOW(L) = FLOW(L) - (LL(M)-SW(M)) * DLAYR(M)
            SW(L) = SWOLD + FLOW(L)/DLAYR(L)
            SW(M) = LL(M)
          ENDIF
        ELSE IF (FLOW(L) .LE. 0.0 .AND. SW(L) .GE. LL(L)) THEN
          SW(L) = SW(L) + FLOW(L)/DLAYR(L)
          IF (SW(L) .LT. LL(L)) THEN
            FLOW(L) = FLOW(L) + (LL(L) - SW(L)) * DLAYR(L)
            SW(L) = LL(L)
          ENDIF
          SW(M) = SW(M) - FLOW(L)/DLAYR(M)
          IF (SW(M) .GT. SAT(M)) THEN
            FLOW(L) = FLOW(L) + (SW(M) - SAT(M)) * DLAYR(M)
            SW(L) = SWOLD + FLOW(L)/DLAYR(L)
            SW(M) = SAT(M)
          ENDIF
        ENDIF
   10 CONTINUE

      END

C=======================================================================
C  XTRACT, Subroutine, J.T. Ritchie
C  Calculates root extraction for each soil layer.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/89 JR  Written
C  12/05/93 NBP Made into subroutine
C  07/11/96 GH  Set TRWU and RWU to 0 if EP = 0
C-----------------------------------------------------------------------
C  Input  : DLAYR,EP,LL,NLAYR,TRWU
C  Output : RWU,SW
C  Local  : L,NL,WUF
C=======================================================================

      SUBROUTINE XTRACT(DLAYR,EP,LL,NLAYR,TRWU,RWU,SW)

      IMPLICIT NONE
      INTEGER L,NL,NLAYR
      PARAMETER (NL=20)
      REAL DLAYR(NL),EP,LL(NL),RWU(NL),SW(NL),TRWU,WUF

      IF (EP .GT. 0.0) THEN
        IF (0.1*EP .LE. TRWU) THEN
          WUF = 0.1*EP/TRWU
        ELSE
          WUF = 1.0
        ENDIF
        TRWU = 0.0
        DO L = 1,NLAYR
          IF (SW(L) .GT. LL(L)) THEN
            RWU(L) = RWU(L)*WUF
            SW(L) = SW(L)-RWU(L)/DLAYR(L)
            IF (SW(L) .LT. LL(L)) THEN
              RWU(L) = RWU(L) - (LL(L)-SW(L))*DLAYR(L)
              SW(L) = LL(L)
            ENDIF
          TRWU = TRWU + RWU(L)
          ENDIF
        END DO
        EP = TRWU * 10.
      ELSE
        TRWU = 0.0
        DO L = 1,NLAYR
          RWU(L) = 0.0
        ENDDO
      ENDIF
      END

C=======================================================================
C  WBSUM, Subroutine, J.T. Ritchie
C  Performs daily summation of water balance variables.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/89 JR  Written
C  12/05/93 NBP Made into subroutine
C-----------------------------------------------------------------------
C  Input  : DLAYR,DRAIN,DSOIL,DUL,EP,ES,LL,NLAYR,RAIN,RUNOFF,SW,SWDF1,
C           TLL
C  Output : ATHETA,CES,CEP,CET,CRAIN,ET,PESW,RATTP,SWFCAB,TDRAIN,
C           TRUNOF,TSW
C  Local  : CUMDEP,I,L,NL,NSWAB,TSWTOP,WET1,XDEP,XDEPL
C=======================================================================

      SUBROUTINE WBSUM(DLAYR,DRAIN,DSOIL,DUL,EP,ES,LL,NLAYR,RAIN,
     &  RUNOFF,SW,SWDF1,TLL,ATHETA,CES,CEP,CET,CRAIN,ET,PESW,
     &  RATTP,SWDEF,SWFCAB,TDRAIN,TRUNOF,TSW,CEO,EO,SEDM,TSEDM)

      IMPLICIT NONE

      INTEGER I,L,NL,NLAYR,NSWAB

      PARAMETER (NL=20)

      REAL ATHETA,CES,CEP,CET,CRAIN,CUMDEP,DLAYR(NL),DRAIN,DSOIL,
     &  DUL(NL),EP,ES,ET,LL(NL),PESW,RATTP,RAIN,RUNOFF,SW(NL),
     &  SWDEF,SWDF1,SWFCAB(5),TDRAIN,TRUNOF,TLL,TSW,TSWTOP,WET1,
     &  XDEP,XDEPL,CEO,EO,TSEDM,SEDM

C     Increment summation variables.

      ET = ES + EP
      CEP = CEP + EP
      CET = CET + ET
      CES = CES + ES
      CEO = CEO + EO
      CRAIN  = CRAIN + RAIN
      TDRAIN = TDRAIN + DRAIN
      TRUNOF = TRUNOF + RUNOFF
      TSEDM  = TSEDM + SEDM
      NSWAB = 5
      DO 10 I = NSWAB,2,-1
        SWFCAB(I) = SWFCAB(I-1)
   10 CONTINUE
      SWFCAB(1) = SWDF1
      RATTP = SWFCAB(NSWAB)
      WET1 = 0.0
      TSWTOP = 0.0
      CUMDEP = 0.0
      DO 20 L = 1,NLAYR
        IF (CUMDEP .LT. DSOIL) THEN
          XDEPL  = CUMDEP
          CUMDEP = CUMDEP + DLAYR(L)
          IF (CUMDEP .GT. DSOIL) THEN
            XDEP = (DSOIL-XDEPL)
          ELSE
            XDEP = DLAYR(L)
          ENDIF
          TSWTOP = TSWTOP + (SW(L)-LL(L))*XDEP
          WET1   = WET1 + (DUL(L)-LL(L))*XDEP
        ENDIF
   20 CONTINUE
      ATHETA = TSWTOP / WET1
      SWDEF  = MAX(0.0,WET1-TSWTOP)
      TSW = 0.0
      DO 30 L = 1,NLAYR
        TSW = TSW + SW(L)*DLAYR(L)
   30 CONTINUE
      PESW = TSW - TLL
      PESW = MAX(0.0,PESW)

      END
C=======================================================================
C  SNOWFALL, Subroutine
C
C  Determines snow melt
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  12/05/93 NBP Written
C  01/07/92 AG  Written
C  01/07/93 PWW Header revision and minor changes
C  02/10/93 PWW Took out common blocks -- pass paramaters
C  07/10/96 GH   Modified to add snow to rain
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : SNOMLT
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : WATBAL
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  SNOMLT  :
C  TMAX    :
C  RAIN    :
C  SNOW    :
C  PRECIP  :
C=======================================================================

      SUBROUTINE SNOWFALL (TMAX,RAIN,SNOW,PRECIP)

      IMPLICIT NONE

      REAL     TMAX,RAIN,SNOW,PRECIP
      REAL     SNOMLT

      SNOMLT = 0.0
      IF (TMAX  .GT. 1.0) THEN
         SNOMLT = TMAX + RAIN*0.4
         IF (SNOMLT .GT. SNOW) THEN
             SNOMLT = SNOW
         ENDIF
         SNOW   = SNOW   - SNOMLT
         PRECIP = PRECIP + SNOMLT
         RAIN   = RAIN + SNOMLT
       ELSE
         SNOW   = SNOW   + RAIN
         PRECIP = PRECIP - RAIN
         RAIN   = 0.0
      ENDIF

      END
C=======================================================================
C  WTDEPT, Subroutine
C
C  Determines water table depth
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/06/97 GH  Written
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : WATBAL
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C=======================================================================

      SUBROUTINE WTDEPT (NLAYR,SW,SAT,WTDEP,DS,DLAYR,PORMIN)

      IMPLICIT NONE

      INTEGER  NL,L,NLAYR

      PARAMETER (NL=20)

      REAL  WTDEP,PORMIN,SW(NL),SAT(NL),DS(NL),DLAYR(NL)

      WTDEP = DS(NLAYR)
      DO L = NLAYR,1,-1
         IF (SW(L) .GE. SAT(L)) THEN
            WTDEP = DS(L) - DLAYR(L)
         ENDIF
      ENDDO

      END
