C=======================================================================
C  TRATIO, Real Function, J.W. Jones
C  Calculates relative transpiration rate (CO2=330 vpm gives 1.0).
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/?? JWJ Written as subroutine ETRATIO.
C  12/05/93 NBP Changed to TRATIO function.  Changed VPDF to VPSLOP.
C  10/18/95 GH  Modified for multiple species.
C-----------------------------------------------------------------------
C  Input  : XHLAI,TAVG,WINDSP,CO2
C  Output : TRATIO
C  Local  :
C  Fn/Sub : BLRRES, VPDF
C=======================================================================

      FUNCTION TRATIO(XHLAI,TAVG,WINDSP,CO2,CROP)

      IMPLICIT NONE

      CHARACTER*2 CROP

      REAL CHIGHT,CO2,DELTA,GAMMA,LAIMAX,LHV,RA,RB,RATIO,RC1,RC2,RL,
     &  RLC,RLF,RLFC,RS1,RS2,TAVG,UAVG,VPSLOP,WINDSP,XDEN,XHLAI,XNUM,
     &  TRATIO

      IF (XHLAI .LT. 0.01) THEN
        TRATIO = 1.0
        RETURN
      ENDIF

C     Initialize.

      UAVG = WINDSP / 86.4
      RB     = 10
      LAIMAX = 3.5

      IF (INDEX('MZMLSG',CROP) .GT. 0) THEN
        CHIGHT =  1.5
      ELSE IF (INDEX('WHBA',CROP) .GT. 0) THEN
        CHIGHT = 1.0
      ELSE
        CHIGHT = 0.5
      ENDIF

C-----------------------------------------------------------------------
C     RLF = Leaf stomatal resistance at 330.0 ppm CO2, s/m
C     RLFC = Leaf stomatal resistance at other CO2 conc., s/m
C     (Allen, 1986), Plant responses to rising CO2.
C     CO2    = CO2    Conc of the increased ATM case
C     CO2    = 330    Ambient CO2
C-----------------------------------------------------------------------
      IF (INDEX('MZMLSG',CROP) .GT. 0) THEN
C     C-4 Crops
C        EQ 7 from Allen (1986) for corn.

        RLF    = (1.0/(0.0328-5.49E-5*330.0 + 2.96E-8 * 330.0**2))+RB
        RLFC   = (1.0/(0.0328-5.49E-5*CO2 + 2.96E-8*CO2 **2))+RB
      ELSE
C     C-3 Crops
        RLF = 9.72 + 0.0757 * 330.0 + 10.0
        RLFC = 9.72 + 0.0757 * CO2 + 10.0
      ENDIF

C     Compute canopy resistances, RL and RLC.

      RL = RLF / XHLAI
      RLC = RLFC / XHLAI

C     Compute boundary layer resistance (Jagtap and Jones, 1990).

      CALL BLRRES(CHIGHT,UAVG,RS1,RS2,RC1,RC2)

      RATIO = XHLAI/LAIMAX
      IF (RATIO .GT. 1.0) RATIO=1.0
      RA = RC1+(RC2-RC1)*RATIO

C     Compute delta and gamma.

      DELTA = VPSLOP(TAVG) / 100.0
      LHV    = 2500.9 - 2.345*TAVG
      GAMMA  = 1013.0*1.005/(LHV*0.622)

      XNUM = DELTA + GAMMA*(1.0+RL/RA)
      XDEN = DELTA + GAMMA*(1.0+RLC/RA)
      TRATIO = XNUM / XDEN

      END

C-----------------------------------------------------------------------

      SUBROUTINE BLRRES(CHIGHT,UAVG,RS1,RS2,RC1,RC2)

      IMPLICIT NONE
      REAL CHIGHT,D,E1,E2,E3,E4,E5,E6,EN,RC1,RC2,RS1,RS2,UAVG,X,Z0,Z1

      EN = 3.0
      Z1 = 0.01
      X = 2.0
      D = 0.7 * CHIGHT**0.979
      Z0 = 0.13 * CHIGHT**0.997
      E1 = EXP(EN*(1.-(D+Z0)/CHIGHT))
      E2 = EXP(EN)
      E3 = CHIGHT/(EN*(CHIGHT-D))
      E4 = ALOG((X-D)/Z0)
      E5 = 0.4*0.4*UAVG
      RS2 = E4*E3*(E2-E1)/E5
      E6 = ALOG((X-D)/(CHIGHT-D))
      RC2 = E4*(E6+E3*(E1-1))/E5
      RS1 = ALOG(X/Z1)*ALOG((D+Z0)/Z1)/E5
      RC1 = (ALOG(X/Z1)**2)/E5
      RC1 = RC1-RS1

      END
