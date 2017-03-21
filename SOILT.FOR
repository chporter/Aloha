C=======================================================================
C  SOILT, Subroutine
C
C  Determines soil temperature
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      2-9-93
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : K,L,XI,ALX,AW,WC,DD,TA,DT,ZD
C
C  OUTPUT : ST
C-----------------------------------------------------------------------
C  Called : SOILNI PINE
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  K,L    : Layer and loop counter
C  XI     : Non-integer Julian date
C  ALX    : Current Julian date as a radian fraction of 1 year for soil
C           temperature caculations
C  AW     : Available water used in soil temperature calculations (cm)
C  WC     : Moisture content affect on soil temperature
C  DD     : Soil temperature damping depth (mm)
C  TA     :
C  DT     : Difference between moving average soil surface temperature and
C           long-term daily average ambient temperature
C  ZD     : Variable used in the calculation of soil temperature
C=======================================================================

      SUBROUTINE SOILT (DOY,TMAX,TAVG,SRAD,HDAY,CUMDPT,PESW,NLAYR,
     &                  ATOT,TMA,WW,FX,B,DP,TAV,TAMP,Z,ST,ALBEDO)
      IMPLICIT NONE

      INTEGER  K,L,DOY,NLAYR
      REAL     TAVG,TMAX,SRAD,ALX,AW,WC,DD,TA,DT,ZD
      REAL     CUMDPT,HDAY,PESW,ATOT,TMA(5),WW,FX,B
      REAL     DP,TAV,TAMP,Z(20),ST(20),ALBEDO

      ALX    = (FLOAT(DOY)-HDAY)*0.0174
      ATOT   = ATOT-TMA(5)

      DO K = 5, 2, -1
         TMA(K) = TMA(K-1)
      END DO

      TMA(1) = (1.0-ALBEDO)*(TAVG+(TMAX-TAVG)*SQRT(SRAD*0.03))+
     &          ALBEDO*TMA(1)
      ATOT   = ATOT + TMA(1)
      AW     = PESW
      IF (AW .LE. 0.0) THEN
         AW = 0.01
      ENDIF
      WC    = AW/(WW*CUMDPT*10.0)
      FX    = EXP(B*((1.0-WC)/(1.0+WC))**2)
      DD    = FX*DP
      TA    = TAV+TAMP*COS(ALX)/2.0
      DT    = ATOT/5.0-TA

      DO L = 1, NLAYR
         ZD    = -Z(L)/DD
         ST(L) = TAV + (TAMP/2.0*COS(ALX+ZD)+DT)*EXP(ZD)
      END DO

      RETURN

      END
