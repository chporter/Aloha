C=======================================================================
C  INSOILT, Subroutine
C
C  Determines soil temperature initialization
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       8-28-93
C  4. Renamed to INSOILT
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  :
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : RICE
C
C  Calls  : SOILT
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  EOS    : Potential soil evaporation - mm/day
C=======================================================================

      SUBROUTINE INSOILT (MESIC,TAVG,XLAT,TMAX,SRAD,DOY,HDAY,CUMDPT,
     &                    Z,BD,FX,DP,WW,B,ALBEDO,TAV,TAMP,TMA,ATOT,ST,
     &                    NLAYR,DLAYR,SALB,PESW)
      IMPLICIT  NONE

      CHARACTER MESIC*1
      INTEGER   I,L,DOY,NLAYR
      REAL      TBD,TAVG,XLAT,ABD,TMAX,SRAD
      REAL      HDAY,CUMDPT,Z(20),BD(20),FX,DP,WW,B,ALBEDO,TAV,TAMP
      REAL      TMA(5),ATOT,ST(20),DLAYR(20),SALB,PESW

      IF (XLAT .LT. 0.0) THEN
         HDAY =  20.0
       ELSE
         HDAY = 200.0
      ENDIF
      CUMDPT = 0.0
      TBD    = 0.0
      DO L = 1, NLAYR
         Z(L)   = CUMDPT + DLAYR(L)* 5.0
         CUMDPT = CUMDPT + DLAYR(L)*10.0
         TBD    = TBD    + BD(L)
      END DO
      ABD    = TBD/FLOAT(NLAYR)
      FX     = ABD/(ABD+686.0*EXP(-5.63*ABD))
      DP     = 1000.0 + 2500.0*FX
      WW     = 0.356  - 0.144*ABD
      B      = ALOG(500.0/DP)
      ALBEDO = SALB
      IF (TAV  .LE. 0.0) THEN
          TAV = 20.0
      ENDIF
      IF (TAMP .LE. 0.0) THEN
          TAMP = 5.0
      ENDIF
      IF (MESIC .EQ. 'M') THEN
         DO I = 1, 5
            TMA(I) = TAVG
         END DO
         ATOT = TMA(1)*5.0
         DO L = 1, NLAYR
            ST(L) = TAVG
         END DO
         DO I = 1, 7
            CALL SOILT (DOY,TMAX,TAVG,SRAD,HDAY,CUMDPT,PESW,NLAYR,
     &                  ATOT,TMA,WW,FX,B,DP,TAV,TAMP,Z,ST,ALBEDO)
         END DO
      ENDIF

      RETURN
      END
