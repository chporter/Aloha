C=======================================================================
C  SOLAR, Subroutine, N.B. Pickering
C  Computes declination, solar time of sunrise and sunset, daily solar
C  constant (Spitters, 1986) and cloudiness factor.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  12/02/90 NBP Written
C  09/23/93 NBP Separated DAYLEN function and created DECL to compute
C               declination from DAYL.  Removed DOY effect on S0N.
C-----------------------------------------------------------------------
C  Input : DOY,SRAD,XLAT
C  Output: CLOUDS,DAYL,DEC,ISINB,S0N,SNDN,SNUP
C  Local : AMTRD,C1,C2,CCOS,PI,RAD,SC,S0D,S1,S2,SC,SOC,SRADJ,
C          SSIN,T
C  Fn/Sub: SUNRIS
C  Notes : CROPGRO version.
C=======================================================================

      SUBROUTINE SOLAR(SRAD,XLAT,CLOUDS,DAYL,DEC,ISINB,S0N)

      IMPLICIT NONE
      REAL AMTRCS,AMTRD,CCOS,CLOUDS,DAYL,DEC,DSINB,ISINB,
     &  PI,RAD,SCLEAR,S0D,S0N,SC,SOC,SRAD,SRADJ,SSIN,XLAT
      PARAMETER (AMTRCS=0.77, PI = 3.14159, RAD = PI/180.0, SC=1368.0)

C     Initialization

      SRADJ = SRAD * 1.0E6

C     Sun angles.  SOC limited for latitudes above polar circles.

      SSIN = SIN(RAD*DEC) * SIN(RAD*XLAT)
      CCOS = COS(RAD*DEC) * COS(RAD*XLAT)
      SOC = SSIN / CCOS
      SOC = MIN(MAX(SOC,-1.0),1.0)

C     Integral of SINB over day (sec) (Eqn. 18).

      DSINB = 3600.0 * (DAYL*SSIN + 24.0/PI*CCOS*SQRT(1.0-SOC**2))

C     Set normal (perp.) extra-terrestial radiation equal to
C     average solar constant (elliptical orbit ignored).

      S0N = SC

C     Calculate daily atmospheric transmission from global
C     and normal extra-terrestial radiation.

      S0D = S0N * DSINB
      AMTRD = SRADJ / S0D

C     Calculate clear sky radiation (0.77*S0D) in MJ.

      SCLEAR = AMTRCS * S0D * 1.E-6

C     Calculate daily cloudiness factor (range = 0-1).

      CLOUDS = MIN(MAX(1.0-SRAD/SCLEAR,0.0),1.0)

C     Integral in Eqn. 6 (used in HMET)

      ISINB = 3600.0 * (DAYL*(SSIN+0.4*(SSIN**2+0.5*CCOS**2)) +
     &  24.0/PI*CCOS*(1.0+1.5*0.4*SSIN)*SQRT(1.0-SOC**2))

      END

C=======================================================================
C  DAYLEN, Real Function, N.B. Pickering, 09/23/93
C  Computes solar day length (Spitters, 1986).
C-----------------------------------------------------------------------
C  Input : DOY,XLAT
C  Output: DAYL
C  Local : DEC,PI,RAD,SOC
C=======================================================================

      SUBROUTINE DAYLEN(DOY,XLAT,DAYL,DEC,SNDN,SNUP)

      IMPLICIT NONE
      INTEGER DOY
      REAL DEC,DAYL,PI,RAD,SOC,SNDN,SNUP,XLAT
      PARAMETER (PI=3.14159, RAD=PI/180.0)

C     Calculation of declination of sun (Eqn. 16). Amplitude= +/-23.45
C     deg. Minimum = DOY 355 (DEC 21), maximum = DOY 172.5 (JUN 21/22).

      DEC = -23.45 * COS(2.0*PI*(DOY+10.0)/365.0)

C     Sun angles.  SOC limited for latitudes above polar circles.

      SOC = TAN(RAD*DEC) * TAN(RAD*XLAT)
      SOC = MIN(MAX(SOC,-1.0),1.0)

C     Calculate daylength, sunrise and sunset (Eqn. 17)

      DAYL = 12.0 + 24.0*ASIN(SOC)/PI
      SNUP = 12.0 - DAYL/2.0
      SNDN = 12.0 + DAYL/2.0

      END

C=======================================================================
C  DECL, Real Function, N.B. Pickering, 09/23/93
C  Computes solar declination from day length (Spitters, 1986).
C-----------------------------------------------------------------------
C  Input : DAYL,XLAT
C  Output: DECL
C  Local : DL,PI,RAD,SC,XL
C=======================================================================

      REAL FUNCTION DECL(DAYL,XLAT)

      IMPLICIT NONE
      REAL DAYL,DL,PI,RAD,SC,XL,XLAT
      PARAMETER (PI=3.14159, RAD=PI/180.0)

C     Limit value of XLAT and DL to prevent zeros.

      IF (XLAT .GE. 0.0 .AND. XLAT .LT. 0.1) THEN
        XL = 0.1
        DL = 12.00573
      ELSE IF (XLAT .LT. 0.0 .AND. XLAT .GT. -0.1) THEN
        XL = -0.1
        DL = 11.99427
      ELSE
        DL = DAYL
        XL = XLAT
      ENDIF

C     Calculation of declination of sun.

      SC = SIN((DL-12.0)/24.0*PI)
      XL = TAN(RAD*XL)
      DECL = ATAN(SC/XL) / RAD

      END
