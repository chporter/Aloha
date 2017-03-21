C=================================================================
C  HMET, Subroutine, N.B. Pickering
C  Calculates hourly values of temperature, PAR, solar radiation,
C  fraction diffuse radiation, and relative humidity,
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  12/02/90 NBP Written
C  05/28/93 NBP Modified temperature and radiation functions
C  10/23/93 NBP Modified FRACD function
C  11/23/93 NBP Limit on BETA to 1 deg, if positive
C  09/04/96 GH  Fixed ASIN problems
C-----------------------------------------------------------------------
C  Input : H,HR,HS,DOY,PAR,SRAD,TMEST,TMAX,TMIN,TMAXY,TMINY,TMINT,XLAT,
C  Output: PARHR,RADHR,TAIRHR,AZZON,BETA
C  Local : TS,DEC,HR,SNDN,SNUP,SNDNY,SNUPY,SNUPT,SRADHR,TINCR
C  Fn/Sub: HANG,HTEMP,HRAD,FRACD,HPAR
C  Notes : CROPGRO version
C=======================================================================

      SUBROUTINE HMET(DAYL,CLOUDS,DEC,ISINB,PAR,REFHT,S0N,SNDN,
     &  SNUP,SRAD,TDEW,TMAX,TMIN,XLAT,WINDHT,WINDSP,AZZON,BETA,
     &  FRDIFP,FRDIFR,PARHR,RADHR,RHUMHR,TAIRHR,TAVG,TDAY,WINDHR)

      IMPLICIT NONE
      INTEGER H,NDAY,TS
      PARAMETER (TS=24)
      REAL AZZON(TS),BETA(TS),CLOUDS,DAYL,DEC,FRDIFP(TS),FRDIFR(TS),
     &  HS,ISINB,PAR,PARHR(TS),TAIRHR(TS),REFHT,S0N,SRAD,SNDN,SNUP,
     &  TAVG,TDAY,TDEW,TINCR,TMAX,TMIN,RADHR(TS),RH,RHUMHR(TS),VPSAT,
     &  WINDAV,WINDHT,WINDSP,WINDHR(TS),XLAT
      PARAMETER (TINCR=24./TS)

C     Initialize

      TAVG = 0.0
      TDAY = 0.0
      NDAY = 0

C     Loop to compute hourly weather data.

      DO 10 H = 1,TS

C       Calculate real and solar time.

        HS = REAL(H) * TINCR

C       Calculate sun angles and hourly weather variables.

        CALL HANG(HS,DEC,XLAT,AZZON(H),BETA(H))
        CALL HTEMP(DAYL,HS,SNDN,SNUP,TMAX,TMIN,TAIRHR(H))
        RH = VPSAT(TDEW) / VPSAT(TAIRHR(H)) * 100.0
        RHUMHR(H) = MIN(RH,100.0)
        WINDAV = WINDSP / 86.4 * (REFHT/WINDHT)**0.2
        IF (HS .GT. SNUP .AND. HS .LT. SNDN) THEN
          WINDHR(H) = 2.0 * WINDAV / (1.0+DAYL/24.0)
        ELSE
          WINDHR(H) = WINDAV / (1.0+DAYL/24.0)
        ENDIF
        CALL HRAD(BETA(H),HS,ISINB,SNDN,SNUP,SRAD,RADHR(H))
        CALL FRACD(BETA(H),CLOUDS,HS,RADHR(H),S0N,SNDN,SNUP,
     &    FRDIFP(H),FRDIFR(H))
        CALL HPAR(HS,PAR,SNDN,SNUP,SRAD,RADHR(H),PARHR(H))
        TAVG = TAVG + TAIRHR(H)
        IF (H .GE. SNUP .AND. H .LE. SNDN) THEN
          TDAY = TDAY + TAIRHR(H)
          NDAY = NDAY + 1
        ENDIF

   10 CONTINUE

      TAVG = TAVG / REAL(TS)
      TDAY = TDAY / REAL(NDAY)
      IF (PAR .LE. 0.) PAR = 2.0 * SRAD

      END

C=======================================================================
C  HANG, Subroutine, N.B. Pickering, 12/02/90
C  Computes solar elevation and azimuth for time-of-day (deg).
C-----------------------------------------------------------------------
C  12/02/90 NBP Written
C  08/16/96 GH  Put limits on BETAS to avoid ASIN problems
C-----------------------------------------------------------------------
C  Input : HS,DEC,XLAT
C  Output: AZZON,BETA
C  Local : PI,RAD,SINAS,SINB,SOLTIM
C=======================================================================

      SUBROUTINE HANG(HS,DEC,XLAT,AZZON,BETA)

      IMPLICIT NONE
      REAL AZZON,BETA,CCOS,HANGL,HS,DEC,PI,RAD,SINAS,SSIN,XLAT,BETAS
      PARAMETER (PI=3.14159, RAD=PI/180.)

C     Sines and cosines and their products.

      SSIN = SIN(RAD*DEC) * SIN(RAD*XLAT)
      CCOS = COS(RAD*DEC) * COS(RAD*XLAT)
      HANGL = (HS-12.0)*PI/12.0

C     Calculate solar elevation and azimuth.
      BETAS = MIN(MAX(SSIN + CCOS*COS(HANGL),-1.0),1.0)
      BETA  = ASIN(BETAS)
      SINAS = COS(RAD*DEC) * SIN(HANGL) / COS(BETA)
      SINAS = MIN(MAX(SINAS,-1.0),1.0)
      AZZON = ASIN(SINAS)
      AZZON = AZZON / RAD
      BETA = BETA / RAD
      IF (BETA .GT. 0) BETA = MAX(BETA, 1.0)

      END

C=======================================================================
C  HTEMP, Subroutine, N.B. Pickering, 09/16/91.
C  Computes temperature for hour-of-day (Parton and Logan, 1981).
C-----------------------------------------------------------------------
C  Input : HS,SNDN,SNUP,TMAX,TMIN
C  Output: TAIRHR
C  Local : HDECAY,SLOPE,T,TSNDNY,TSNDN
C  Notes : Only today's TMAX,TMIN,SNUP,SNDN are used.
C          A=0.0,B=1.0e-6,C=2.0 gives the same as WCALC.
C=======================================================================

      SUBROUTINE HTEMP(DAYL,HS,SNDN,SNUP,TMAX,TMIN,TAIRHR)

      IMPLICIT NONE
      REAL A,ARG,B,C,DAYL,HDECAY,HS,MAX,MIN,PI,SNDN,SNUP,T,
     &  TMAX,TMIN,TMINI,TAIRHR,TSNDN
      PARAMETER (A=2.0, B=2.2, C=1.0, PI=3.14159)    ! Parton and Logan

C     Calculate day length, time of maximum temperature,time of
C     minimum temperature, hours of exponential decay, temperature
C     at sunset, and temperature at infinite time.

      MIN = SNUP + C
      MAX = MIN + DAYL/2.0 + A
      T = 0.5 * PI * (SNDN-MIN) / (MAX-MIN)
      TSNDN = TMIN + (TMAX-TMIN)*SIN(T)
      TMINI = (TMIN-TSNDN*EXP(-B)) / (1.0-EXP(-B))
      HDECAY = 24.0 + C - DAYL

C     Daylight hours--sine curve.  Minimum temperature at (SNUP+C) and
C     maximum temperature at (noon+A+C) hours.

      IF (HS .GE. SNUP+C .AND. HS .LE. SNDN) THEN
        T = 0.5 * PI * (HS-MIN) / (MAX-MIN)
        TAIRHR = TMIN + (TMAX-TMIN)*SIN(T)

C     Nighttime--exponential decrease from sunset temperature (TSNDN)
C     to minimum (TMINI) at infinite time and TMIN at (SNUP+C).

      ELSE
        IF (HS .LT. SNUP+C) THEN
          T = 24.0 + HS - SNDN
        ELSE IF (HS .GT. SNDN) THEN
          T = HS - SNDN
        ENDIF
        ARG = -B * T / HDECAY
        TAIRHR = TMINI + (TSNDN-TMINI)*EXP(ARG)
      ENDIF

      END

C=======================================================================
C  HRAD, Subroutine, N.B. Pickering, 03/19/91
C  Computes total hourly solar radiation (Spitters, 1986).
C-----------------------------------------------------------------------
C  Input : BETA,HS,ISINB,SNDN,SNUP,SRAD
C  Output: RADHR
C  Local : CONST,PARQC,PI,RAD,SINB,SOLTIM
C=======================================================================

      SUBROUTINE HRAD(BETA,HS,ISINB,SNDN,SNUP,SRAD,RADHR)

      IMPLICIT NONE
      REAL BETA,HS,ISINB,PI,RAD,RADHR,SINB,SNDN,SNUP,SRAD
      PARAMETER (PI=3.14159, RAD=PI/180.0)

C     Daylight hour calculations

      IF (HS .GT. SNUP .AND. HS .LT. SNDN) THEN

C       Initialization

        SINB = SIN(RAD*BETA)

C       Compute instantaneous SRAD values with Spitters' Eqn. 6.

        RADHR = SINB * (1.0+0.4*SINB) * SRAD*1.0E6 / ISINB

      ELSE
        RADHR = 0.0
      ENDIF

      END

C=======================================================================
C  FRACD, Subroutine, N.B. Pickering, 03/19/91
C  Computes hourly fraction diffuse total radiation and PAR.
C-----------------------------------------------------------------------
C  Input : BETA,CLOUDS,HS,RADHR,S0N,SNDN,SNUP
C  Output: FRDIFP,FRDIFP
C  Local : AMTRH,CORR,COS90B,COSB,DAYL,FRDFH,PI,RAD,S0,SDF,SINB
C=======================================================================

      SUBROUTINE FRACD(BETA,CLOUDS,HS,RADHR,S0N,SNDN,SNUP,FRDIFP,FRDIFR)

      IMPLICIT NONE
      REAL AMTRH,BETA,CLOUDS,CORR,COS90B,COSB,FRDFH,FRDIFP,
     &  FRDIFR,HS,PI,RAD,RADHR,S0,S0N,SDF,SINB,SNDN,SNUP
      PARAMETER (PI = 3.14159, RAD = PI/180.0)

C     Daylight hour calculations

      IF (HS .GT. SNUP .AND. HS .LT. SNDN) THEN

C       Initialization

        SINB = SIN(RAD*BETA)
        COSB = COS(RAD*BETA)
        COS90B = COS(RAD*(90.0-BETA))

C       Calculate instantaneous atmospheric transmission from global
C       and extra-terrestial radiation.

        S0 = S0N * SINB
        AMTRH = RADHR / S0

C       Calculate hourly fraction diffuse from hourly atmospheric
C       transmission (after Erbs, Klein and Duffie, 1982; De Jong,
C       1980).  Lower end adjusted to give smooth transition.

        IF (AMTRH .LE. 0.22) THEN
          FRDFH = 1.0
        ELSE IF (AMTRH .GT. 0.22 .AND. AMTRH .LE. 0.5) THEN
          FRDFH = 1.0 - 4.3*(AMTRH-0.22)**2
        ELSE
          FRDFH = 0.1 + 0.563*EXP(-5.5*(AMTRH-0.5))
        ENDIF

C       Calculate instantaneous diffuse global irradiance.

        SDF = RADHR * FRDFH

C       Decrease sdf by circumsolar part of diffuse radiation.

        CORR =  (1.0-CLOUDS) * COS90B**2 * COSB**3
        SDF = SDF / (1.0 + CORR)

C       Calculate intantaneous fraction diffuse of global and par.

        IF (RADHR .GT. 0.0) THEN
          FRDIFR = SDF / RADHR
        ELSE
          FRDIFR = 0.0
        ENDIF
        FRDIFP = (1.0+0.3*(1.0-CLOUDS)) * FRDIFR
        FRDIFR = MIN(MAX(FRDIFR,0.0),1.0)
        FRDIFP = MIN(MAX(FRDIFP,0.0),1.0)

      ELSE
        FRDIFP = 1.0
        FRDIFR = 1.0
      ENDIF

      END

C=======================================================================
C  HPAR, Subroutine, N.B. Pickering, 03/19/91
C  Computes hourly PAR using Spitters (1986) or Efimova (1967)
C  in Pearcy et al (1991).
C-----------------------------------------------------------------------
C  Input : BETA,FRDIFR,HS,ISINB,PAR,SNDN,SNUP,RADHR
C  Output: PARHR
C  Local : CONST,DAYL,PARFAC,PARQC,PI,RAD,SINB,SOLTIM
C=======================================================================

      SUBROUTINE HPAR(HS,PAR,SNDN,SNUP,SRAD,RADHR,PARHR)

      IMPLICIT NONE
      REAL HS,PAR,PARFAC,PARHR,PARQC,RADHR,SNDN,SNUP,SRAD
      PARAMETER (PARQC=4.6)

C     Daylight hour calculations

      IF (HS .GT. SNUP .AND. HS .LT. SNDN) THEN

C       Compute instantaneous PAR using same equation as HRAD.  If PAR not
C       available, compute from RADHR and FRDIFR.  Uses PARQC=4.6 photon/J
C       and Eqn. 6.2 in Pearcy et al (1991).  PARFAC=2.1-2.6 (sunny-cloudy).

        IF (PAR .GT. 0.0) THEN
          PARHR = RADHR * PAR/SRAD
        ELSE
C         PARFAC = (0.43*(1.0-FRDIFR)+0.57*FRDIFR) * PARQC
          PARFAC = 2.0
          PARHR = RADHR * PARFAC
        ENDIF

C     Night time.

      ELSE
        PARHR = 0.0
      ENDIF

      END

C=======================================================================
C  VPSAT, Real Function, N.B. Pickering, 4/1/90
C  Calculates saturated vapor pressure of air (Tetens, 1930).
C-----------------------------------------------------------------------
C  Input : T (C)
C  Output: VPSAT (Pa)
C=======================================================================

      REAL FUNCTION VPSAT(T)

      IMPLICIT NONE
      REAL T

      VPSAT = 610.78 * EXP(17.269*T/(T+237.30))

      END

C=======================================================================
C  VPSAT, Real Function, N.B. Pickering, 4/1/90
C  Calculates slope of saturated vapor pressure versus temperature curve
C  using Classius-Clapeyron equation (see Brutsaert, 1982 p. 41)
C-----------------------------------------------------------------------
C  Input : T (C)
C  Output: VPSAT (Pa/K)
C=======================================================================

      REAL FUNCTION VPSLOP(T)

      IMPLICIT NONE

      REAL T,VPSAT

C     dEsat/dTempKel = MolWeightH2O * LatHeatH2O * Esat / (Rgas * TempKel^2)

      VPSLOP = 18.0 * (2501.0-2.373*T) * VPSAT(T) / (8.314*(T+273.0)**2)

      END
