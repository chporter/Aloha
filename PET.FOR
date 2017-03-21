C=======================================================================
C  PETPEN, Subroutine, N.B. Pickering
C  Calculates FAO-24 Penman potential evapotranspiration (without
C  correction)--grass reference.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/19/92 NBP Written
C  11/04/93 NBP Renamed routine PET to PETPEN.  Changed XLAI to XHLAI
C               Added XHLAI .LE. 0.0 statement.
C  05/13/94 NBP Converted all vapor pressures to Pa.  Rearranged.
C  09/16/94 NBP Added limits to prevent EO and ES (PE) < 0.
C-----------------------------------------------------------------------
C  INPUT : CLOUDS,SALB,SRAD,TAVG,TDEW,TMAX,TMIN,WINDHT,WINDSP,XHLAI
C  OUTPUT: EO
C  LOCAL : ALBEDO,DAIR,EAIR,ESAT,G,LHVAP,PATM,PSYCON,RADB,RNET,RT,S,
C          SBZCON(MJ/m2/d),SHAIR,TK4,VHCAIR,VPD,WFNFAO,WINDM,WINDM2
C  FN/SUB: VPSAT,VPSLOP
C=======================================================================

      SUBROUTINE PETPEN(CLOUDS,SALB,SRAD,TAVG,TDEW,TMAX,TMIN,WINDHT,
     &  WINDSP,XHLAI,EO)

      IMPLICIT NONE
      REAL ALBEDO,CLOUDS,DAIR,EAIR,EO,ESAT,G,LHVAP,PATM,PSYCON,RADB,
     &  RNET,RNETMG,RT,S,SALB,SBZCON,SHAIR,SRAD,TAVG,TDEW,TK4,TMAX,
     &  TMIN,VHCAIR,VPD,VPSAT,VPSLOP,WFNFAO,WINDHT,WIND2,WINDSP,XHLAI
      PARAMETER (SHAIR = 1005.0, PATM = 101300.0, SBZCON=4.093E-9)

C     Compute air properties.

      LHVAP = (2501.0-2.373*TAVG) * 1000.0                 ! J/kg
      PSYCON = SHAIR * PATM / (0.622*LHVAP)                ! Pa/K
      ESAT = (VPSAT(TMAX)+VPSAT(TMIN)) / 2.0               ! Pa
      EAIR = VPSAT(TDEW)                                   ! Pa
      VPD = ESAT - EAIR                                    ! Pa
      S = (VPSLOP(TMAX)+VPSLOP(TMIN)) / 2.0                ! Pa/K
      RT = 8.314 * (TAVG+273.0)                            ! N.m/mol
      DAIR = 0.1 * 18.0 / RT * ((PATM-EAIR)/0.622+EAIR)    ! kg/m3
      VHCAIR = DAIR * SHAIR                                ! J/m3

C     Convert windspeed to 2 m reference height.

      WIND2 = WINDSP * (2.0/WINDHT)**0.2

C     Calculate net radiation (MJ/m2/d).  Constants for RADB from
C     Jensen et al (1989) for semi-humid conditions.  The value 0.005
C     converts the value 0.158 from kPa to Pa.

      G = 0.0
      IF (XHLAI .LE. 0.0) THEN
        ALBEDO = SALB
      ELSE
        ALBEDO = 0.23-(0.23-SALB)*EXP(-0.75*XHLAI)
      ENDIF
      TK4 = ((TMAX+273.)**4+(TMIN+273.)**4) / 2.0
      RADB = SBZCON*TK4 * (0.4-0.005*SQRT(EAIR)) * (1.1*(1.-CLOUDS)-0.1)
      RNET= (1.0-ALBEDO)*SRAD - RADB

C     Compute ETP using the FAO wind function.  The multipliers for WNDFAO
C     are 1000 times smaller than in Jensen et al (1979) to convert VPD in
C     Pa to kPa. Equation for RNETMG converts from MJ/m2/d to mm/day.

      WFNFAO = 0.0027 * (1.0+0.01*WIND2)
      RNETMG = (RNET-G) / LHVAP * 1.0E6
      EO = (S*RNETMG + PSYCON*WFNFAO*VPD) / (S+PSYCON)
      EO = MAX(EO,0.0)

      END

C=======================================================================
C  PETPT, Subroutine, J.T. Ritchie
C  Calculates Priestly-Taylor potential evapotranspiration
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/?? JR  Written
C  11/04/93 NBP Modified
C-----------------------------------------------------------------------
C  Input :
C  Output: EO
C  Local :
C  Fn/Sub:
C=======================================================================

      SUBROUTINE PETPT(SALB,SRAD,TMAX,TMIN,XHLAI,EO)

      IMPLICIT NONE
      REAL ALBEDO,EEQ,EO,SALB,SLANG,SRAD,TD,TMAX,TMIN,XHLAI

      TD = 0.60*TMAX+0.40*TMIN
      IF (XHLAI .LE. 0.0) THEN
        ALBEDO = SALB
      ELSE
        ALBEDO = 0.23-(0.23-SALB)*EXP(-0.75*XHLAI)
      ENDIF

      SLANG = SRAD*23.923
      EEQ = SLANG*(2.04E-4-1.83E-4*ALBEDO)*(TD+29.0)
      EO = EEQ*1.1
      IF (TMAX .GT. 35.0) THEN
        EO = EEQ*((TMAX-35.0)*0.05+1.1)
      ELSE IF (TMAX .LT. 5.0) THEN
        EO = EEQ*0.01*EXP(0.18*(TMAX+20.0))
      ENDIF
      EO = MAX(EO,0.0)

      END

C=======================================================================
C  PE, Real Function, J.T. Ritchie
C  Calculates soil potential evaporation from total PET and LAI.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/?? JR  Written
C  11/04/93 NBP Modified
C-----------------------------------------------------------------------
C  Input : EO,XHLAI
C  Output: PE
C=======================================================================

      REAL FUNCTION PE(EO,XHLAI)

      IMPLICIT NONE

      REAL EO,XHLAI

      IF (XHLAI .LE. 1.0) THEN
        PE = EO*(1.0-0.43*XHLAI)
      ELSE
        PE = EO/1.1*EXP(-0.4*XHLAI)
      ENDIF
      PE = MAX(PE,0.0)

      END
