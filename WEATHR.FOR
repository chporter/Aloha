C=======================================================================
C  WEATHR, Subroutine, N.B. Pickering
C  Weather handling routine: input, weather modification and
C  generation of hourly values.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  08/30/91 NBP Written
C  10/23/93 NBP Separated DAYLEN function and created DECL to compute
C               declination from DAYL.  Used for sens. analysis.
C  04/04/96 GH  Added RHUM
C  07/14/96 GH  Initialize variables for WGEN
C-----------------------------------------------------------------------
C  Input :
C  Output:
C  Local :
C  Sub/Fn: DAYLEN,ERROR,HMET,IPWTH,SOLAR,WTHMDB,WGEN
C=======================================================================

      SUBROUTINE WEATHR(CO2ADJ,CO2FAC,DAYADJ,DAYFAC,DOY,DPTADJ,DPTFAC,
     &  FILEW,PRCADJ,PRCFAC,RADADJ,RADFAC,TMADJ,TMFAC,
     &  TXADJ,TXFAC,WMODB,WNDADJ,WNDFAC,WMDATE,
     &  AZZON,BETA,CLOUDS,CO2,DAYL,FRDIFP,FRDIFR,NEV,PAR,
     &  PARHR,RADHR,RAIN,REFHT,RHUMHR,SNDN,SNUP,SRAD,TAIRHR,TAMP,
     &  TAV,TAVG,TDAY,TDEW,TMAX,TMIN,WINDHR,WINDHT,WINDSP,XLAT,XLONG,
     &  YRDOY,YRSIM,PATHWT,MEWTH,MULTI,NREP,RSEED1,RSEED,RNMODE,
     &  RHUM,XELEV,WTHADJ)

      IMPLICIT NONE

      CHARACTER*1  CO2FAC(10),DAYFAC(10),DPTFAC(10),WNDFAC(10)
      CHARACTER*1  PRCFAC(10),RADFAC(10),TMFAC(10),TXFAC(10)
      CHARACTER*1  MEWTH,RNMODE
      CHARACTER*1  WMODB
      CHARACTER*6  ERRKEY
      CHARACTER*12 FILEW
      CHARACTER*80 PATHWT

      INTEGER DOY,LINWTH,NEV,TS,WMDATE(10),YRDOY,YRSIM,MULTI
      INTEGER RSEED1,RSEED(4),NREP

      PARAMETER (TS=24)

      REAL
     &  AZZON(TS),BETA(TS),DAYL,CLOUDS,CO2,CO2ADJ(10),CO2BAS,DAYADJ(10),
     &  DEC,DPTADJ(10),XELEV,FRDIFP(TS),FRDIFR(TS),ISINB,PAR,PARHR(TS),
     &  PRCADJ(10),RADADJ(10),RADHR(TS),RAIN,REFHT,RHUMHR(TS),S0N,
     &  SNDN,SNUP,SRAD,TAIRHR(TS),TAMP,TAV,TAVG,TDEW,TMADJ(10),TMAX,
     &  TMIN,TXADJ(10),WINDHR(TS),WINDHT,WINDSP,WNDADJ(10),WTHADJ(2,8),
     &  XLAT,XLONG,CCO2,TDAY,RHUM

      PARAMETER (CO2BAS = 330.0)
      PARAMETER (ERRKEY = 'WEATHR')

C     Read new weather record.

      IF (MEWTH .EQ. 'M' .OR. MEWTH .EQ. 'G') THEN
        CALL IPWTH(YRDOY,FILEW,YRSIM,LINWTH,XELEV,PAR,TAMP,TAV,
     &    TDEW,TMAX,TMIN,RAIN,REFHT,SRAD,WINDSP,XLAT,XLONG,WINDHT,CCO2,
     &    PATHWT,RHUM)
      ELSE IF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W') THEN
C       Set default values FOR REFHT AND WINDHT
        REFHT  = 1.5
        WINDHT = 2.0
        CCO2   = 0.0
        SRAD = -99.0
        TMAX = -99.0
        TMIN = -99.0
        RAIN = -99.0
        PAR  = -99.0
        RHUM = -99.0
        CALL WGEN (PATHWT,FILEW,MEWTH,RSEED1,YRDOY,YRSIM,XLAT,XLONG,
     &    SRAD,TMAX,TMIN,RAIN,PAR,TDEW,WINDSP,MULTI,NREP,RSEED,RNMODE)
      ELSE
        CALL ERROR(ERRKEY,1,' ',0)
      ENDIF
      IF (CCO2 .LE. 0.) THEN
        CO2 = CO2BAS
      ELSE
        CO2 = CCO2
      ENDIF

C     Calculate day length, sunrise and sunset.

      CALL DAYLEN(DOY,XLAT,DAYL,DEC,SNDN,SNUP)

C     Update weather modification array (WTHADJ).

      IF (NEV .GT. 0)
     &  CALL WTHMDB(CO2ADJ,CO2FAC,DAYADJ,DAYFAC,DPTADJ,
     &  DPTFAC,NEV,PRCADJ,PRCFAC,RADADJ,RADFAC,TMADJ,TMFAC,
     &  TXADJ,TXFAC,WMDATE,WNDADJ,WNDFAC,YRDOY,WMODB,WTHADJ)

C     Adjust daily weather data, if weather modification requested.
C     Effective DEC calculated if DAYL is changed.

      IF (WMODB .EQ. 'Y') THEN
        CALL WTHMOD(FILEW,LINWTH,WTHADJ,XLAT,DEC,CO2,DAYL,PAR,
     &  RAIN,SNUP,SNDN,SRAD,TDEW,TMAX,TMIN,WINDSP)
      ENDIF

C     Calculate daily solar parameters.

      CALL SOLAR(SRAD,XLAT,CLOUDS,DAYL,DEC,ISINB,S0N)

C     Calculate hourly weather data.

      CALL HMET(DAYL,CLOUDS,DEC,ISINB,PAR,REFHT,S0N,SNDN,
     &  SNUP,SRAD,TDEW,TMAX,TMIN,XLAT,WINDHT,WINDSP,AZZON,BETA,
     &  FRDIFP,FRDIFR,PARHR,RADHR,RHUMHR,TAIRHR,TAVG,TDAY,WINDHR)

      END
