C=======================================================================
C  WGEN, Subroutine, Richardson and Wright
C  Generates daily weather data for crop models.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/84     Written
C  ??/??/??     Modified for DSSAT V2.
C  09/02/92 NBP Modified for DSSAT V3.  Internal generation of daily
C               data and use of monthly means as input.
C  06/29/94 JH  Added lower limit of 0.1 to calc. of rainfall in WGEN.
C  06/29/94 JH  Modified to handle months with no rainfall in WGENIN.
C  07/29/94 NBP Mod. JH's changes.  Added extra checks for rain probs.
C               Changed defaults for TDEW, WINDSP.  Cleaned up.
C  07/14/96 GH  Moved initialization section to WEATHR
C-----------------------------------------------------------------------
C  Input : PATHWT,FILEW,MEWTH,RSEED1,YRDOY,YRSIM,XLAT,XLONG,MULTI,
C          NREP,RNMODE
C  In/Out: RSEED
C  Output: SRAD,TMAX,TMIN,RAIN,PAR,TDEW,WINDSP
C  Saved : Y,CF,MTH,MM,YMID,YMEND,WETPRV,XPRV
C  Local :
C  Notes : The WGEN weather estimator (Richarson and Wright, 1984) was
C          modified by IBSNAT for DSSAT V3.  The IBSNAT modification
C          uses standard deviations rather than coefficients of
C          variation which makes the estimator more stable than
C          original version.  The generator has been modified to permit
C          internal use in the DSSAT crop models by generating a day
C          at a time.  Monthly input parameters are now used for all
C          variables with linear interpolation computed internally.
C          Monthly climate parameters can now be accepted as input and
C          the generator uses a combination of the regression equations
C          in SIMMETEO (Shu Geng et al., 1986, 1987?) and Pickering
C          et al. (1988) to compute the standard deviations.  All
C          previous versions of this generator and the coefficient
C          files supplied by the IBSNAT must be replaced with this
C          new version.  This generator produces values of SRAD, TMAX,
C          TMIN, and RAIN.  TDEW is estimated from TMIN.  WINDSP is
C          set at 1 m/s.  Later versions will include the appropriate
C          WGEN/SIMMETEO code to generate these variables.
C=======================================================================

      SUBROUTINE WGEN(PATHWT,FILEW,MEWTH,RSEED1,YRDOY,YRSIM,XLAT,XLONG,
     &  SRAD,TMAX,TMIN,RAIN,PAR,TDEW,WINDSP,MULTI,NREP,RSEED,RNMODE)

      IMPLICIT NONE

      SAVE Y,CF,MTH,MM,YMID,YMEND,WETPRV,XPRV

      CHARACTER*1  MEWTH*1,RNMODE*1,ERRKEY*6,FILEW*12,PATHWT*80
      INTEGER DOY,I,M,N,MTH,MM,YY,YR,YRDOY,YRSIM,YMID,YMEND,
     &  YDOY,RSEED1,RSEED(4),MTHMID,MTHEND,WETPRV,MULTI,NREP

      PARAMETER (N=12, M=14, ERRKEY = 'WGEN  ')

      REAL Y(N,M),CF(N,M),SRDMN,SRDSD,SRWMN,SRWSD,TXDMN,TXDSD,
     &  TXWMN,TXWSD,TNAMN,TNASD,PWW,PDW,ALPHA,BETA,PAR,SRAD,
     &  TMAX,TMIN,RAIN,XLAT,XLONG,XPRV(3),TDEW,WINDSP

C     Initialize.

      CALL YR_DOY(YRDOY,YR,DOY)
      YY = YR

C     Input and initialize if first day.

      IF (YRDOY .EQ. YRSIM) THEN
        IF ((INDEX('IANGS',RNMODE) .GT. 0 .AND. MULTI .EQ. 1) .OR.
     &      (INDEX('QF',RNMODE) .GT. 0 .AND. NREP .EQ. 1)) THEN
          IF (RSEED1 .GT. 0) THEN
            RSEED(1) = RSEED1
          ELSE
            RSEED(1) = 2510
          ENDIF
          RSEED(2) = 7692
          RSEED(3) = 2456
          RSEED(4) = 3765
        ENDIF

        IF (MEWTH .EQ. 'W') THEN
          CALL WGENIN(PATHWT,FILEW,XLAT,XLONG,Y,CF)
        ELSE IF (MEWTH .EQ. 'S') THEN
          CALL SIMMIN (PATHWT,FILEW,XLAT,XLONG,Y,CF)
        ELSE
          CALL ERROR(ERRKEY,1,' ',0)
        ENDIF
        WETPRV = 0
        DO 10 I=1,3
          XPRV(I) = 0.0
   10   CONTINUE
      ENDIF

C     Reset month if mid-month boundary is crossed.

      IF (YRDOY .EQ. YRSIM .OR. YRDOY .GT. YMID) THEN
        IF (YRDOY .EQ. YRSIM) THEN
          MTH = 1
   20     IF (YRDOY .GT. YDOY(YY,MTHMID(YY,MTH))) THEN
            CALL INCMTH(YY,MTH,+1)
          GOTO 20
          ENDIF
        ELSE
          CALL INCMTH(YY,MTH,+1)
        ENDIF
        MM = 0
        YMID = YDOY(YY,MTHMID(YY,MTH))
        CALL INCMTH(YY,MTH,-1)
        YMEND = YDOY(YY,MTHEND(YY,MTH))
        CALL INCMTH(YY,MTH,+1)
      ENDIF
      IF (YRDOY .GT. YMEND) THEN
        MM = 1
        YMEND = YDOY(YY,MTHEND(YY,MTH))
      ENDIF

C     Calculate generator parameters for day.

      CALL WGENPM (YR,DOY,Y,CF,MTH,MM,TXDMN,TXDSD,TXWMN,TXWSD,TNAMN,
     &  TNASD,SRDMN,SRDSD,SRWMN,SRWSD,PWW,PDW,ALPHA,BETA)

C     Generate daily weather data.

      CALL WGENGN (DOY,RSEED,XLAT,SRDMN,SRDSD,SRWMN,SRWSD,
     &  TXDMN,TXDSD,TXWMN,TXWSD,TNAMN,TNASD,PDW,PWW,ALPHA,BETA,
     &  WETPRV,XPRV,SRAD,TMAX,TMIN,RAIN,PAR)

C     Set default values for dew point temp and windspeed

      TDEW = TMIN + 2.0
      WINDSP = 86.4

      END

C=======================================================================
C  WGENIN,  Subroutine, N.B. Pickering
C  Reads WGEN inputs from parameter file and creates two matrices:
C  inputs and correction factors.
C  1=SRDMN, 2=SRDSD, 3=SRWMN, 4=SRWSD,  5=TXDMN, 6=TXDSD, 7=TXWMN,
C  8=TXWSD, 9=TNAMN, 10=TNASD, 11=ALPHA, 12=BETA, 13=PDW, 14=PWW
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/02/92 NBP Written for DSSAT V3.
C-----------------------------------------------------------------------
C  Input : PATHWT,FILEW,XLAT,XLONG,Y,CF
C  Output: Y,CF
C  Local :
C=======================================================================

      SUBROUTINE WGENIN(PATHWT,FILEW,XLAT,XLONG,Y,CF)

      IMPLICIT NONE

      CHARACTER BLANK*1,FILEW*12,PATHWT*80,FILEWW*92,LINE*100
      CHARACTER*6 ERRKEY,FINDCH

      INTEGER N,M,I,J,MTH,FOUND,LNUM,NUMDAY(12),LUNCLI,PATHL,ERRNUM

      PARAMETER (N=12, M=14, LUNCLI=11, ERRKEY='WGENIN', BLANK = ' ')

      REAL Y(N,M),CF(N,M),PW,RNUM,RTOT,RWET,XLAT,XLONG,ALPHA,BETA,
     &  PDW,PWW

      DATA NUMDAY/31,28.25,31,30,31,30,31,31,30,31,30,31/

      LNUM = 1
      PATHL  = INDEX(PATHWT,BLANK)
      IF (PATHL .LE. 1) THEN
          FILEWW = FILEW
      ELSE
         FILEWW = PATHWT(1:(PATHL-1)) // FILEW
      ENDIF
      OPEN (LUNCLI,FILE=FILEWW,STATUS='OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LNUM)

      FINDCH = '*CLIMA'
      CALL FIND (LUNCLI,FINDCH,LNUM,FOUND)
      IF (FOUND .EQ. 1) THEN
        CALL IGNORE(LUNCLI,LNUM,FOUND,LINE)
        IF (FOUND .NE. 1) CALL ERROR(ERRKEY,-1,FILEW,LNUM)
        READ (LINE,'(6X,2F9.0)') XLAT,XLONG
        REWIND(LUNCLI)
      ELSE
        CALL ERROR(ERRKEY,-1,FILEW,LNUM)
      ENDIF

      FINDCH = '*WGEN '
      CALL FIND (LUNCLI,FINDCH,LNUM,FOUND)
      IF (FOUND .EQ. 1) THEN
        DO 10 MTH=1,12
          CALL IGNORE(LUNCLI,LNUM,FOUND,LINE)
          IF (FOUND .NE. 1) CALL ERROR(ERRKEY,-1,FILEW,LNUM)
          READ (LINE,'(I6,14(1X,F5.0))')
     &      I,(Y(MTH,J),J=1,10),ALPHA,RTOT,PDW,RNUM

C         Compute PWW from PW and PDW and BETA [Y(MTH,M)] from RTOT
C         and ALPHA [Y(MTH,M-1)]. (ALPHA = 0.44 ==> skewness = 3.0)

          IF (RNUM .EQ. 0 .OR. RTOT .EQ. 0.0) THEN
            PW = 0.0
            PDW = 0.0
            PWW = 0.0
            ALPHA = 0.44
            BETA = 0.0
          ELSE
            PW = RNUM / NUMDAY(MTH)
            PWW = 1.0 - PDW*(1.0-PW)/PW
            PWW = MIN(MAX(PWW,0.0),1.0)
            ALPHA = MIN(MAX(ALPHA, 0.01),0.998)
            RWET  = RTOT / RNUM
            BETA  = MAX(RWET/ALPHA, 0.0)
          ENDIF
          Y(MTH,11)  = ALPHA
          Y(MTH,12)  = BETA
          Y(MTH,13)  = PDW
          Y(MTH,14)  = PWW
   10   CONTINUE
      ELSE
        CALL ERROR(ERRKEY,-1,FILEW,LNUM)
      ENDIF
      CALL CFACT(Y,CF)
      CLOSE(LUNCLI)

      END

C=======================================================================
C  SIMMIN,  Subroutine, N.B. Pickering
C  Reads SIMMETEO inputs from parameter file and creates two matrices:
C  1=SRDMN, 2=SRDSD, 3=SRWMN, 4=SRWSD,  5=TXDMN, 6=TXDSD, 7=TXWMN,
C  8=TXWSD, 9=TNAMN, 10=TNASD, 11=ALPHA, 12=BETA, 13=PDW, 14=PWW
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/02/92 NBP Written for DSSAT V3.
C-----------------------------------------------------------------------
C  Input : PATHWT,FILEW,XLAT,XLONG,Y,CF
C  Output: Y,CF
C  Local :
C=======================================================================

      SUBROUTINE SIMMIN (PATHWT,FILEW,XLAT,XLONG,Y,CF)

      IMPLICIT NONE

      CHARACTER BLANK*1,FILEW*12,PATHWT*80,FILEWW*92,LINE*100
      CHARACTER*6 ERRKEY,FINDCH
      INTEGER N,M,I,MTH,FOUND,LNUM,NUMDAY(12),LUNCLI,PATHL,ERRNUM

      PARAMETER (N=12, M=14, LUNCLI=11, ERRKEY='SIMMIN', BLANK = ' ')

      REAL Y(N,M),CF(N,M),PW(12),PDW(12),RNUM,RTOT,RWET,XLAT,XLONG,
     &  AVG,PWMN,PDWMN,SRDMNF(3),SRDCVF(3),SRWMNF(3),SRWCVF(3),
     &  SRAMNF(3),TXDMNF(3),TXWMNF(3),TXAMNF(3),SAVG,SDIFF,TDIFF,
     &  FOURIN,SRAMN(12),SRDCV,SRDMN,SRDSD,SRWCV,SRWMN,SRWSD,TNAMN(12),
     &  TNASD,TXAMN(12),TXDMN,TXDSD,TXWMN,TXWSD,ALPHA,BETA,PWW

      DATA NUMDAY/31,28.25,31,30,31,30,31,31,30,31,30,31/

      LNUM = 1
      PATHL  = INDEX(PATHWT,BLANK)
      IF (PATHL .LE. 1) THEN
          FILEWW = FILEW
      ELSE
         FILEWW = PATHWT(1:(PATHL-1)) // FILEW
      ENDIF
      OPEN (LUNCLI,FILE=FILEWW,STATUS='OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LNUM)
      FINDCH = '*CLIMA'
      CALL FIND (LUNCLI,FINDCH,LNUM,FOUND)
      IF (FOUND .EQ. 1) THEN
        CALL IGNORE(LUNCLI,LNUM,FOUND,LINE)
        IF (FOUND .NE. 1) CALL ERROR(ERRKEY,-1,FILEW,LNUM)
        READ (LINE,'(6X,2F9.0)') XLAT,XLONG
        REWIND(LUNCLI)
      ELSE
        CALL ERROR(ERRKEY,-1,FILEW,LNUM)
      ENDIF
      FINDCH = '*MONTH'
      CALL FIND (LUNCLI,FINDCH,LNUM,FOUND)
      IF (FOUND .EQ. 1) THEN
        DO 10 MTH=1,12
          CALL IGNORE(LUNCLI,LNUM,FOUND,LINE)
          IF (FOUND .NE. 1) CALL ERROR(ERRKEY,-1,FILEW,LNUM)
          READ (LINE,'(I6,5(1X,F5.0))')
     &      I,SRAMN(MTH),TXAMN(MTH),TNAMN(MTH),RTOT,RNUM

C         Compute PDW [Y(MTH,M-3)] from PW (0.82*PW in Hershfield, 1970?),
C         PWW [Y(MTH,M-2)] from PW and PDW [Y(MTH,M-3)], and BETA [Y(MTH,M)]
C         from RTOT and ALPHA [Y(MTH,M-1)]. (ALPHA = 0.44 ==> skewness = 3.0)

          IF (RNUM .EQ. 0 .OR. RTOT .EQ. 0.0) THEN
            PW(MTH) = 0.0
            PDW(MTH) = 0.0
            PWW = 0.0
            ALPHA = 0.44
            BETA  = 0.0
          ELSE
            PW(MTH) = RNUM / NUMDAY(MTH)
            PDW(MTH) = 0.75*PW(MTH)
            PWW = 1. - PDW(MTH)*(1.-PW(MTH))/PW(MTH)
            PWW = MIN(MAX(PWW,0.),1.)
            RWET  = RTOT / RNUM
C-GH        BETA  = MAX (-2.16+1.83*RWET, 0.0)
            BETA  = MAX (-2.16+1.83*RWET, 0.01)
            ALPHA = MIN(MAX(RWET/BETA, 0.01),0.998)
          ENDIF
          Y(MTH,11) = ALPHA
          Y(MTH,12) = BETA
          Y(MTH,13) = PDW(MTH)
          Y(MTH,14) = PWW
   10   CONTINUE
      ELSE
        CALL ERROR(ERRKEY,-1,FILEW,LNUM)
      ENDIF
      PWMN = AVG(PW,12)
      PDWMN = AVG(PDW,12)

C     Calculate solar radiation parameters (wet and dry days).
C     Coefficient calculations in Langleys then converted to MJ/m2/d.

      CALL FOURCF (SRAMN,SRAMNF)
      SAVG = 23.87*SRAMNF(1)
      SDIFF = ABS(410-3.21*XLAT-0.350*SAVG)
      SRDMNF(1) = (SAVG+PWMN*SDIFF) * 0.04189
      SRDMNF(2) = SRAMNF(2)
      SRDMNF(3) = SRAMNF(3)
      SRWMNF(1) = (SAVG-(1.0-PWMN)*SDIFF) * 0.04189
      SRWMNF(2) = SRAMNF(2)
      SRWMNF(3) = SRAMNF(3)
      SRDCVF(1) = 0.24
      SRDCVF(2) = -0.08
      SRDCVF(3) = SRAMNF(3)
      SRWCVF(1) = 0.48
      SRWCVF(2) = -0.13
      SRWCVF(3) = SRAMNF(3)

C     Calculate mean maximum temperature parameters (wet and dry days).
C     TDIFF calculations in Langleys then converted to MJ/m2/d.

      CALL FOURCF (TXAMN,TXAMNF)
      TDIFF = ABS(9.67-27.4*PDWMN) * 5./9.
      TXDMNF(1) = TXAMNF(1) + PWMN*TDIFF
      TXDMNF(2) = TXAMNF(2)
      TXDMNF(3) = TXAMNF(3)
      TXWMNF(1) = TXAMNF(1) - (1.-PWMN)*TDIFF
      TXWMNF(2) = TXAMNF(2)
      TXWMNF(3) = TXAMNF(3)

C     Compute monthly SDEVs from mean and CV, then compute fourier coefficients.

      DO 20 MTH=1,12
        SRDMN = FOURIN (SRDMNF,MTH)
        SRWMN = FOURIN (SRWMNF,MTH)
        SRDCV = MAX (FOURIN (SRDCVF,MTH), 0.06)
        SRWCV = MAX (FOURIN (SRWCVF,MTH), 0.06)
        SRDSD = SRDMN * SRDCV
        SRWSD = SRWMN * SRWCV

        TXDMN = FOURIN (TXDMNF,MTH)
        TXWMN = FOURIN (TXWMNF,MTH)
        TXDSD = MAX (5.8-0.09*TXDMN, 0.5)
        TXWSD = MAX (5.8-0.09*TXWMN, 0.5)
        TNASD = MAX (5.2-0.13*TNAMN(MTH), 0.5)
        Y(MTH,1) = SRDMN
        Y(MTH,2) = SRDSD
        Y(MTH,3) = SRWMN
        Y(MTH,4) = SRWSD
        Y(MTH,5) = TXDMN
        Y(MTH,6) = TXDSD
        Y(MTH,7) = TXWMN
        Y(MTH,8) = TXWSD
        Y(MTH,9) = TNAMN(MTH)
        Y(MTH,10) = TNASD
   20 CONTINUE

      CALL CFACT(Y,CF)
      CLOSE(LUNCLI)

      END

C=======================================================================
C  WGENPM, Subroutine, N.B. Pickering
C  Estimates daily generator parameters
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/02/92 NBP Written for DSSAT V3.
C-----------------------------------------------------------------------
C  Input : YR,DOY,Y,CF,MTH,MM,
C  Output: TXDMN,TXDSD,TXWMN,TXWSD,TNAMN,TNASD,SRDMN,SRDSD,SRWMN,SRWSD,
C          PWW,PDW,ALPHA,BETA
C  Local :
C=======================================================================
      SUBROUTINE WGENPM (YR,DOY,Y,CF,MTH,MM,TXDMN,TXDSD,TXWMN,TXWSD,
     &  TNAMN,TNASD,SRDMN,SRDSD,SRWMN,SRWSD,PWW,PDW,ALPHA,BETA)

      IMPLICIT NONE

      INTEGER CIRC,DOY,ENDYR,I,N,M,MTH,MM,YR,CC
      PARAMETER (N=12, M=14)
      REAL SRDMN,SRDSD,SRWMN,SRWSD,TXDMN,TXDSD,TXWMN,TXWSD,TNAMN,
     &  TNASD,PWW,PDW,ALPHA,BETA,Y(N,M),CF(N,M),XP,YP(M)

      XP = DOY / REAL(ENDYR(YR)) * 12.

      CALL LININT(MTH,Y,XP,YP)

      CC = MTH+MM-1
      I = CIRC(CC,1,N)
      SRDMN = YP(1) + CF(I,1)
      SRDSD = YP(2) + CF(I,2)
      SRWMN = YP(3) + CF(I,3)
      SRWSD = YP(4) + CF(I,4)
      TXDMN = YP(5) + CF(I,5)
      TXDSD = YP(6) + CF(I,6)
      TXWMN = YP(7) + CF(I,7)
      TXWSD = YP(8) + CF(I,8)
      TNAMN = YP(9) + CF(I,9)
      TNASD = YP(10) + CF(I,10)
      ALPHA = MIN(YP(11)+CF(I,11),0.998)
      BETA  = MAX(YP(12)+CF(I,12),1.1)
      PDW = MIN(MAX(YP(13)+CF(I,13),0.),1.)
      PWW = MIN(MAX(YP(14)+CF(I,14),0.),1.)

      END

C=======================================================================
C  WGEN, Subroutine, Richardson and Wright
C  Generates daily weather.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/84     Written.
C  09/02/92 NBP Modified for DSSAT V3.
C  06/29/94  JH Added lower limit of 0.1 to calc. of rainfall.
C-----------------------------------------------------------------------
C  Input : DOY,RSEED,XLAT,SRDMN,SRDSD,SRWMN,SRWSD,TXDMN,TXDSD,TXWMN,
C          TXWSD,TNAMN,TNASD,PDW,PWW,ALPHA,BETA,WETPRV,XPRV
C  Output: SRAD,TMAX,TMIN,RAIN,PAR
C  Local :
C=======================================================================

      SUBROUTINE WGENGN (DOY,RSEED,XLAT,SRDMN,SRDSD,SRWMN,SRWSD,
     &  TXDMN,TXDSD,TXWMN,TXWSD,TNAMN,TNASD,PDW,PWW,ALPHA,BETA,
     &  WETPRV,XPRV,SRAD,TMAX,TMIN,RAIN,PAR)

      IMPLICIT NONE

      INTEGER DOY,RSEED(4),I,WET,WETPRV,J,K
      REAL SRDMN,SRDSD,SRWMN,SRWSD,TXDMN,TXDSD,TXWMN,TXWSD,TNAMN,TNASD,
     &  PWW,PDW,ALPHA,BETA,RADA,A(3,3),B(3,3),XPRV(3),E(3),R(3),X(3),
     &  RR(3),XLAT,AA,AB,RAIN,RC,SRMN,RMIN,RN,RN1,RN2,RN3,SRSD,S1,S2,
     &  PAR,SRAD,TMAX,TMIN,HOLD,TR1,TR2,TXXMN,TXXSD,V,Z,PI,PARFAC
      PARAMETER (PI=3.14159, PARFAC=2.0)
      DATA A/0.567,0.253,-0.006,0.086,0.504,-0.039,-0.002,-0.050,0.244/
      DATA B/0.781,0.328,0.238,0.0,0.637,-0.341,0.0,0.0,0.873/

C     Calculate maximum solar radiation for each day (langleys).

      RC = RADA(DOY,XLAT) * 0.8
      RMIN = 0.2*RC

C     Determine wet or dry day using Markov chain model.

      IF (RAIN .EQ. -99.) THEN
        CALL RANDN (RSEED,RN)
        IF ((WETPRV.EQ.0 .AND. RN.LT.PDW) .OR. (WETPRV.EQ.1 .AND.
     &    RN.LT.PWW)) THEN

C         Determine rainfall amount for wet days using Gamma distribution

          AA = 1. / ALPHA
          AB = 1. / (1.-ALPHA)
          TR1 = EXP(-18.42/AA)
          TR2 = EXP(-18.42/AB)
          S1 = 1.
          S2 = 1.
   10     IF (S1+S2 .GT. 1.) THEN
            CALL RANDN (RSEED,RN1)
            CALL RANDN (RSEED,RN2)
            IF (RN1 .LE. TR1) THEN
              S1 = 0.
            ELSE
              S1 = RN1**AA
            ENDIF
            IF (RN2 .LE. TR2) THEN
              S2 = 0.
            ELSE
              S2 = RN2**AB
            ENDIF
          GOTO 10
          ENDIF
          Z = S1 / (S1+S2)
          CALL RANDN (RSEED,RN3)
          RAIN = MAX(-Z*ALOG(RN3)*BETA,0.1)
        ELSE
          RAIN = 0.
        ENDIF
      ENDIF
      IF (RAIN .EQ. 0.) THEN
        WET = 0
      ELSE
        WET = 1
      ENDIF

C     Generate TMAX, TMIN, and SRAD.

      IF (WET .EQ. 0) THEN
        SRMN = SRDMN
        SRSD = SRDSD
        TXXMN = TXDMN
        TXXSD = TXDSD
      ELSE
        SRMN = SRWMN
        SRSD = SRWSD
        TXXMN = TXWMN
        TXXSD = TXWSD
      ENDIF
      IF (TMAX .EQ. -99. .OR. TMIN .EQ. -99. .OR. SRAD .EQ. -99.) THEN
        K = 1
   20   IF (K .LE. 3) THEN
          CALL RANDN (RSEED,RN1)
          CALL RANDN (RSEED,RN2)
          V = SQRT(-2.*ALOG(RN1))*COS(2.*PI*RN2)
          IF (ABS(V) .LE. 2.5) THEN
            E(K) = V
            K = K + 1
          ENDIF
        GOTO 20
        ENDIF
        DO 30 I = 1,3
          R(I) = 0.
          RR(I) = 0.
   30   CONTINUE
        DO 50 I = 1,3
          DO 40 J = 1,3
            R(I) = R(I) + B(I,J)*E(J)
            RR(I) = RR(I) + A(I,J)*XPRV(J)
   40     CONTINUE
   50   CONTINUE
        DO 60 K = 1,3
          X(K) = R(K) + RR(K)
   60   CONTINUE
      ENDIF
      IF (TMAX .EQ. -99.) THEN
        TMAX = X(1)*TXXSD + TXXMN
      ELSE
        X(1) = (TMAX-TXXMN) / TXXSD
      ENDIF
      IF (TMIN .EQ. -99.) THEN
        TMIN = X(2)*TNASD + TNAMN
      ELSE
        X(2) = (TMIN-TNAMN) / TNASD
      ENDIF
      IF (SRAD .EQ. -99.) THEN
        SRAD  = X(3)*SRSD + SRMN
        SRAD = MIN(MAX(SRAD,RMIN),RC)
      ELSE
        X(3) = (SRAD-SRMN) / SRSD
      ENDIF
      IF (PAR .EQ. -99.) PAR = SRAD * PARFAC
      IF (TMIN .GT. TMAX) THEN
        HOLD = TMAX
        TMAX = TMIN
        TMIN = HOLD
      ENDIF

C     Update values to be remembered.

      WETPRV = WET
      DO 70 K = 1,3
        XPRV(K) = X(K)
   70 CONTINUE

      END

C=======================================================================
C  RANDN, Subroutine, Richardson and Wright
C  Generates a uniform random number (0-1)
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/84     Written.
C  09/02/92 NBP Modified for DSSAT V3.
C-----------------------------------------------------------------------
C  Input : RSEED
C  Output: RSEED, RN
C  Local :
C=======================================================================

      SUBROUTINE RANDN (RSEED,RN)

      IMPLICIT NONE

      INTEGER I,RSEED(4)
      REAL RN

      RSEED(4) = 3*RSEED(4)+RSEED(2)
      RSEED(3) = 3*RSEED(3)+RSEED(1)
      RSEED(2) = 3*RSEED(2)
      RSEED(1) = 3*RSEED(1)
      I = RSEED(1)/1000
      RSEED(1) = RSEED(1)-I*1000
      RSEED(2) = RSEED(2) + I
      I = RSEED(2)/100
      RSEED(2) = RSEED(2)-100*I
      RSEED(3) = RSEED(3)+I
      I = RSEED(3)/1000
      RSEED(3) = RSEED(3)-I*1000
      RSEED(4) = RSEED(4)+I
      I = RSEED(4)/100
      RSEED(4) = RSEED(4)-100*I
      RN = (((RSEED(1)*.001+RSEED(2))*.01+RSEED(3))*.001+RSEED(4))*.01

      END

C=======================================================================
C  LININT, Subroutine, N.B. Pickering
C  Linearly interpolates between monthly values.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/02/92 NBP Written for DSSAT V3.
C-----------------------------------------------------------------------
C  Input : MTH,Y,XP
C  Output: YP
C  Local :
C=======================================================================

      SUBROUTINE LININT(MTH,Y,XP,YP)

      IMPLICIT NONE

      INTEGER J,N,M,MTH,PRV,CIRC,CC

      PARAMETER (N=12, M=14)

      REAL DIFX,X,Y(N,M),XP,YP(M)

      CC  = MTH - 1
      PRV = CIRC(CC,1,N)
      X = MTH - 1.5
      DIFX = XP - X
      IF (DIFX .GT. N) DIFX = DIFX - N
      DO J = 1, M
        YP(J) = Y(PRV,J) + (Y(MTH,J)-Y(PRV,J))*DIFX
      END DO

      END

C=======================================================================
C  CFACT, Subroutine, N.B. Pickering
C  Calculates correction factor for month based on three consecutive
C  monly values assuming daily values were linearly interpolated.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/02/92 NBP Written for DSSAT V3.
C-----------------------------------------------------------------------
C  Input : Y
C  Output: CF
C  Local :
C=======================================================================

      SUBROUTINE CFACT(Y,CF)

      IMPLICIT NONE

      INTEGER I,J,N,M,NXT,PRV,CIRC,CC

      PARAMETER (N=12, M=14)

      REAL Y(N,M),CF(N,M)

      DO J = 1, M
        DO I = 1, N
          CC = I - 1
          PRV = CIRC(CC,1,N)
          CC = I + 1
          NXT = CIRC(CC,1,N)
          CF(I,J) = Y(I,J) - (0.25*(Y(PRV,J)+2.*Y(I,J)+Y(NXT,J)))
        END DO
      END DO

      END

C=======================================================================
C  CIRC, Integer Function, N.B. Pickering
C  Creates circular sequence from L to U eg. 1,2,3,1,2,3 etc.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/02/92 NBP Written for DSSAT V3.
C-----------------------------------------------------------------------
C  Input : I, L, U
C  Output: CIRC
C  Local :
C=======================================================================

      INTEGER FUNCTION CIRC(I,L,U)

      IMPLICIT NONE

      INTEGER I,L,U

      IF (I .LT. L) THEN
        I = I + (U-L+1.)
      ELSE IF (I .GT. U) THEN
        I = I - (U-L+1.)
      ENDIF
      CIRC = I

      END

C=======================================================================
C  AVG, Real Function, N. B. Pickering
C  Calculates average of array.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  10/11/90 NBP Written.
C-----------------------------------------------------------------------
C  Input : X, N
C  Output: AVG
C  Local :
C=======================================================================

      REAL FUNCTION AVG(X,N)

      IMPLICIT NONE

      INTEGER I,N
      REAL X(N),SUM

      SUM = 0.
      DO 10 I=1,N
        SUM = SUM + X(I)
10    CONTINUE
      AVG = SUM / N

      END

C=======================================================================
C  FOURCF, Subroutine, N. B. Pickering
C  Computes coefficients for 1ST order Fourier eqn. using AMP and
C  PHASE shift form.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/02/92 NBP Written for DSSAT V3.
C-----------------------------------------------------------------------
C  Input : YDATA
C  Output: COEFF
C  Local :
C=======================================================================

      SUBROUTINE FOURCF(YDATA,COEFF)

      IMPLICIT NONE

      INTEGER M
      REAL COEFF(3),A1,A2,T,PI,RAD,SUM0,SUM1,SUM2,X,YDATA(12)
      PARAMETER (PI=3.14159, RAD=2.*PI/12.)

      SUM0 = 0.0
      SUM1 = 0.0
      SUM2 = 0.0
      DO 40 M = 1,12
        X = RAD * (M-0.5)
        SUM0 = SUM0 + YDATA(M)
        SUM1 = SUM1 + YDATA(M)*COS(X)
        SUM2 = SUM2 + YDATA(M)*SIN(X)
   40 CONTINUE
      A1  = SUM1 / 6.
      A2  = SUM2 / 6.
      T = ATAN2(A2,A1)
      IF (T .LT. 0.) T = T + 2.*PI
      COEFF(1) = SUM0 / 12.
      COEFF(2) = A1 / COS(T)
      COEFF(3) = T

      END

C=======================================================================
C  FOURIN, Subroutine, N. B. Pickering
C  Calculates a monthly value using 1st order Fourier eqn.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/02/92 NBP Written for DSSAT V3.
C-----------------------------------------------------------------------
C  Input : COEFF, M
C  Output: FOURIN
C  Local :
C=======================================================================

      REAL FUNCTION FOURIN(COEFF,M)

      IMPLICIT NONE

      INTEGER M
      REAL COEFF(3),PI,RAD,X
      PARAMETER (PI=3.14159, RAD=2.*PI/12.)

      X = RAD * (M-0.5)
      FOURIN = COEFF(1) + COEFF(2)*COS(X-COEFF(3))

      END

C=======================================================================
C  RADA, Real Function, N.B. Pickering
C  Computes extraterrestrial radiation (Spitters, 1986).  Accuracy 0.3%
C  based on Tables in List (1984) adjusted for new SC value 1370 W/m2.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/02/92 NBP Written for DSSAT V3.
C-----------------------------------------------------------------------
C  Input : DOY, XLAT
C  Output: RADA
C  Local :
C=======================================================================

      REAL FUNCTION RADA(DOY,XLAT)

      IMPLICIT NONE

      INTEGER DOY
      REAL C1,CCOS,DAYL,DEC,DSINB,PI,RAD,S0D,SC,SOC,SSIN,T,XLAT
      PARAMETER (PI = 3.14159, RAD = PI/180.)

C     Initialization

      T = 2.*PI*(DOY+10.)/365.
      C1 = COS(T)

C     Calculation of declination of sun (Eqn. 16) and sun angles.
C     SOC limited for latitudes above polar circles.

      DEC = - 23.45 * C1
      SSIN = SIN(RAD*DEC) * SIN(RAD*XLAT)
      CCOS = COS(RAD*DEC) * COS(RAD*XLAT)
      SOC = SSIN / CCOS
      SOC = MIN(MAX(SOC,-1.),1.)

C     Calculate daylength (Eqn. 17), integral of SINB (s) (Eqn. 18),
C     solar constant (Eqn. 1).

      DAYL = 12. + 24.*ASIN(SOC)/PI
      DSINB = 3600. * (DAYL*SSIN + 24./PI*CCOS*SQRT(1.-SOC**2))
      SC = 1370. * (1. + 0.033*COS(2.*PI*DOY/365.))

C     Calculate extra-terrestial radiation.

      S0D = SC * DSINB
      RADA = S0D / 1.E6

      END
