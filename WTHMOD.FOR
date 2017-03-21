C=======================================================================
C  WTHMOD, Subroutine, N.B. Pickering
C  Adjust weather variables according to selections made in WTHMDI
C  and stored in WTHADJ.  Variables are adjusted by an offset an
C  multiplier.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/04/91 NBP Written
C  01/30/98 GH  Added WTHMDB
C-----------------------------------------------------------------------
C  Input : WTHADJ (weather modification array)
C          CO2,PAR,RAIN,SNUP,SNDN,SRAD,TMAX,TMIN,TDEW,WINDSP
C  Output: CO2,PAR,RAIN,SNUP,SNDN,SRAD,TMAX,TMIN,TDEW,WINDSP
C=======================================================================

      SUBROUTINE WTHMOD(FILEW,LINWTH,WTHADJ,XLAT,DEC,CO2,DAYL,PAR,
     &  RAIN,SNUP,SNDN,SRAD,TDEW,TMAX,TMIN,WINDSP)

      IMPLICIT NONE
      CHARACTER ERRKEY*6,FILEW*12
      INTEGER LINWTH
      REAL CO2,CO2BAS,DEC,DECL,DAYL,HOLD,PAR,RAIN,SNUP,SNDN,SRAD,
     &  TDEW,TMAX,TMIN,WINDSP,WTHADJ(2,8),XLAT
      PARAMETER ( CO2BAS=330., ERRKEY='WTHMOD' )

C     Adjustment of DAY LENGTH.  Effective DOY is calculated so calcs.
C     of SNUP, SNDN, and FRACD in SOLAR and HMET are correct.

      IF (WTHADJ(1,1) .NE. 0. .OR. WTHADJ(2,1) .NE. 1.) THEN
        DAYL = WTHADJ(1,1) + DAYL*WTHADJ(2,1)
        SNUP = 12.0 - DAYL/2.0
        SNDN = 12.0 + DAYL/2.0
        DEC = DECL(DAYL,XLAT)
      ENDIF

C     Adjustment of radiation.  PAR and SRAD are automatically
C     adjusted together.

      IF (WTHADJ(1,2) .NE. 0. .OR. WTHADJ(2,2) .NE. 1.) THEN
        HOLD = WTHADJ(1,2) + SRAD*WTHADJ(2,2)
        IF (PAR .GT. 0.) PAR = PAR * HOLD/SRAD
        SRAD = HOLD
      ENDIF

C     Adjustment of maximum and minimum temperature.

      IF (WTHADJ(1,3) .NE. 0. .OR. WTHADJ(2,3) .NE. 1.)
     &  TMAX = WTHADJ(1,3) + TMAX*WTHADJ(2,3)

      IF (WTHADJ(1,4) .NE. 0. .OR. WTHADJ(2,4) .NE. 1.)
     &  TMIN = WTHADJ(1,4) + TMIN*WTHADJ(2,4)

C     Adjustment of precipitation.

      IF (WTHADJ(1,5) .NE. 0. .OR. WTHADJ(2,5) .NE. 1.)
     &  RAIN = WTHADJ(1,5) + RAIN*WTHADJ(2,5)

C     Adjustment of CO2.

      IF (WTHADJ(1,6) .NE. 0. .OR. WTHADJ(2,6) .NE. 1.)
     &  CO2 = WTHADJ(1,6) + CO2BAS*WTHADJ(2,6)

C     Adjustment of humidity (dew point).

      IF (WTHADJ(1,7) .NE. 0. .OR. WTHADJ(2,7) .NE. 1.)
     &  TDEW = WTHADJ(1,7) + TDEW*WTHADJ(2,7)

C     Adjustment of wind speed.

      IF (WTHADJ(1,8) .NE. 0. .OR. WTHADJ(2,8) .NE. 1.)
     &  WINDSP = WTHADJ(1,8) + WINDSP*WTHADJ(2,8)

      IF (CO2.LE.0. .OR. DAYL.LT.0. .OR. SRAD.LE.0. .OR. RAIN.LT.0. .OR.
     &  (TMAX.EQ.0. .AND. TMIN.EQ.0.)) CALL ERROR(ERRKEY,1,FILEW,LINWTH)

      END
C=======================================================================
C  WTHMDB, Subroutine, N.B. Pickering
C  Initialize weather modification array in BATCH mode.  Used to update
C  daily weather record. All variables are adjusted on input.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/04/91 NBP Written
C-----------------------------------------------------------------------
C  Input : WMOD
C  Output: WMOD,WTHADJ
C  Local :
C  Notes : WTHADJ is weather modification array (2X8) containing offset
C          and multiplier for each variable:
C          1=DAYL, 2=SRAD, 3=TMAX, 4=TMIN, 5=RAIN, 6=RH, 7=WIND, 8=CO2
C=======================================================================

      SUBROUTINE WTHMDB(CO2ADJ,CO2FAC,DAYADJ,DAYFAC,DPTADJ,
     &  DPTFAC,NEV,PRCADJ,PRCFAC,RADADJ,RADFAC,TMADJ,TMFAC,
     &  TXADJ,TXFAC,WMDATE,WNDADJ,WNDFAC,YRDOY,WMODB,WTHADJ)

      IMPLICIT NONE

      CHARACTER*1  WMODB
      CHARACTER*1  CO2FAC(10),DAYFAC(10),DPTFAC(10),PRCFAC(10)
      CHARACTER*1  TMFAC(10),TXFAC(10),RADFAC(10),WNDFAC(10)

      INTEGER I,NEV,WMDATE(10),YRDOY

      REAL
     &  CO2ADJ(10),DAYADJ(10),DPTADJ(10),PRCADJ(10),TMADJ(10),
     &  TXADJ(10),RADADJ(10),WNDADJ(10),WTHADJ(2,8)

C     Repeat until a weather modification (WM) date preceeds today's date.

      I = 1
   20 IF (YRDOY .GE. WMDATE(I) .AND. I .LE. NEV) THEN
        I = I + 1
      GO TO 20
      ENDIF
      I = I - 1
      IF (I .GE. 1 .AND. I .LE. NEV) THEN
        CALL WTHSET(DAYADJ(I),DAYFAC(I),1,WTHADJ)
        CALL WTHSET(RADADJ(I),RADFAC(I),2,WTHADJ)
        CALL WTHSET( TXADJ(I), TXFAC(I),3,WTHADJ)
        CALL WTHSET( TMADJ(I), TMFAC(I),4,WTHADJ)
        CALL WTHSET(PRCADJ(I),PRCFAC(I),5,WTHADJ)
        CALL WTHSET(CO2ADJ(I),CO2FAC(I),6,WTHADJ)
        CALL WTHSET(DPTADJ(I),DPTFAC(I),7,WTHADJ)
        CALL WTHSET(WNDADJ(I),WNDFAC(I),8,WTHADJ)
        WMODB = 'N'
        DO 30 I=1,8
          IF (WTHADJ(1,I) .NE. 0. .OR. WTHADJ(2,I) .NE. 1.) WMODB= 'Y'
   30   CONTINUE

      ENDIF

      END

C=======================================================================
C  WTHSET, Subroutine, N.B. Pickering
C  Update WTHADJ array.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/04/91 NBP Written
C-----------------------------------------------------------------------
C  Input : AMOUNT,TYPE,VNUM,WTHADJ
C  Output: WTHADJ
C=======================================================================

      SUBROUTINE WTHSET(AMOUNT,TYPE,VNUM,WTHADJ)

      IMPLICIT NONE

      CHARACTER*1 TYPE

      INTEGER VNUM

      REAL AMOUNT,WTHADJ(2,8)

C     Change: additive <> 0, multiplicative <> 1, constant > 0.

      IF (TYPE .EQ. 'A' .AND. AMOUNT .NE. 0.) THEN
        WTHADJ(1,VNUM) = AMOUNT
        WTHADJ(2,VNUM) = 1.
      ELSE IF (TYPE .EQ. 'S' .AND. AMOUNT .NE. 0.) THEN
        WTHADJ(1,VNUM) = -AMOUNT
        WTHADJ(2,VNUM) = 1.
      ELSE IF (TYPE .EQ. 'M' .AND. AMOUNT .NE. 1.) THEN
        WTHADJ(1,VNUM) = 0.
        WTHADJ(2,VNUM) = AMOUNT
      ELSE IF (TYPE .EQ. 'R' .AND. AMOUNT .GE. 0.) THEN
        WTHADJ(1,VNUM) = AMOUNT
        WTHADJ(2,VNUM) = 0.
      ELSE
        WTHADJ(1,VNUM) = 0.
        WTHADJ(2,VNUM) = 1.
      ENDIF

      END
