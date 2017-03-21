C=======================================================================
C  INSEQ, Subroutine, G. Hoogenboom, W. T. Bowen
C-----------------------------------------------------------------------
C
C  Input of temporary data for sequencing and crop rotations
C
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/09/89 GH  Written.
C  01/19/96 GH  Modified for residue carry over
C  07/12/96 GH  Add chemical and tillage applications
C  09/04/96 GH  Fix common blocks and redo initialization
C  06/11/98 GH  Fix date change problem across years
C-----------------------------------------------------------------------
C  Input  :
C  In/Out :
C  Output :
C  Saved  :
C  Local  :
C  Fn/Sub :
C-----------------------------------------------------------------------
C  Called : CROPGRO, CERES
C
C  Calls  : None
C=======================================================================

      SUBROUTINE INSEQ(WRESR,YRSIM,NREP,RSEED,NLAYR,DEPMAX,ESW,
     &                 SUMES1,SUMES2,TLL,PESW,CUMDEP,TSAT,SWDEF,T,TSW,
     &                 ATHETA,SW,DLAYR,GNP,CNSD1,CNSD2,ATANC,ANFAC,
     &                 DMINR,FPOOL,WFY,TFY,PHN,CNI,FOM,FON,HUM,NHUM,
     &                 UREA,UPPM,ATOT,BD,PH,OC,NO3,TMA,NH4,FAC,SNO3,
     &                 SNH4,ANO3,ANH4,TIFON,TIFOM,TSON,EFNFIX,EFINOC,
     &                 PRCHO,PRCEL,PRLIG)

      IMPLICIT NONE

      INTEGER I,IOUT,YRSIM,NREP,RSEED(4),NLAYR

      REAL STOVRL,APTNPL,RTWTL,RTWTNL,RNKG,RESAMT,RESNIT,RESDEP
      REAL WSUM,DEPTH,FACTOR,HOLD,FR,ADD,AINO3,AINH4,WRESR
      REAL WRN(20),PRCHO,PRCEL,PRLIG
      REAL DEPMAX,ESW(20),SUMES1,SUMES2,TLL,PESW,CUMDEP,TSAT,SWDEF
      REAL T,TSW,ATHETA,SW(20),DLAYR(20)
      REAL GNP,CNSD1,CNSD2,ATANC,ANFAC
      REAL DMINR,FPOOL(20,3),WFY(20),TFY(20),PHN(20),CNI(20),FOM(20)
      REAL FON(20),HUM(20),NHUM(20),UREA(20),UPPM(20),ATOT,BD(20)
      REAL PH(20),OC(20),NO3(20),TMA(5),NH4(20),FAC(20),SNO3(20)
      REAL SNH4(20),ANO3,ANH4,TIFON,TIFOM,TSON,EFNFIX,EFINOC

C-----------------------------------------------------------------------
C     SEQUENCE RUNS
C
C     Read variables passed from previous model
C-----------------------------------------------------------------------

      DO I = 1, 20
         WRN(I) = 0.0
      END DO

C-----------------------------------------------------------------------

      OPEN(UNIT=29,FILE='TMP.DAT',STATUS='OLD')

      READ (29,*) YRSIM,NREP
      READ (29,*) STOVRL, APTNPL
      READ (29,*) RTWTL, RTWTNL, DEPMAX,NLAYR
      READ (29,*) ESW
      READ (29,*) SUMES1, SUMES2, T, TLL, PESW, TSW, CUMDEP
      READ (29,*) TSAT, SWDEF, ATHETA
      READ (29,*) GNP, CNSD1, CNSD2, ATANC, ANFAC, DMINR
      READ (29,*) FPOOL
      READ (29,*) WFY
      READ (29,*) TFY
      READ (29,*) PHN
      READ (29,*) CNI
      READ (29,*) FOM
      READ (29,*) FON
      READ (29,*) HUM
      READ (29,*) NHUM
      READ (29,*) UREA
      READ (29,*) UPPM
      READ (29,*) TMA
      READ (29,*) ATOT
      READ (29,*) RSEED

      DO I = 1, NLAYR
         READ (29,*) SW(I),BD(I),PH(I),OC(I)
      END DO

      DO I = 1, NLAYR
         READ (29,*) NO3(I),NH4(I),FAC(I),SNO3(I),SNH4(I)
      END DO

      CLOSE (29)
C-----------------------------------------------------------------------
C     Adjust internal pools for residue carryover
C-----------------------------------------------------------------------

      RESAMT     = STOVRL
      WRESR      = RTWTL

C-----------------------------------------------------------------------
C     Calculate N contribution from previous crop residue
C-----------------------------------------------------------------------

      RNKG       = RTWTNL

C-----------------------------------------------------------------------
C     Calculations to set RESNIT
C-----------------------------------------------------------------------

      IF (STOVRL .GT. 0.0) THEN
         RESNIT = MAX(APTNPL / STOVRL * 100.0,0.01)
       ELSE
         RESNIT = 0.0
      ENDIF
      RESDEP = 20.0

C-----------------------------------------------------------------------
C     Distribute root mass
C-----------------------------------------------------------------------

      WSUM  = 0.0
      DEPTH = 0.0
      DO I = 1, NLAYR
        DEPTH  = DEPTH + DLAYR(I)
        WRN(I) = EXP(-3.0*DEPTH/DEPMAX)
        WSUM   = WSUM  + WRN(I)
      END DO

      ANO3 = 0.0
      ANH4 = 0.0
      DO I = 1, NLAYR
         IF (WSUM .GT. 0.0) THEN
            FACTOR = WRN(I)/WSUM
          ELSE
            FACTOR = 0.0
         ENDIF
         FOM(I)    =  FOM(I)     + WRESR * FACTOR
         FON(I)     = FON(I)     + RNKG  * FACTOR
         FPOOL(I,1) = FPOOL(I,1) + WRESR * FACTOR * PRCHO   ! 0.20
         FPOOL(I,2) = FPOOL(I,2) + WRESR * FACTOR * PRCEL   ! 0.70
         FPOOL(I,3) = FPOOL(I,3) + WRESR * FACTOR * PRLIG   ! 0.10
      END DO

      DEPTH = 0.0
      IOUT  = 1

      DO I = 1, NLAYR
         HOLD  = DEPTH
         DEPTH = DEPTH + DLAYR(I)
         IF (RESDEP .LE. DEPTH) THEN
            FR = MAX (0.0,(RESDEP-HOLD))/RESDEP
            IF (I .EQ. 1) THEN
               FR = 1
            ENDIF
            IOUT = 2
          ELSE
            FR = DLAYR(I)/RESDEP
         ENDIF

         ADD        = RESAMT * FR
         FOM(I)     = FOM(I) + ADD
         FPOOL(I,1) = FPOOL(I,1) + ADD * PRCHO         ! 0.20
         FPOOL(I,2) = FPOOL(I,2) + ADD * PRCEL         ! 0.70
         FPOOL(I,3) = FPOOL(I,3) + ADD * PRLIG         ! 0.10
         FON(I)     = FON(I) + ADD * RESNIT/100.0
         IF (IOUT .EQ. 2) THEN
            GO TO 450
         ENDIF
      END DO

 450  CONTINUE

      TIFOM = 0.0
      TIFON = 0.0
      TSON  = 0.0
      DO I = 1, NLAYR
         TIFOM = TIFOM + FOM(I)
         TIFON = TIFON + FON(I)
         TSON  = TSON  + NHUM(I)
         ANO3  = ANO3  + SNO3(I)
         ANH4  = ANH4  + SNH4(I)
      END DO

      AINO3 = ANO3
      AINH4 = ANH4

      IF (EFINOC .LE. 0.0) THEN
        EFINOC = 1.0
      ENDIF
      IF (EFNFIX .LE. 0.0) THEN
        EFNFIX = 1.0
      ENDIF

C-----------------------------------------------------------------------
C     Set carryovers back to 0
C-----------------------------------------------------------------------

      WRESR       = 0.0
      STOVRL      = 0.0

C-----------------------------------------------------------------------
C     End of inputs passed from previous model for sequencing
C-----------------------------------------------------------------------

      RETURN
      END

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      SUBROUTINE DATECS(FILEW,YRSIM,YRPLT,
     &  JULWTB,JULAPL,NAPW,RESDAY,NRESAP,FDAY,NFERT,NTBL,
     &  HLATE,PWDINF,PWDINL,NHAR,HDATE,MEWTH,CROP,
     &  IIRRI,IFERI,IHARI,IRESI,ISWCHE,ISWTIL,NCHEM,NTIL,
     &  CDATE,TDATE)
C-----------------------------------------------------------------------
C    This subroutine resets values when
C    a new weather year is selected interactively
C-----------------------------------------------------------------------
      IMPLICIT NONE

      CHARACTER*1  MEWTH,IIRRI,IFERI,IHARI,IRESI,ISWCHE,ISWTIL
      CHARACTER*2  CROP
      CHARACTER*6  ERRKEY
      CHARACTER*12 FILEW

      INTEGER YRSIM,YRPLT,YRDIF,IPLT,YR,NAPW,NRESAP,NFERT,NYEAR
      INTEGER FDAY(25),JULWTB(150),JULAPL(150),RESDAY(10),I,IDATE
      INTEGER NTBL,ISIM,HLATE,PWDINF,PWDINL,NHAR,HDATE(3),YRO,YRS
      INTEGER NCHEM,NTIL,CDATE(10),TDATE(10)

      PARAMETER (ERRKEY = 'DATECS')

      CALL YR_DOY(YRSIM,YRS,ISIM)

      NYEAR = (ICHAR(FILEW(7:7)) - 48)*10 + (ICHAR(FILEW(8:8)) - 48 )
      IF (NYEAR .EQ. 1) THEN
        IF (MEWTH .EQ. 'M' .OR. MEWTH .EQ. 'G') THEN
          WRITE(FILEW(5:6),5) MOD(YRS,100)
    5     FORMAT(I2.2)
        ENDIF
      ENDIF

      IF (CROP .EQ. 'FA') THEN
        IF (IHARI .NE. 'D') THEN
          CALL YR_DOY(HDATE(1),YRO,IDATE)
          YRDIF    =  YRS - YRO
          HDATE(1) = (YRO + YRDIF) * 1000 + IDATE
          IF (HDATE(1) .LT. YRSIM) THEN
            YRDIF = YRDIF + 1
            HDATE(1) = (YRO + YRDIF) * 1000 + IDATE
          ENDIF
        ENDIF
      ELSE
        CALL YR_DOY(YRPLT,YRO,IPLT)
        YRDIF = YRS - YRO
        YRPLT = (YRO + YRDIF) * 1000 + IPLT
        IF (YRPLT .LT. YRSIM) THEN
          YRPLT = (YRO + YRDIF + 1) * 1000 + IPLT
        ENDIF
        IF (NHAR .GT. 0 .AND. HDATE(1) .LT. YRSIM .AND. IHARI .NE. 'D')
     &      THEN
          DO I = 1, NHAR
            CALL YR_DOY(HDATE(I),YR,IDATE)
            YRDIF = YRS - YR
            HDATE(I) = (YR + YRDIF) * 1000 + IDATE
            IF (HDATE(I) .LT. YRSIM) THEN
              HDATE(I) = (YR + YRDIF + 1) * 1000 + IDATE
            ENDIF
          END DO
        ENDIF
      ENDIF

      IF (NAPW .GT. 0 .AND. JULAPL(1) .LT. YRSIM .AND. IIRRI .NE. 'D')
     &  THEN
        DO I = 1, NAPW
          CALL YR_DOY(JULAPL(I),YR,IDATE)
          YRDIF = YRS - YR
          CALL YR_DOY(JULAPL(I),YR,IDATE)
          JULAPL(I) = (YR + YRDIF) * 1000 + IDATE
          IF (JULAPL(I) .LT. YRSIM) THEN
             JULAPL(I) = (YR + YRDIF + 1) * 1000 + IDATE
          ENDIF
        END DO
      ENDIF

      IF (NTBL .GT. 0 .AND. JULWTB(1) .LT. YRSIM .AND. IIRRI .NE. 'D')
     &  THEN
        DO I = 1, NTBL
          CALL YR_DOY(JULWTB(I),YR,IDATE)
          YRDIF = YRS - YR
          CALL YR_DOY(JULWTB(I),YR,IDATE)
          JULWTB(I) = (YR + YRDIF) * 1000 + IDATE
          IF (JULWTB(I) .LT. YRSIM) THEN
             JULWTB(I) = (YR + YRDIF + 1) * 1000 + IDATE
          ENDIF
        END DO
      ENDIF

      IF (NFERT .GT. 0 .AND. FDAY(1) .LT. YRSIM .AND. IFERI .NE. 'D')
     &   THEN
        DO I = 1, NFERT
          CALL YR_DOY(FDAY(I),YR,IDATE)
          YRDIF = YRS - YR
          FDAY(I) = (YR + YRDIF) * 1000 + IDATE
          IF (FDAY(I) .LT. YRSIM) THEN
             FDAY(I) = (YR + YRDIF + 1) * 1000 + IDATE
          ENDIF
        END DO
      ENDIF

      IF (NRESAP .GT. 0 .AND. RESDAY(1) .LT. YRSIM .AND. IRESI .NE. 'D')
     &   THEN
        DO I = 1, NRESAP
          CALL YR_DOY(RESDAY(I),YR,IDATE)
          YRDIF = YRS - YR
          RESDAY(I) = (YR + YRDIF) * 1000 + IDATE
          IF (RESDAY(I) .LT. YRSIM) THEN
             RESDAY(I) = (YR + YRDIF + 1) * 1000 + IDATE
          ENDIF
        END DO
      ENDIF

      IF (NTIL .GT. 0 .AND. TDATE(1) .LT. YRSIM .AND. ISWTIL .NE. 'N')
     &   THEN
        DO I = 1, NTIL
          CALL YR_DOY(TDATE(I),YR,IDATE)
          YRDIF = YRS - YR
          TDATE(I) = (YR + YRDIF) * 1000 + IDATE
          IF (TDATE(I) .LT. YRSIM) THEN
             TDATE(I) = (YR + YRDIF + 1) * 1000 + IDATE
          ENDIF
        END DO
      ENDIF

      IF (NCHEM .GT. 0 .AND. CDATE(1) .LT. YRSIM .AND. ISWCHE .NE. 'N')
     &   THEN
        DO I = 1, NCHEM
          CALL YR_DOY(CDATE(I),YR,IDATE)
          YRDIF = YRS - YR
          CDATE(I) = (YR + YRDIF) * 1000 + IDATE
          IF (CDATE(I) .LT. YRSIM) THEN
             CDATE(I) = (YR + YRDIF + 1) * 1000 + IDATE
          ENDIF
        END DO
      ENDIF

      IF (HLATE .LT. YRSIM) THEN
        CALL YR_DOY(HLATE,YR,IDATE)
        YRDIF = YRS - YR
        HLATE = (YR +  YRDIF) * 1000 + IDATE
        IF (HLATE .LT. YRSIM) THEN
           HLATE = (YR + YRDIF + 1) * 1000 + IDATE
        ENDIF
      ENDIF

      IF (PWDINF .LT. YRSIM) THEN
        CALL YR_DOY(PWDINF,YR,IDATE)
        YRDIF = YRS - YR
        PWDINF = (YR +  YRDIF) * 1000 + IDATE
        IF (PWDINF .LT. YRSIM) THEN
           PWDINF = (YR + YRDIF + 1) * 1000 + IDATE
        ENDIF
      ENDIF

      IF (PWDINL .LT. YRSIM) THEN
        CALL YR_DOY(PWDINL,YR,IDATE)
        YRDIF = YRS - YR
        PWDINL = (YR +  YRDIF) * 1000 + IDATE
        IF (PWDINL .LT. YRSIM) THEN
           PWDINL = (YR + YRDIF + 1) * 1000 + IDATE
        ENDIF
      ENDIF

      END
