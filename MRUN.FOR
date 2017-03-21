C=======================================================================
C  MRUN, Subroutine
C
C  This subroutine resets YRDOY values for start of simulation date,
C  planting date, irrigation, fertilization, and residue applications.
C
C  This subroutine can also be used to reset values when a new weather
C  year is selected interactively
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       2-28-93
C-----------------------------------------------------------------------
C  INPUT  :
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called :
C
C  Calls  :
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE MRUN(FILEW,YRSIM,YRPLT,MULTI,YEAR,
     &  RESDAY,NRESAP,FDAY,NFERT,NIRR,MEWTH,NREP,
     &  HLATE,PWDINF,PWDINL,NHAR,HDATE,IDLAPL,PATHWT,
     &  IIRRI,IFERI,IHARI,IRESI,YRIC,TDATE,CDATE,ISWTIL,
     &  ISWCHE,NTIL,NCHEM)
C-----------------------------------------------------------------------
C    This subroutine resets YRDOY values for start of simulation date,
C    planting date, irrigation, fertilization, and residue applications.
C
C    This subroutine can also be used to reset values when
C    a new weather year is selected interactively
C-----------------------------------------------------------------------
      IMPLICIT NONE

      CHARACTER*1  MEWTH,BLANK,IIRRI,IFERI,IHARI,IRESI
      CHARACTER*1  ISWTIL,ISWCHE
      CHARACTER*6  ERRKEY
      CHARACTER*12 FILEW
      CHARACTER*80 PATHWT
      CHARACTER*92 FILEWW

      INTEGER YRSIM,YRPLT,MULTI,IPLT,ISIM,YR,YEAR,NRESAP,NFERT,WYEAR
      INTEGER FDAY(25),IDLAPL(150),RESDAY(10),I,IDATE,PATHL
      INTEGER NIRR,HLATE,PWDINF,PWDINL,NHAR,HDATE(3),NREP,NYEAR
      INTEGER IDAYIC,YRIC,TDATE(10),CDATE(10),NCHEM,NTIL

      LOGICAL      FEXIST

      PARAMETER (ERRKEY = 'MRUN  ')
      PARAMETER (BLANK = ' ')

      NREP = NREP + 1
      IF (MEWTH .EQ. 'M' .OR. MEWTH .EQ. 'G') THEN
        NYEAR = (ICHAR(FILEW(7:7)) - 48)*10 + (ICHAR(FILEW(8:8)) - 48 )
        WYEAR = (ICHAR(FILEW(5:5)) - 48)*10 + (ICHAR(FILEW(6:6)) - 48 )
        IF (NYEAR .EQ. 1) THEN
          PATHL  = INDEX(PATHWT,BLANK)
          YEAR = YEAR + (MULTI - 1)
          WYEAR = MOD((WYEAR + MULTI - 1),100)
          WRITE(FILEW(5:6),10) WYEAR
   10     FORMAT(I2.2)
          IF (PATHL .LE. 1) THEN
            FILEWW = FILEW
          ELSE
            FILEWW = PATHWT(1:(PATHL-1)) // FILEW
          ENDIF
          INQUIRE (FILE = FILEWW,EXIST = FEXIST)
          IF (.NOT. FEXIST) CALL ERROR (ERRKEY,1,FILEW,0)
        ENDIF
      ENDIF
      CALL YR_DOY(YRSIM,YR,ISIM)
      YRSIM = (YR + MULTI - 1) * 1000 + ISIM
      CALL YR_DOY(YRPLT,YR,IPLT)
      YRPLT = (YR + MULTI - 1) * 1000 + IPLT
      CALL YR_DOY(YRIC,YR,IDAYIC)
      YRIC  = (YR + MULTI - 1) * 1000 + IDAYIC
      IF (NIRR .GT. 0 .AND. IIRRI .NE. 'D') THEN
      DO I = 1, NIRR
        CALL YR_DOY(IDLAPL(I),YR,IDATE)
        IDLAPL(I) = (YR + MULTI - 1) * 1000 + IDATE
      END DO
      ENDIF
      IF (NFERT .GT. 0 .AND. IFERI .NE. 'D') THEN
        DO I = 1, NFERT
          CALL YR_DOY(FDAY(I),YR,IDATE)
          FDAY(I) = (YR + MULTI - 1) * 1000 + IDATE
        END DO
      ENDIF
      IF (NRESAP .GT. 0 .AND. IRESI .NE. 'D') THEN
        DO I = 1, NRESAP
          CALL YR_DOY(RESDAY(I),YR,IDATE)
          RESDAY(I) = (YR + MULTI - 1) * 1000 + IDATE
        END DO
      ENDIF
      IF (NHAR .GT. 0 .AND. IHARI .NE. 'D') THEN
        DO I = 1, NHAR
          CALL YR_DOY(HDATE(I),YR,IDATE)
          HDATE(I) = (YR + MULTI - 1) * 1000 + IDATE
        END DO
      ENDIF
      IF (NTIL .GT. 0 .AND. ISWTIL .NE. 'N') THEN
        DO I = 1, NTIL
          CALL YR_DOY(TDATE(I),YR,IDATE)
          TDATE(I) = (YR + MULTI - 1) * 1000 + IDATE
        END DO
      ENDIF
      IF (NCHEM .GT. 0 .AND. ISWCHE .NE. 'N') THEN
        DO I = 1, NCHEM
          CALL YR_DOY(CDATE(I),YR,IDATE)
          CDATE(I) = (YR + MULTI - 1) * 1000 + IDATE
        END DO
      ENDIF
      CALL YR_DOY(HLATE,YR,IDATE)
      HLATE = (YR +  MULTI - 1) * 1000 + IDATE
      CALL YR_DOY(PWDINF,YR,IDATE)
      PWDINF = (YR +  MULTI - 1) * 1000 + IDATE
      CALL YR_DOY(PWDINL,YR,IDATE)
      PWDINL = (YR +  MULTI - 1) * 1000 + IDATE

      RETURN
      END
