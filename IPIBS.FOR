C=======================================================================
C  IPIBS, Subroutine
C
C  Determines experiment and treatment selection
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       G.H.        4-12-93
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       6-28-93
C  4. Fixed units of DSOIL to be cm                 B.D.B.    19-APR-94
C  5. Change CN2 to CN                              B.D.B.     1-JUL-97
C-----------------------------------------------------------------------
C  INPUT  :
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called :
C
C  Calls  :  MRUN  YR_DOY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

	  SUBROUTINE IPIBS (PGFAC3,NEV,AIRAMT,EFINOC,EFNFIX,
     &           WMDATE,DAYFAC,RADFAC,TXFAC,TMFAC,PRCFAC,CO2FAC,
     &           DPTFAC,WNDFAC,DAYADJ,RADADJ,TXADJ,TMADJ,PRCADJ,
     &           CO2ADJ,DPTADJ,WNDADJ,HSTG,HDLAY,HLATE,HDATE,
     &           PWDINF,PWDINL,SWPLTL,SWPLTH,SWPLTD,PTX,PTTN,
     &           DSOILN,SOILNC,SOILNX,ROTNO,ROTOPT,CRPNO,FLDNAM,
     &           MULTI,LUNIO,CROPD,SDSOIL,STHETA,MODEL,NYRS,YRSIM,
     &           TRTNO,YRPLT,NHAR,RSEED1,YEAR,EXPER,ENAME,TITLET,NREP,
     &           VARNO,PLME,FTYPEN,YRIC,HBPC,PRCROP,SLTX,NCHEM,NTIL,
     &           TDATE,TIMPL,CDATE,CHCOD,CHMET,CHAMT,PLANTSIZE,SDWTPL,
     &           ISENS)

      IMPLICIT NONE

      INCLUDE 'SWITCH.BLK'
      INCLUDE 'GEN1.BLK'
      INCLUDE 'GEN2.BLK'
      INCLUDE 'GEN3.BLK'
      INCLUDE 'NTRC1.BLK'
      INCLUDE 'NTRC2.BLK'

      CHARACTER*1  DAYFAC(10),RADFAC(10),TXFAC(10),TMFAC(10),ISIMI
      CHARACTER*1  PRCFAC(10),CO2FAC(10),DPTFAC(10),WNDFAC(10)
      CHARACTER*1  PLDS,USSWI,NSWI,BLANK
      CHARACTER*2  PRCROP
      CHARACTER*4  WSTA,WSTA1
      CHARACTER*5  FLST,SLTX,IOFF,NCODE,NEND,PLME,DFDRN
      CHARACTER*5  IOFFX,HSIZ(3),FOCOD(25)
      CHARACTER*5  TIMPL(10),CHMET(10),CHCOD(10),CHT(10)
      CHARACTER*6  VARNO,ECONO
      CHARACTER*8  FLDNAM,FNAME,MODEL,EXPER
      CHARACTER*10 CROPS(35),SLNO,CROPD
      CHARACTER*11 SLSOUR
      CHARACTER*16 VRNAME
      CHARACTER*20 HEADER
      CHARACTER*25 TITSIM,TITLET
      CHARACTER*50 SLDESC,TAXON
      CHARACTER*60 ENAME

      INTEGER IEMRG,TRTNO,YRSIM,YRPLT,NYRS,FTYPEN,ERRNUM,YEAR,NREP
      INTEGER ROTNO,ROTOPT,CRPNO,ISENS
      INTEGER LUNIO,RSEED1,PWDINF,PWDINL
      INTEGER HDLAY,HLATE,NEV,I,WMDATE(10),NHAR,HDATE(3),HSTG(3)
      INTEGER IDLAPL(150),MULTI,NIRR,CRID,REP
      INTEGER YRIC,CDATE(10),TDATE(10),NCHEM,NTIL

      REAL    PLANTS,SDWTPL,SDAGE,ATEMP,PLPH,EFINOC,EFNFIX,AZIR
      REAL    FLOB,FLDD,SFDRN,SWPLTL,SWPLTH,SWPLTD,PTX,PTTN
      REAL    IEPT,AIRAMT,DSOILN,SOILNC,SOILNX,SLDP
      REAL    RIP,PGFAC3,WRESND,HBPC(3),PLANTSIZE
      REAL    DAYADJ(10),RADADJ(10),TXADJ(10),TMADJ(10),PRCADJ(10)
      REAL    CO2ADJ(10),DPTADJ(10),WNDADJ(10)
      REAL    DSOILX,THETCX,IEPTX,AIRAMX,EFFIRX
      REAL    APFER(25),AKFER(25),ACFER(25),AOFER(25)
      REAL    AMT(150),CLAY(20),SILT(20),STONES(20),SLNF,SLPF
      REAL    PHKCL(20),EXTP(20),TOTP(20),ORGP(20)
      REAL    SDSOIL(3),STHETA(3),TOTAPW,WTABL(150)
      REAL    TDEP(10),CHDEP(10),CHAMT(10),ICWD,SPRLAP

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------

      DATA CROPS/'DRY BEAN  ','PEANUT    ','SOYBEAN   ','COWPEA    ',
     &           'PEA       ','CHICKPEA  ','PIGEONPEA ','PEPPER    ',
     &           'RICE      ','FALLOW    ','MAIZE     ','WHEAT     ',
     &           'MILLET    ','SORGHUM   ','BARLEY    ','CASSAVA   ',
     &           'POTATO    ','TOMATO    ','C3-CROPS  ','C4-CROPS  ',
     &           'BAHIA     ','GRASS-1   ','GRASS-2   ','GRASS-3   ',
     &           'GRASS-4   ','GRASS-5   ','GRASS-6   ','GRASS-7   ',
     &           'GRASS-8   ','GRASS-9   ','SUGARCANE ','AROIDS    ',
     &           'SUNFLOWER ','PINEAPPLE ','          '/

C-----------------------------------------------------------------------
C     Read FILE names and paths
C-----------------------------------------------------------------------

      READ (LUNIO,1000) ISENS
      READ (LUNIO,1050) MODEL
      READ (LUNIO,1100) FILEX
      READ (LUNIO,1100) FILEA
      READ (LUNIO,1100) FILET
      READ (LUNIO,1100) FILEC,PATHCR
      READ (LUNIO,1100) FILEE,PATHEC
      READ (LUNIO,1100) FILEG,PATHGE
      READ (LUNIO,1100) FILEP,PATHPE
      READ (LUNIO,1100) FILES,PATHSL
      READ (LUNIO,1100) FILEW,PATHWT
      READ (LUNIO,1200) FNAME

      IF (FNAME .EQ. 'OVERVIEW') THEN
         OUTO  = 'OVERVIEW.OUT'
         OUTS  = 'SUMMARY.OUT'
         OUTG  = 'GROWTH.OUT'
         OUTW  = 'WATER.OUT'
         OUTN  = 'NITROGEN.OUT'
         OUTC  = 'CARBON.OUT'
         OUTD  = 'PEST.OUT'
         OUTP  = 'PHOSPHOR.OUT'
         OUTR  = 'OPERAT.OUT'
         OUTH  = 'CHEMICAL.OUT'
         OUTE  = 'ENVIRON.OUT'
       ELSE
         WRITE (OUTO(1:12),1300) FNAME,'.',FILEX(10:11),'O'
         WRITE (OUTS(1:12),1300) FNAME,'.',FILEX(10:11),'S'
         WRITE (OUTG(1:12),1300) FNAME,'.',FILEX(10:11),'G'
         WRITE (OUTW(1:12),1300) FNAME,'.',FILEX(10:11),'W'
         WRITE (OUTN(1:12),1300) FNAME,'.',FILEX(10:11),'N'
         WRITE (OUTC(1:12),1300) FNAME,'.',FILEX(10:11),'C'
         WRITE (OUTD(1:12),1300) FNAME,'.',FILEX(10:11),'D'
         WRITE (OUTP(1:12),1300) FNAME,'.',FILEX(10:11),'P'
         WRITE (OUTR(1:12),1300) FNAME,'.',FILEX(10:11),'R'
         WRITE (OUTH(1:12),1300) FNAME,'.',FILEX(10:11),'H'
         WRITE (OUTE(1:12),1300) FNAME,'.',FILEX(10:11),'E'
      ENDIF

C-----------------------------------------------------------------------
C    Read Simulation Control
C-----------------------------------------------------------------------

      READ (LUNIO,900) NYRS,REP,ISIMI,YRSIM,RSEED1,TITSIM,USSWI
      READ (LUNIO,910) ISWWAT,ISWNIT,ISWSYM,ISWPHO,ISWPOT,ISWDIS,
     &                 ISWCHE,ISWTIL
      READ (LUNIO,910) MEWTH,MESIC,MELI,MEEVP,MEINF,MEPHO,MEHYD,NSWI
      READ (LUNIO,910) IPLTI,IIRRI,IFERI,IRESI,IHARI
      READ (LUNIO,920) IOX,IDETO,IDETS,FROP,IDETG,IDETC,IDETW,IDETN,
     &                 IDETP,IDETD,IDETL,IDETH,IDETR
      CALL YR_DOY (YRSIM,YEAR,ISIM)

C-----------------------------------------------------------------------
C    Read Automatic Management
C-----------------------------------------------------------------------

      READ (LUNIO,930) PWDINF,PWDINL,SWPLTL,SWPLTH,SWPLTD,PTX,PTTN
      READ (LUNIO,940) DSOIL,THETAC,IEPT,IOFF,IAME,AIRAMT,EFFIRR
      READ (LUNIO,940) DSOILN,SOILNC,SOILNX,NCODE,NEND
      READ (LUNIO,950) RIP,NRESDL,DRESMG
      READ (LUNIO,955) HDLAY,HLATE,HPP,HRP
C
C     Convert NCODE to integer
C
      IF (SOILNX .GT. 0) THEN
         READ (NCODE,'(3X,I2)') FTYPEN
      ENDIF

C-----------------------------------------------------------------------
C    Read Experimental Details Section
C-----------------------------------------------------------------------

      READ (LUNIO,50) EXPER,CG,ENAME

C-----------------------------------------------------------------------
C    Read Treatment Section
C-----------------------------------------------------------------------

      READ (LUNIO,55) TRTNO,ROTNO,ROTOPT,CRPNO,TITLET

C-----------------------------------------------------------------------
C    Read Cultivar Section
C-----------------------------------------------------------------------

      READ (LUNIO,56) CROP,VARNO,VRNAME

C-----------------------------------------------------------------------
C    Read Field Section
C-----------------------------------------------------------------------

      READ( LUNIO,59) FLDNAM,WSTA,WSTA1,SLOPE,FLOB,DFDRN,FLDD,SFDRN,
     &                FLST,SLTX,SLDP,SLNO

C-----------------------------------------------------------------------
C    New section to read second tier of field section
C-----------------------------------------------------------------------

      READ (LUNIO,60) XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS

C-----------------------------------------------------------------------
C    Read Initial Conditions Section
C-----------------------------------------------------------------------

      READ (LUNIO,40) HEADER
      READ (LUNIO,61) PRCROP,YRIC,WRESR,WRESND,EFINOC,EFNFIX,
     &                ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID

      DO I = 1, 20
         READ (LUNIO,62,IOSTAT=ERRNUM,ERR=63) DS(I),SW(I),NH4(I),NO3(I)
      END DO

 63   CONTINUE
      IF (ERRNUM .NE. 0) BACKSPACE(LUNIO)

C-----------------------------------------------------------------------
C    Read Planting Details Section
C-----------------------------------------------------------------------

      READ (LUNIO,40) HEADER
      READ (LUNIO,70) YRPLT,IEMRG,PLANTS,PLTPOP,PLME,PLDS,ROWSPC,AZIR,
     &                SDEPTH,SDWTPL,SDAGE,ATEMP,PLPH,SPRLAP,NFORCING,
     &                PLANTSIZE,NDOF,PMTYPE
C-PW  ROWSPC = ROWSPC / 100.0

C-----------------------------------------------------------------------
C    Read Irrigation Section
C-----------------------------------------------------------------------
      READ (LUNIO,75) EFFIRX,DSOILX,THETCX,IEPTX,IOFFX,IAMEX,AIRAMX

      NIRR = 0
      DO I = 1, 150
         READ (LUNIO,76,IOSTAT=ERRNUM,ERR=77) IDLAPL(I),
     &                  IRRCOD(I),AMT(I)
         NIRR = NIRR + 1
      END DO
 77   CONTINUE
      IF (ERRNUM .NE. 0) BACKSPACE(LUNIO)

C-----------------------------------------------------------------------
C    Read Fertilizer Section
C-----------------------------------------------------------------------
      READ (LUNIO,40) HEADER

      NFERT = 0
      DO I = 1, 25
         READ (LUNIO,80,IOSTAT=ERRNUM,ERR=81) FDAY(I),IFTYPE(I),
     &        FERCOD(I),DFERT(I),ANFER(I),APFER(I),AKFER(I),
     &        ACFER(I),AOFER(I),FOCOD(I)
         NFERT = NFERT + 1
      END DO
 81   CONTINUE
      IF (ERRNUM .NE. 0) BACKSPACE(LUNIO)

C-----------------------------------------------------------------------
C    Read Residue Section
C-----------------------------------------------------------------------
      READ (LUNIO,40) HEADER

      NRESAP = 0
      DO I = 1, 10
         READ (LUNIO,86,IOSTAT=ERRNUM,ERR=87) RESDAY(I),RESCOD(I),
     &               RESIDUE(I),RESN(I),RESP(I),RESK(I),RINP(I),
     &               DEPRES(I),RMET(I)
         IF (RESCOD(I)(1:2) .NE. 'RE') RESCOD(I)(1:2) = 'RE'
         NRESAP = NRESAP + 1
      END DO
 87   CONTINUE
      IF (ERRNUM .NE. 0) BACKSPACE(LUNIO)

C-----------------------------------------------------------------------
C     Read Chemical Section
C-----------------------------------------------------------------------

      READ (LUNIO,40) HEADER

      NCHEM = 0
      DO I = 1, 10
         READ (LUNIO,96,IOSTAT=ERRNUM,ERR=97) CDATE(I),CHCOD(I),
     &         CHAMT(I),CHMET(I),CHDEP(I),CHT(I)

         NCHEM = NCHEM + 1
      END DO
 97   CONTINUE
      IF (ERRNUM .NE. 0) BACKSPACE(LUNIO)

C-----------------------------------------------------------------------
C     Read Tillage Section
C-----------------------------------------------------------------------

      READ (LUNIO,40) HEADER

      NTIL  = 0
      DO I = 1, 10
         READ (LUNIO,106,IOSTAT=ERRNUM,ERR=107) TDATE(I),TIMPL(I),
     &                                          TDEP(I)
         NTIL  = NTIL  + 1
      END DO
107   CONTINUE
      IF (ERRNUM .NE. 0) BACKSPACE(LUNIO)

C-----------------------------------------------------------------------
C    Read Environmental Modification Section
C-----------------------------------------------------------------------
      READ (LUNIO,40) HEADER

      NEV = 0
      DO I = 1, 10
         READ (LUNIO,90,IOSTAT=ERRNUM,ERR=92)
     &        WMDATE(I),DAYFAC(I),DAYADJ(I),RADFAC(I),
     &        RADADJ(I),TXFAC(I),TXADJ(I),TMFAC(I),TMADJ(I),
     &        PRCFAC(I),PRCADJ(I),CO2FAC(I),CO2ADJ(I),
     &        DPTFAC(I),DPTADJ(I),WNDFAC(I),WNDADJ(I)
         NEV = NEV + 1
      END DO
 92   CONTINUE
      IF (ERRNUM .NE. 0) BACKSPACE(LUNIO)

C-----------------------------------------------------------------------
C    Read Harvest Section
C-----------------------------------------------------------------------
      READ (LUNIO,40)

      NHAR = 0
      DO I = 1, 3
        !
        ! Add new variable HBPC, percentage of byproduct harvest
        !
         READ (LUNIO,100,IOSTAT=ERRNUM,ERR=102)HDATE(I),HSTG(I),HCOM(I),
     &                                         HSIZ(I),HPC(I),HBPC(I)
         NHAR = NHAR + 1
      END DO
 102  CONTINUE
      IF (ERRNUM .NE. 0) BACKSPACE(LUNIO)

C-----------------------------------------------------------------------
C     Read soil description properties
C-----------------------------------------------------------------------
      READ (LUNIO,40)  HEADER
      READ (LUNIO,960) SLNO,SLSOUR,SLTX,SLDP,SLDESC
      READ (LUNIO,970) TAXON
      READ (LUNIO,980) SALB,U,SWCON,CN,SLNF,SLPF

C-----------------------------------------------------------------------
C     Read soil layer properties
C-----------------------------------------------------------------------
      NLAYR = 0
      DO I = 1, 20
         !
         ! Changed WR() to SHF() ... PW
         !
         READ (LUNIO,990) DS(I),LL(I),DUL(I),SAT(I),SHF(I),SWCN(I),
     &         BD(I),OC(I),CLAY(I),SILT(I),STONES(I),TOTN(I),PH(I),
     &         PHKCL(I),CEC(I),ADCOEF(I)
         IF (I .GT. 1) THEN
           IF (DS(I) .LT. DS(I-1)) EXIT
         ENDIF
         NLAYR = NLAYR + 1
      END DO

C-----------------------------------------------------------------------
C     Handle problem with NLAYR is set to 20
C-----------------------------------------------------------------------

      IF (NLAYR .EQ. 20) THEN
         READ (LUNIO,994) BLANK
 994     FORMAT (A1)
      ENDIF

C-----------------------------------------------------------------------
C     Read phosphorus layer data
C-----------------------------------------------------------------------

      DO I = 1, NLAYR
         READ (LUNIO,995,IOSTAT=ERRNUM,ERR=996)
     &         DS(I),EXTP(I),TOTP(I),ORGP(I),CACO(I),TOTBAS(I)
      END DO
 996  CONTINUE
      IF (ERRNUM .NE. 0) BACKSPACE(LUNIO)

C-----------------------------------------------------------------------
C     Read crop genetic information
C-----------------------------------------------------------------------

      IF (CROP .NE. 'FA') THEN
         READ (LUNIO,40)
         IF (CROP .EQ. 'PI') THEN
            READ (LUNIO,1800) VARNO,VRNAME,ECONO,P2,P3,P4,G2,G3,PHINT
         ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     Multi-run routines
C-----------------------------------------------------------------------

      IF (MULTI .GT. 1) THEN
         CALL MRUN (FILEW,YRSIM,YRPLT,MULTI,YEAR,
     &        RESDAY,NRESAP,FDAY,NFERT,NIRR,MEWTH,NREP,
     &        HLATE,PWDINF,PWDINL,NHAR,HDATE,IDLAPL,PATHWT,
     &        IIRRI,IFERI,IHARI,IRESI,YRIC,TDATE,CDATE,ISWTIL,
     &        ISWCHE,NTIL,NCHEM)
      ENDIF

C-----------------------------------------------------------------------
C     Set Irrigation Management
C-----------------------------------------------------------------------

      IF (IIRRI .EQ. 'R' .OR. IIRRI .EQ. 'D') THEN
         IF (EFFIRX .GT. 0.0) THEN
            EFFIRR = EFFIRX
         ENDIF
         IF (THETCX .GT. 0.0) THEN
            THETAC = THETCX
         ENDIF
         IF (DSOILX .GT. 0.0) THEN
            DSOIL  = DSOILX
         ENDIF
         IF (AIRAMX .GT. 0.0) THEN
            AIRAMT = AIRAMX
         ENDIF
      ENDIF

      DO I = 1, 3
         SDSOIL(I) = DSOIL                       ! DSOIL has units of cm
         STHETA(I) = THETAC                      ! THETA has units of %
      END DO

C-----------------------------------------------------------------------
C     Irrigation details
C-----------------------------------------------------------------------

      NTBL   = 0
      NAPW   = 0
      TOTAPW = 0.0

      DO I = 1, 150
         JULAPL(I) = 0
         JULWTB(I) = 0
         AMIR(I)   = 0.0
         WTABL(I)  = 0.0
      END DO

      DO I = 1, NIRR
         IF (IRRCOD(I) .EQ. 7 ) THEN
             NTBL         = NTBL + 1
             JULWTB(NTBL) = IDLAPL(I)
             WTABL(NTBL)  = AMT(I)
          ELSE IF (IRRCOD(I) .LE. 5) THEN
C
C            If not WATER TABLE or BUND MANAGEMENT, then it is an irrigation
C
             NAPW         = NAPW + 1
             JULAPL(NAPW) = IDLAPL(I)
             AMIR(NAPW)   = AMT(I)
             TOTAPW       = TOTAPW + AMIR(NAPW)
         ENDIF
      END DO

C-----------------------------------------------------------------------
C    Soil Parameter Initialization
C-----------------------------------------------------------------------

      DMOD     = SLNF
      PGFAC3   = SLPF
      DLAYR(1) = DS(1)

      IF (NLAYR .GT. 1) THEN
         DO I = 2, NLAYR
            DLAYR(I) = DS(I) - DS(I-1)
         END DO
      ENDIF

      DEPMAX = DS(NLAYR)
      WTDEP  = ICWD

C-----------------------------------------------------------------------
C    Harvest Initializaiton Section
C-----------------------------------------------------------------------

      IF (NHAR .EQ. 0) THEN
         HPC(1)  = HPP
         HBPC(1) = HRP
      ENDIF

C-----------------------------------------------------------------------
C    Set Species Identification
C-----------------------------------------------------------------------

      IF (CROP .EQ. 'BN') CRID = 1
      IF (CROP .EQ. 'PN') CRID = 2
      IF (CROP .EQ. 'SB') CRID = 3
      IF (CROP .EQ. 'CP') CRID = 4
      IF (CROP .EQ. 'PE') CRID = 5
      IF (CROP .EQ. 'CH') CRID = 6
      IF (CROP .EQ. 'PP') CRID = 7
      IF (CROP .EQ. 'PR') CRID = 8
      IF (CROP .EQ. 'RI') CRID = 9
      IF (CROP .EQ. 'FA') CRID = 10
      IF (CROP .EQ. 'MZ') CRID = 11
      IF (CROP .EQ. 'WH') CRID = 12
      IF (CROP .EQ. 'ML') CRID = 13
      IF (CROP .EQ. 'SG') CRID = 14
      IF (CROP .EQ. 'BA') CRID = 15
      IF (CROP .EQ. 'CS') CRID = 16
      IF (CROP .EQ. 'PT') CRID = 17
      IF (CROP .EQ. 'TM') CRID = 18
      IF (CROP .EQ. 'C3') CRID = 19
      IF (CROP .EQ. 'C4') CRID = 20
      IF (CROP .EQ. 'G0') CRID = 21
      IF (CROP .EQ. 'G1') CRID = 22
      IF (CROP .EQ. 'G2') CRID = 23
      IF (CROP .EQ. 'G3') CRID = 24
      IF (CROP .EQ. 'G4') CRID = 25
      IF (CROP .EQ. 'G5') CRID = 26
      IF (CROP .EQ. 'G6') CRID = 27
      IF (CROP .EQ. 'G7') CRID = 28
      IF (CROP .EQ. 'G8') CRID = 29
      IF (CROP .EQ. 'G9') CRID = 30
      IF (CROP .EQ. 'SC') CRID = 31
      IF (CROP .EQ. 'SU') CRID = 33
      IF (CROP .EQ. 'PI') CRID = 34

      CROPD = CROPS(CRID)

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

   40 FORMAT (A20)
   50 FORMAT (/,3X,A8,1X,A2,1X,A60)
   55 FORMAT (/,I3,I1,2(1X,I1),1X,A25)
   56 FORMAT (/,3X,A2,1X,A6,1X,A16)
   59 FORMAT (/,3X,A8,1X,2A4,1X,F5.0,1X,F5.0,1X,A5,2(1X,F5.0),
     &        2(1X,A5),1X,F5.0,1X,A10)
   60 FORMAT (3X,2(F15.5,1X),F9.2,1X,F17.1,1X,F5.0,2(1X,F5.1))
   61 FORMAT (3X,A2,4X,I5,2(1X,F5.0),2(1X,F5.2),1X,F5.1,1X,F5.0,
     &        2(1X,F5.2),2(1X,F5.0))
   62 FORMAT (3X,F5.0,1X,F5.3,2(1X,F5.1))
   70 FORMAT (3X,I5,3X,I3,2(1X,F5.1),2(5X,A1),2(1X,F5.0),1X,F5.1,
     &        5(1X,F5.0),I6,F6.1,2(I6))
   75 FORMAT (/,3X,F5.3,3(1X,F5.0),2(1X,A5),1X,F5.1)
   76 FORMAT (3X,I5,5X,I1,1X,F5.0)
   80 FORMAT (3X,I5,4X,I2,1X,A5,6(1X,F5.0),1X,A5)
   86 FORMAT (3X,I5,1X,A5,1X,F5.0,3(1X,F5.2),2(1X,F5.0),1X,A5)
   90 FORMAT (3X,I5,5(1X,A1,F4.1),1X,A1,F4.0,2(1X,A1,F4.1))
   96 FORMAT (3X,I5,1X,A5,1X,F5.0,1X,A5,1X,F5.0,1X,A5)
  100 FORMAT (3X,I5,4X,I2,2(1X,A5),2(1X,F5.0))
  106 FORMAT (3X,I5,1X,A5,1X,F5.0)
  900 FORMAT (/,18X,I2,2X,I4,5X,A1,2(1X,I5),1X,A25,5X,A1)
  910 FORMAT (14X,8(5X,A1))
  920 FORMAT (14X,3(5X,A1),4X,I2,9(5X,A1))
  930 FORMAT (/,14X,2(1X,I5),5(1X,F5.0))
  940 FORMAT (14X,3(1X,F5.0),2(1X,A5),1X,F5.1,1X,F5.3)
  950 FORMAT (15X,F5.0,1X,I5,1X,F5.0)
  955 FORMAT (14X,2(1X,I5),5(1X,F5.0))
  960 FORMAT (1X,A10, 2X, A11,1X,A5,1X,F5.0,1X,A50)
  970 FORMAT (43X,A50)
  980 FORMAT (7X,F5.2,1X,F5.1,1X,F5.2,1X,F5.0,2(1X,F5.2))
  990 FORMAT (1X,F5.0,6X,4(1X,F5.3),1X,F5.1,2(1X,F5.2),3(1X,F5.1),
     &        1X,F5.2,19(1X, F5.1))
  995 FORMAT (1X,F5.0,18(1X,F5.0))
 1000 FORMAT (55X,I5)
 1050 FORMAT (/,15X,A8)
 1100 FORMAT (15X,A12,1X,A80)
 1200 FORMAT (15X,A8)
 1300 FORMAT (A8,A1,A2,A1)
c1800 FORMAT (A6,1X,A16,1X,A6,1X,F5.1,F7.2,F7.1,F7.2,F7.3,F6.1)
 1800 FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.1,F6.0,F6.1,F6.2,F6.1)
      END
