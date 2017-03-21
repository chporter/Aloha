C=======================================================================
C  OPHARV, Subroutine
C
C  Write the harvest report
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2. Header revision and minor changes           P.W.W.      2-7-93
C  3. Added switch block, etc.                    P.W.W.      2-7-93
C-----------------------------------------------------------------------
C  INPUT  : TRTNO,YRDOY,YRSIM,YRPLT,CROP,CROPD,
C           WTNSD,NAP,TOTIR,CRAIN,CET,TRUNOF,PESW,TDRAIN,TSON,
C           TSOC,TLCH,NAPNIT,ISDATE,MDATE,
C           YIELD,SKERWT,GPSM,GPP,MAXLAI,PBIOMS,STOVER,XGNP,TOTNUP,APTNUP,
C           GNUP,BIOMAS,CGPE,CLAI,NYRS,FLDNAM,NSENS,EXPER,WTNFX,WTNCAN,
C           TSIN,WTNUP,NREP,AMTNIT,SDWTAM,TITLET,NIRR,STGDOY,CANNAA,
C           CANWAA,XN,ENAME,ROTNO,ROTOPT,CRPNO,BEXIST
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : PINE
C
C  Calls  : OPTAB OPSTRS OPSUM OPBAT
C-----------------------------------------------------------------------

C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPHARV (TRTNO,YRDOY,YRSIM,YRPLT,CROP,CROPD,
     &   WTNSD,NAP,TOTIR,CRAIN,CET,TRUNOF,PESW,TDRAIN,TSON,TSOC,TLCH,
     &   NAPNIT,ISDATE,MDATE,YIELD,SKERWT,GPSM,GPP,MAXLAI,
     &   PBIOMS,STOVER,XGNP,TOTNUP,APTNUP,GNUP,BIOMAS,
     &   NYRS,FLDNAM,EXPER,WTNFX,WTNCAN,TSIN,WTNUP,NREP,AMTNIT,
     &   SDWTAM,TITLET,STGDOY,ENAME,SDWT,ROTNO,ROTOPT,CRPNO,
     &   SDRATE,TOPWT,AMTRES,HBPC,FBIOM,EYEWT,PMDATE,FHDATE,
     &   WTINITIAL,BWAH,SDWTAH,TSEDM,TRON,TOTPST,H2OLOS,SEPLOS,CHMCOD,
     &   ISENS)

      IMPLICIT     NONE

      INCLUDE     'SWITCH.BLK'

      CHARACTER*1  UPCASE
      CHARACTER*2  CROP
      CHARACTER*5  CHMCOD(10)
      CHARACTER*8  EXPER,FLDNAM
      CHARACTER*10 CROPD
      CHARACTER*25 TITLET
      CHARACTER*60 ENAME

      INTEGER TRTNO,ROTNO,ROTOPT,CRPNO,ISENS
      INTEGER NAPNIT,NAP,ISDATE,PMDATE,FHDATE
      INTEGER YRDOY,YRPLT,YRSIM,YRNR1,YRNR2,YRNR5,YRNR7,DNR1,DNR7
      INTEGER YRNR3,NYRS,MDATE,NREP,STGDOY(20)

      REAL    MAXLAI,ACREFC,TOTNUP,PSDWT,PSPP,HI
      REAL    YIELD,YIELDB,TLCH
      REAL    AMTNIT,WTNCAN,TSON,TSOC,CRESAP
      REAL    TOTIR,CRAIN,CET,TRUNOF,PESW,TDRAIN,WTNSD,WTNFX,WTNUP
      REAL    PBIOMS,SDWTAM,SDWTAH,APTNUP
      REAL    SKERWT,STOVER,TSIN,SEEDNO,SDWT
      REAL    TOPWT,GPP,GPSM,GNUP,BIOMAS,XGNP,FBIOM,EYEWT,PEYEWT
      REAL    SDRATE,AMTRES,HBPC(3),BWAH,WTINITIAL
      REAL    TOTPST(*),H2OLOS(*),SEPLOS(*),TSEDM,TRON

      PARAMETER (ACREFC = 2.47)

C-----------------------------------------------------------------------
C     Calculate variables for output.
C     Update nitrogen and residue applications after routines have been
C     modified to handle automatic management.
C-----------------------------------------------------------------------

      CRESAP = AMTRES
      YRNR1  = ISDATE
      YRNR2  = STGDOY(2)
      YRNR3  = STGDOY(3)
      YRNR5  = STGDOY(5)
      YRNR7  = MDATE
      WTNUP  = TOTNUP/10.0
      WTNCAN = TOTNUP/10.0
      WTNSD  = GNUP  /10.0

      IF (INDEX ('PI',CROP) .EQ. 0) THEN
         TOTNUP = WTNUP
      ENDIF
      PSDWT  = 0.0
      SDRATE = WTINITIAL
      IF (SEEDNO .GT. 0.0 .AND. SDWT  .GE. 0.0) THEN
         PSDWT = SDWT/SEEDNO
      ENDIF
      IF (BIOMAS .GT. 0.0 .AND. YIELD .GE. 0.0) THEN
         HI = YIELD/(BIOMAS*10.0)
       ELSE
         HI = 0.0
      ENDIF

C-----------------------------------------------------------------------
C     Adjust dates since Pineapple grows after harvest
C-----------------------------------------------------------------------

      MDATE = FHDATE

C-----------------------------------------------------------------------
C     Actual yield harvested (default is 100 %)
C-----------------------------------------------------------------------

      SDWT   = YIELD / 10.0
      SDWTAM = YIELD / 10.0
      SDWTAH = SDWT * HPC(1)/100.0
      TOPWT  = BIOMAS

C-----------------------------------------------------------------------
C     Actual byproduct harvested (default is 0 %)
C     Byproduct not harvested is incorporated
C-----------------------------------------------------------------------

      BWAH   = STOVER * HBPC(1)/100.0

C-----------------------------------------------------------------------
C     Call OPTAB for comparing measured and simulated data
C     for yield, yield components and major development stages
C-----------------------------------------------------------------------

      CALL OPTAB (TRTNO,NOUTDO,FILEA,IDETO,RNMODE,IDETS,
     &    CROP,ISDATE,MDATE,YIELD,GPSM,GPP,MAXLAI,PBIOMS,
     &    STOVER,XGNP,TOTNUP,APTNUP,GNUP,BIOMAS,NYRS,
     &    YRSIM,YRPLT,IPLTI,NREP,TITLET,FBIOM,EYEWT,PMDATE,
     &    DNR1,DNR7,ISENS)

C-----------------------------------------------------------------------
C     Resource input and stress summary
C-----------------------------------------------------------------------

      CALL OPSTRS (IDETO,RNMODE,NOUTDO,CROP,NYRS)

C-----------------------------------------------------------------------
C     Crop yield summary report
C-----------------------------------------------------------------------

      IF (CROP .EQ.'PI') THEN
         YIELDB = YIELD/0.8914         ! Fresh fruit yield (lb/acre)
      ELSE
         YIELDB = SDWT*10.0/ACREFC * 2.2046
      ENDIF

      PEYEWT = EYEWT*1000.          ! Eye weight (mg/eye)

      IF (RNMODE .EQ. 'I' .AND. NYRS .LE. 1) THEN
         WRITE (     *,1200) CROPD,NINT(YIELD),NINT(YIELDB),NINT(GPSM),
     &                       NINT(PEYEWT)
      ENDIF

      IF (UPCASE(IDETO) .EQ. 'Y') THEN
         WRITE (NOUTDO,1200) CROPD,NINT(YIELD),NINT(YIELDB),NINT(GPSM),
     &                       NINT(PEYEWT)
      ENDIF

C-----------------------------------------------------------------------
C     Currently not computing N stress in veg and rep stages
C-----------------------------------------------------------------------

C-------------------------------------------------------------------
C     Call Simulation Summary File
C-------------------------------------------------------------------

      IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
         CALL OPSUM (NREP,EXPER,CROP,TRTNO,ROTNO,ROTOPT,CRPNO,TITLET,
     &        YRDOY,YRPLT,YRSIM,YRNR1,YRNR7,TOPWT,SEEDNO,PSPP,NAP,
     &        TOTIR,CRAIN,CET,TRUNOF,PESW,TDRAIN,NAPNIT,AMTNIT,WTNUP,
     &        WTNFX,TLCH,TSIN,CRESAP,TSON,TSOC,WTNSD,WTNCAN,PSDWT,
     &        IDETS,NOUTDS,FLDNAM,OUTS,CG,SDWTAM,
     &        GPSM,SKERWT,GPP,ENAME,SDRATE,BWAH,SDWTAH)
c        CALL OPENV (NREP,EXPER,CROP,TRTNO,ROTNO,ROTOPT,CRPNO,TITLET,
c    &        TLCH,IDETS,NOUTDE,FLDNAM,ENAME,OUTE,CG,AMTNIT,TRON,TSEDM,
c    &        TOTPST,H2OLOS,SEPLOS,CHMCOD)
      ELSE
        IF (NREP .EQ. 1) THEN
           OPEN(UNIT = NOUTDS,  FILE = OUTS , STATUS = 'UNKNOWN')
c          OPEN(UNIT = NOUTDE,  FILE = OUTE , STATUS = 'UNKNOWN')
        ENDIF
      ENDIF

C-------------------------------------------------------------------
C
C-------------------------------------------------------------------

      IF (INDEX('NQSA',RNMODE) .GT. 0 .OR. NYRS .GT. 1) THEN
           CALL OPBAT (CROP,NREP,RNMODE,
     &     TRTNO,YRNR1,YRNR7,TOPWT,SDWT,CRAIN,TOTIR,
     &     CET,PESW,WTNUP,TLCH,TSOC,TSON,TSIN,NYRS,DNR1,DNR7)
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------
 1200 FORMAT (/,2X,A10,'Yield (kg/ha)=',I6,' (lb/a)=',I7,1X,
     1       'Eye mý=',I4,' Eye wt.(mg)=',I4)

      END

C=======================================================================
C  OPSUM, Subroutine
C
C  Determines summary output
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      2-18-93
C-----------------------------------------------------------------------
C  INPUT  : NREP,EXPER,CROP,TRTNO,ROTNO,ROTOPT,CRPNO,TITLET,YRDOY,
C           YRPLT,YRSIM,YRNR1,YRNR7,TOPWT,SEEDNO,PSPP,NAP,TOTIR,
C           CRAIN,CET,TRUNOF,PESW,TDRAIN,NAPNIT,AMTNIT,WTNUP,WTNFX,TLCH,
C           TSIN,CRESAP,TSON,TSOC,WTNSD,WTNCAN,PSDWT,IDETS,NOUTDS,
C           FLDNAM,OUTS,CG,SDWTAM,BIOMAS,YIELD,GPSM,SKERWT,GPP,NIRR,ENAME,
C           SEXIST
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPHARV
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

       SUBROUTINE OPSUM (NREP,EXPER,CROP,TRTNO,ROTNO,
     & ROTOPT,CRPNO,TITLET,YRDOY,YRPLT,YRSIM,YRNR1,YRNR7,TOPWT,
     & SEEDNO,PSPP,NAP,TOTIR,CRAIN,CET,TRUNOF,PESW,TDRAIN,
     & NAPNIT,AMTNIT,WTNUP,WTNFX,TLCH,TSIN,CRESAP,TSON,TSOC,
     & WTNSD,WTNCAN,PSDWT,IDETS,NOUTDS,FLDNAM,OUTS,CG,SDWTAM,
     & GPSM,SKERWT,GPP,ENAME,SDRATE,BWAH,SDWTAH)

      IMPLICIT     NONE

      CHARACTER*1  IDETS
      CHARACTER*2  CROP,CG
      CHARACTER*8  EXPER,FLDNAM
      CHARACTER*12 OUTS
      CHARACTER*25 TITLET
      CHARACTER*60 ENAME

      INTEGER NREP,ROTNO,ROTOPT,CRPNO,NAP,NAPNIT
      INTEGER TRTNO,NOUTDS,NNAPHO,YRDOY,YRPLT,YRSIM,YRNR1,YRNR7

      REAL    SDRATE,TOPWT,SEEDNO,PSPP,TOTIR,CRAIN,CET
      REAL    TRUNOF,PESW,TDRAIN,AMTNIT,WTNUP,WTNFX,TLCH,TSIN,CRESAP
      REAL    TSON,TSOC,WTNSD,TPLEFT,PSDWT,WTNCAN
      REAL    TOTPH,TOTPUP,SDWTAM
      REAL    GPSM,SKERWT,GPP,BWAH,SDWTAH

C-------------------------------------------------------------------
C     Pass following variables once the PHOSPHORUS balance is
C     implemented
C-------------------------------------------------------------------

      NNAPHO  = 0
      TOTPH   = 0.0
      TOTPUP  = 0.0
      TPLEFT  = 0.0

C-------------------------------------------------------------------
C     Simulation Summary File
C-------------------------------------------------------------------

      IF (NREP .EQ. 1 .AND. IDETS .NE. 'A') THEN
         OPEN (UNIT = NOUTDS,FILE = OUTS, STATUS = 'UNKNOWN')
         WRITE (NOUTDS,100) EXPER,CG,ENAME
         WRITE (NOUTDS,200)
       ELSE
         OPEN (UNIT = NOUTDS,FILE = OUTS, ACCESS = 'APPEND',
     &   STATUS = 'UNKNOWN')
      ENDIF

      PSDWT  = SKERWT
C     SDRATE = (PSDWT * PLTPOP) / 0.8 * 10.0
      SEEDNO = GPSM
      PSPP   = GPP

      WRITE (NOUTDS,300) MOD(NREP,1000),MOD(TRTNO,1000),
     &  ROTNO,ROTOPT,CRPNO,CROP,
     &  TITLET(1:19),FLDNAM,MOD(YRSIM,100000),MOD(YRPLT,100000),
     &  MOD(YRNR1,100000),MOD(YRNR7,100000),MOD(YRDOY,100000),
     &  NINT(SDRATE),NINT(TOPWT*10),NINT(SDWTAM*10),NINT(SDWTAH*10),
     &  NINT(BWAH*10),NINT(PSDWT*1000),NINT(SEEDNO),
     &  NINT(PSPP),NAP,NINT(TOTIR),NINT(CRAIN),NINT(CET),NINT(TRUNOF),
     &  NINT(TDRAIN),NINT(PESW*10),NAPNIT,NINT(AMTNIT),
     &  NINT(WTNFX*10),NINT(WTNUP*10),NINT(TLCH),NINT(TSIN),
     &  NINT(WTNCAN*10),NINT(WTNSD*10),NINT(CRESAP),NINT(TSON),
     &  NINT(TSOC/1000),NNAPHO,NINT(TOTPH),NINT(TOTPUP),NINT(TPLEFT)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 100  FORMAT (
     & '*SUMMARY : ',A8,A2,1X,A60,//,
     & '!IDENTIFIERS.............................. ',
     &  'DATES......................... ',
     &  'DRY WEIGHTS.................................... ',
     &  'WATER.................................... ',
     &  'NITROGEN....................................... ',
     &  'ORGANIC MATTER... ',
     &  'PHOSPHORUS............')
 200  FORMAT ('@RP TN ROC CR TNAM',16X,'FNAM      SDAT',
     & '  PDAT  ADAT  MDAT  HDAT  DWAP  CWAM  HWAM  HWAH  BWAH',
     & '  HWUM  H#AM  H#UM  IR#M  IRCM  PRCM  ETCM  ROCM  DRCM',
     & '  SWXM  NI#M  NICM  NFXM  NUCM  NLCM  NIAM  CNAM  GNAM',
     & '  RECM  ONAM  OCAM  PO#M  POCM  CPAM  SPAM')
 300  FORMAT (2I3,1X,3(I1),1X,A2,1X,A19,1X,A8,5(1X,I5.5),30(1X,I5))

      END

C=======================================================================
C  OPENV, Subroutine
C
C  Determines environment summary output
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      2-18-93
C-----------------------------------------------------------------------
C  INPUT  : NREP,EXPER,CROP,TRTNO,ROTNO,ROTOPT,CRPNO,TITLET,YRDOY,
C           YRPLT,YRSIM,YRNR1,YRNR7,TOPWT,SEEDNO,PSPP,NAP,TOTIR,
C           CRAIN,CET,TRUNOF,PESW,TDRAIN,NAPNIT,AMTNIT,WTNUP,WTNFX,TLCH,
C           TSIN,CRESAP,TSON,TSOC,WTNSD,WTNCAN,PSDWT,PLTPOP,IDETS,NOUTDS,
C           FLDNAM,OUTS,CG,SDWTAM,BIOMAS,YIELD,GPSM,SKERWT,GPP,NIRR,ENAME,
C           SEXIST
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPHARV
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

       SUBROUTINE OPENV (NREP,EXPER,CROP,TRTNO,ROTNO,
     &     ROTOPT,CRPNO,TITLET,TLCH,IDETS,NOUTDE,FLDNAM,ENAME,OUTE,CG,
     &     AMTNIT,TRON,TSEDM,TOTPST,H2OLOS,SEPLOS,CHMCOD)

      IMPLICIT  NONE

      INCLUDE  'COWTH.BLK'

C     COMMON/COWTH/ TMAXR,TMINR,RAINR,DAYLR,RADR,CETR,TURFR,
C    &              RADTR,SWFCR,NSTRR,AGEFR,NNR

      INTEGER I,NREP,ROTNO,ROTOPT,CRPNO,TRTNO,NOUTDE
C     INTEGER NNR(5)

      REAL TLCH,TRON,PLCH,PROM,TSEDM,TOTPH,AMTNIT
C     REAL TMAXR(5),TMINR(5),RAINR(5),DAYLR(5),RADR(5),CETR(5)
C     REAL TURFR(5),RADTR(5),SWFCR(5),NSTRR(5),AGEFR(5)
      REAL TSS,TGF,TPGF,SRPGF,WSPGF,NSPGF
      REAL H2OLOS(*),SEPLOS(*),TOTPST(*)

      CHARACTER*1  IDETS
      CHARACTER*2  CROP,CG
      CHARACTER*8  EXPER,FLDNAM
      CHARACTER*12 OUTE
      CHARACTER*25 TITLET
      CHARACTER*60 ENAME
      CHARACTER*5  CHMCOD(10)

C-------------------------------------------------------------------
C     Pass following variables once the PHOSPHORUS balance is
C     implemented
C-------------------------------------------------------------------

      PLCH  = 0.0  ! leached
      PROM  = 0.0  ! runoff/sediment
      TOTPH = 0.0  ! total applied

C-------------------------------------------------------------------
C!     Calculate temperature variables for output
C!     TGF - grain filling, TPGF - pre-grain filling, TSS - season
C!
C-------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
         IF ((NNR(1)+NNR(2)) .GT. 0) THEN
           TGF   = (TMAXR(3)+TMINR(3))/2.0
           TSS   = (TMAXR(4)+TMINR(4))/2.0
           TPGF  = (0.5*(TMAXR(1)+TMINR(1))*NNR(1) +
     &             0.5*(TMAXR(2)+TMINR(2))*NNR(2))/
     &             (NNR(1)+NNR(2))
           SRPGF = (RADR(1)*NNR(1)+RADR(2)*NNR(2))/(NNR(1)+NNR(2))
           WSPGF = (TURFR(1)*NNR(1)+TURFR(2)*NNR(2))/(NNR(1)+NNR(2))
           NSPGF = (NSTRR(1)*NNR(1)+NSTRR(2)*NNR(2))/(NNR(1)+NNR(2))
         ELSE
           TGF   = 0.0
           TSS   = 0.0
           TPGF  = 0.0
           SRPGF = 0.0
           WSPGF = 0.0
           NSPGF = 0.0
         ENDIF

      ENDIF

C-------------------------------------------------------------------
C     Environment Summary File
C-------------------------------------------------------------------

      IF (NREP .EQ. 1 .AND. IDETS .NE. 'A') THEN
         OPEN (UNIT = NOUTDE,FILE = OUTE, STATUS = 'UNKNOWN')
         WRITE (NOUTDE,100) EXPER,CG,ENAME,(CHMCOD(I),I=1,4)
         WRITE (NOUTDE,200)
       ELSE
         OPEN(UNIT = NOUTDE,FILE = OUTE, ACCESS = 'APPEND',
     &   STATUS = 'UNKNOWN')
      ENDIF

      WRITE (NOUTDE,300) MOD(NREP,1000),MOD(TRTNO,1000),
     &  ROTNO,ROTOPT,CRPNO,CROP,
     &  TITLET(1:19),FLDNAM,NINT(TSEDM),
     &  NINT(RAINR(4)),NINT(RAINR(1)+RAINR(2)),NINT(RAINR(3)),
     &  TSS,TPGF,TGF,RADR(4),SRPGF,RADR(3),
     &  TURFR(4),WSPGF,TURFR(3),NSTRR(4),NSPGF,NSTRR(3),
     &  NINT(AMTNIT),NINT(TLCH),NINT(TRON),
     &  NINT(TOTPH),NINT(PLCH),NINT(PROM),
     &  (TOTPST(I),NINT(1000*H2OLOS(I)),NINT(1000*SEPLOS(I)),I=1,4)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 100  FORMAT (
     & '*ENVIRONMENTAL SUMMARY : ',A8,A2,1X,A60,//,
     & '!IDENTIFIERS............................... ',
     &  'SOIL  ',
     &  'WEATHER.............................................. ',
     &  'STRESS............................. ',
     &  'NUTRIENTS.......................... ',
     &  'CHEMICAL.............................................',
     &  '................',/,
     & '!                                           LOSS ',
     &  ' RAINFALL         ',' TEMPERATURE      ',' RADIATION        ',
     &  ' WATER            ',' NITROGEN         ',
     &  ' NITROGEN         ',' PHOSPORUS        ',
     &  4(1X,A5,12X))
 200  FORMAT ('@RP TN ROC CR TNAM',16X,'FNAM      SEDM',
     & '  PRSS  PPGF  PRGF  TPSS  TPPG  TPGF  SRSS  SPGF  SRGF',
     & '  WSSS  WPGF  WSGF  NSSS  NPGF  NSGF',
     & '  NICM  NLCM  NROM  POCM  PLCM  PROM',
     & '  CHM1  CRO1  LCH1  CHM2  CRO2  LCH2  CHM3  CRO3  LCH3',
     & '  CHM4  CRO4  LCH4')
 300  FORMAT (2I3,1X,3(I1),1X,A2,1X,A19,1X,A8,
     &        4(1X,I5),6(1X,F5.1),6(1X,F5.3),6(1X,I5),
     &        4(1X,F5.1,2(1X,I5)))

      END

C=======================================================================
C  OPBAT, Subroutine
C
C  Writes batch output data
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      2-28-93
C-----------------------------------------------------------------------
C  INPUT  : CROP,SEXIST,NREP,TRTNO,YRPLT,YRNR1,YRNR7,TOPWT,
C           SDWT,CRAIN,TOTIR,CET,PESW,WTNUP,TLCH,TSOC,TSON,TSIN
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPHARV
C
C  Calls  : CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPBAT (CROP,NREP,RNMODE,
     &     TRTNO,YRNR1,YRNR7,TOPWT,SDWT,CRAIN,TOTIR,
     &     CET,PESW,WTNUP,TLCH,TSOC,TSON,TSIN,NYRS,DNR1,DNR7)

      IMPLICIT    NONE

      CHARACTER*1 RNMODE*1,CROP*2
      INTEGER     NLINES,NREP,TRTNO,YRNR1,YRNR7,NYRS,DNR1,DNR7
      REAL        TLCH,TOTIR,CET,CRAIN,TOPWT,SDWT,PESW,WTNUP
      REAL        TSOC,TSIN,TSON

      IF (RNMODE .EQ. 'A' .AND. NYRS .EQ. 1) THEN
         NLINES = TRTNO - 1
      ELSE
         NLINES = NREP - 1
      ENDIF
      NLINES = MOD(NLINES,20)

      IF (NLINES .EQ. 0) THEN
         CALL CLEAR
         WRITE (*,1425)
      END IF

      IF (CROP .NE. 'FA') THEN
         WRITE (*,1450) MOD(NREP,1000),CROP,MOD(TRTNO,1000),
     &     DNR1,DNR7,
     &     NINT(TOPWT*10),NINT(SDWT*10),NINT(CRAIN),NINT(TOTIR),
     &     NINT(CET),NINT(PESW*10),NINT(WTNUP*10),NINT(TLCH),
     &     NINT(TSIN),NINT(TSON),NINT(TSOC/1000)
       ELSE
         WRITE (*,1450) MOD(NREP,1000),CROP,MOD(TRTNO,1000),
     &     YRNR1,YRNR7,
     &     NINT(TOPWT*10),NINT(SDWT*10),NINT(CRAIN),NINT(TOTIR),
     &     NINT(CET),NINT(PESW*10),NINT(WTNUP*10),NINT(TLCH),
     &     NINT(TSIN),NINT(TSON),NINT(TSOC/1000)

      ENDIF

      NLINES = NLINES + 1

      RETURN
C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 1425 FORMAT(1X,'RUN   TRT FLO MAT TOPWT SEEDW TRAIN  TIRR'
     & 3X,'CET  PESW TNUP TNLC TNLF  TSON TSOC',/,
     & 11X,'dap dap kg/ha kg/ha',4(4X,'mm'),1X,4('-'),'kg/ha',
     & 5('-'),1X,'kg/ha',1X,'t/ha')
 1450 FORMAT(1X,I3,1X,A2,I3,2(1X,I3),6(1X,I5),3(1X,I4),I6,I5)

      END

C=======================================================================
C  OPTAB, Subroutine
C
C  Write summary table of predicted and measured growth and development
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      2-28-93
C-----------------------------------------------------------------------
C  INPUT  : TRTNO,NOUTDO,FILEA,IDETO,IDETV,
C           HI,CROP,ISDATE,MDATE,YIELD,SKERWT,GPSM,GPP,MAXLAI,PBIOMS,
C           STOVER,XGNP,TOTNUP,APTNUP,GNUP,BIOMAS,CGPE,CLAI,NYRS,NSENS,
C           YRSIM,CANWAA,CANNAA,XN
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPHARV
C
C  Calls  : READA CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPTAB (TRTNO,NOUTDO,FILEA,IDETO,RNMODE,IDETS,
     &   CROP,ISDATE,MDATE,YIELD,GPSM,GPP,MAXLAI,PBIOMS,
     &   STOVER,XGNP,TOTNUP,APTNUP,GNUP,BIOMAS,NYRS,
     &   YRSIM,YRPLT,IPLTI,NREP,TITLET,FBIOM,EYEWT,PMDATE,
     &   DNR1,DNR7,ISENS)

      IMPLICIT     NONE

      INCLUDE     'XFIELD.BLK'

      CHARACTER*1  RNMODE,IDETO,IDETS,ANS,IPLTI
      CHARACTER*2  CROP
      CHARACTER*12 FILEA
      CHARACTER*25 TITLET

      INTEGER      NOUTDO,TRTNO,NYRS,MDATE,ISDATE,YRSIM,YRPLT,PMDATE
      INTEGER      TIMDIF,DNR1,DNR3,DNR7,DFLR,DMAT,NREP,PMAT,ISENS

      REAL         MAXLAI,PBIOMS,APTNUP,STOVER,GPP,TOTNUP
      REAL         XGNP,GPSM,YIELD,GNUP,BIOMAS
      REAL         FBTONS,FYIELD,PVEGWT,FBIOM,EYEWT

C
C     Initialize observed values to -99 before reading values
C
      IFLR   =     -99
      IFPD   =     -99
      IFSD   =     -99
      IMAT   =     -99
      XGWT   = '   -99'
      XPDW   = '   -99'
      XNOGR  = '   -99'
      XGWU   = '   -99'
      XNOGU  = '   -99'
      XCWT   = '   -99'
      XSWT   = '   -99'
      XLAM   = '   -99'
      XHIN   = '   -99'
      XNGR   = '   -99'
      XNST   = '   -99'
      XNTP   = '   -99'
      XNPS   = '   -99'
      XTHR   = '   -99'
      XCWAA  = '   -99'
      XCNAA  = '   -99'
      XLFNO  = '   -99'

      FBTONS = FBIOM*0.01
      PBIOMS = (BIOMAS*10.0)/1000.0
      FYIELD = YIELD/1000.0
      PVEGWT = STOVER/1000.0

      IF (IDETO .EQ. 'Y' .OR. RNMODE .EQ. 'I' .OR. IDETS .EQ. 'Y' .OR.
     &    IDETS .EQ. 'A') THEN
         CALL READA (FILEA,TRTNO,YRSIM)
      ENDIF

C-----------------------------------------------------------------------
C     Summary output to screen
C-----------------------------------------------------------------------

      IF (IFLR .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
         DFLR = TIMDIF (YRPLT,IFLR)
       ELSE
         DFLR = -99
      ENDIF
      IF (IMAT .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
         DMAT = TIMDIF (YRPLT,IMAT)
       ELSE
         DMAT = -99
      ENDIF
      IF (IFPD .GT. 0 .AND. IPLTI .EQ. 'R') THEN
         PMAT = TIMDIF (YRPLT,IFPD)
       ELSE
         PMAT = -99
      ENDIF
      DNR1 = TIMDIF (YRPLT,ISDATE)
      IF (DNR1 .LE. 0) THEN
         DNR1 = -99
      ENDIF
      DNR3 = TIMDIF (YRPLT,MDATE)
      IF (DNR3 .LE. 0) THEN
         DNR3 = -99
      ENDIF
      DNR7 = TIMDIF (YRPLT,PMDATE)
      IF (DNR7 .LE. 0) THEN
         DNR7 = -99
      ENDIF

      IF (RNMODE .EQ. 'I' .AND. NYRS .LE. 1) THEN
         WRITE (*,100)
         READ  (5,'(A1)') ANS
         CALL CLEAR
         WRITE (*,300)
            IF (INDEX ('PI',CROP) .GT. 0) THEN
                WRITE (*,400) DNR1,DFLR,DNR3,PMAT,DNR7,DMAT,
     &          FYIELD,XGWT,EYEWT,XGWU,GPSM,XNOGR,GPP,XNOGU,
     &          MAXLAI,XLAM,FBTONS,XCWT,PBIOMS,XSWT,
     &          PVEGWT,XPDW,XGNP,XNPS,
     &          TOTNUP,XNTP,APTNUP,XNST,GNUP,XNGR
            ENDIF
         ENDIF

C-----------------------------------------------------------------------
C     Summary output to file
C-----------------------------------------------------------------------

      IF (IDETO .EQ. 'Y') THEN
         WRITE (NOUTDO,300)
         IF (INDEX ('PI',CROP) .GT. 0) THEN
              WRITE (NOUTDO,400) DNR1,DFLR,DNR3,PMAT,DNR7,DMAT,
     &        FYIELD,XGWT,EYEWT,XGWU,GPSM,XNOGR,GPP,XNOGU,
     &        MAXLAI,XLAM,FBTONS,XCWT,PBIOMS,XSWT,
     &        PVEGWT,XPDW,XGNP,XNPS,
     &        TOTNUP,XNTP,APTNUP,XNST,GNUP,XNGR
         ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     Print warning message
C-----------------------------------------------------------------------

      IF (RNMODE .EQ. 'I' .AND. NYRS .LE. 1) THEN
         WRITE (*,200) NREP,TITLET
         READ (5,'(A1)') ANS
         CALL CLEAR
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  100 FORMAT (/,40X,' Press < ENTER > key to continue  ',$)
  200 FORMAT (/,'*RUN ',I3,4X,': ',A25,
     &          '... Press < ENTER > key to continue',$)
  300 FORMAT (//,'*MAIN GROWTH AND DEVELOPMENT VARIABLES',//,
     &   '@',5X,'VARIABLE',24X,'PREDICTED',5X,'MEASURED',/,
     &       6X,8('-'),24X,9('-'),5X,8('-'))
  400 FORMAT (6X, 'FORCING DATE (DAP)             ',2X,I6,7X,I6,/,
     &        6X, 'HARVEST DATE (DAP)             ',2X,I6,7X,I6,/,
     &        6X, 'PHYSIO. MATURITY DATE          ',2X,I6,7X,I6,/,
     &        6X, 'FRESH FRUIT YIELD (T/HA)       ',F8.2, 7X,A6,/,
     &        6X, 'EYE WEIGHT (G)                 ',F8.3, 7X,A6,/,
     &        6X, 'EYE PER SQ METER               ',F8.0, 7X,A6,/,
     &        6X, 'EYE PER FRUIT                  ',F8.2, 7X,A6,/,
     &        6X, 'MAX. LAI                       ',F8.2, 7X,A6,/,
     &        6X, 'DRY BIOM @ FORC. (T/HA)        ',F8.2, 7X,A6,/,
     &        6X, 'DRY BIOMASS (T/HA)             ',F8.2, 7X,A6,/,
     &        6X, 'DRY VEG. WT. (T/HA)            ',F8.2, 7X,A6,/,
     &        6X, 'FRUIT N%                       ',F8.2, 7X,A6,/,
     &        6X, 'TOT N UPTAKE (KG N/HA)         ',F8.1, 7X,A6,/,
     &        6X, 'VEGETATIVE N UPTAKE            ',F8.1, 7X,A6,/,
     &        6X, 'FRUIT N UPTAKE                 ',F8.1, 7X,A6)
      END


C=======================================================================
C  READA, Subroutine
C
C  Read development and final harvest data from FILEA
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      2-28-93
C-----------------------------------------------------------------------
C  INPUT  : FILEA,TRTNO,YRSIM
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPTAB
C
C  Calls  : YR_DOY ERROR IGNORE
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE READA (FILEA,TRTNO,YRSIM)

      IMPLICIT      NONE

      INCLUDE      'XFIELD.BLK'

      INTEGER       TRTNO,ERRNUM,LUNA,IFIND,LINEXP,ISECT,NTR,I
      INTEGER       YRSIM,YR,ISIM,J

      CHARACTER*4   OLAB(40)
      CHARACTER*5   HEAD(40),HD
      CHARACTER*6   ERRKEY,DAT(40)
      CHARACTER*6   XFLR,XFPD,XFSD,XMAT
      CHARACTER*12  FILEA
      CHARACTER*255 C255

      LOGICAL         FEXIST

      DATA OLAB/'FDAT','HDAT','PDFT','MDAT','FWAH','PWAM',
     &          'E#AM','EWUM','E#UM','CWAM','BWAH','LAIX','HIAM',
     &          'THAM','GNAM','CNAM','SNAM','GN%M','CWAA','CNAA',
     &          'L#SM',19*'    '/

      ERRKEY = 'READA '
      XFLR   = '      '
      XFPD   = '      '
      XFSD   = '      '
      XMAT   = '      '
      LUNA   = 8
      LINEXP = 0
      IFIND  = 0

      INQUIRE (FILE = FILEA, EXIST = FEXIST)

      IF (FEXIST) THEN
         OPEN (LUNA,FILE = FILEA,STATUS = 'OLD',IOSTAT=ERRNUM)
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEA,0)
         CALL YR_DOY (YRSIM,YR,ISIM)
C
C        Find the header line, designated by @TRNO
C
  10     READ (LUNA,'(A)',END=200) C255
         LINEXP = LINEXP + 1
         IF (C255(1:1) .NE. '@') GO TO 10
C
C        Found header line, save it in head and search for treatment
C
         DO I = 1, 40
            READ (C255,300) HEAD(I)
            C255 = C255(7:255)
         END DO
C
C        Find the right treatment line of data
C
         DO I = 1, 1000
            CALL IGNORE (LUNA,LINEXP,ISECT,C255)
C
C           Return if no matching treatment is found in file FILA
C           No field measured data are necessary to be able to run the
C           model
C
            IF (ISECT .EQ. 0) GO TO 100
            READ (C255(1:6),500,IOSTAT=ERRNUM) NTR

            IF (NTR .EQ. TRTNO) GO TO 60
         END DO

  60     CONTINUE
C
C        Read data line
C
         DO I = 1, 40
            READ (C255,700) DAT(I)
            C255 = C255(7:255)
         END DO
C
C        Match header with data
C
         DO I = 2, 40
            HD = HEAD(I)
            DO J = 1, 21
               IF (OLAB(J) .EQ. HD(1:4) .OR. OLAB(J) .EQ. HD(2:5)) THEN
                  IF (J .EQ. 1) THEN
                      XFLR  = DAT(I)
                   ELSEIF (J .EQ.  2) THEN
                      XFPD  = DAT(I)
                   ELSEIF (J .EQ.  3) THEN
                      XFSD  = DAT(I)
                   ELSEIF (J .EQ.  4) THEN
                      XMAT  = DAT(I)
                   ELSEIF (J .EQ.  5) THEN
                      XGWT  = DAT(I)
                   ELSEIF (J .EQ.  6) THEN
                      XPDW  = DAT(I)
                   ELSEIF (J .EQ.  7) THEN
                      XNOGR = DAT(I)
                   ELSEIF (J .EQ.  8) THEN
                      XGWU  = DAT(I)
                   ELSEIF (J .EQ.  9) THEN
                      XNOGU = DAT(I)
                   ELSEIF (J .EQ. 10) THEN
                      XCWT  = DAT(I)
                   ELSEIF (J .EQ. 11) THEN
                      XSWT  = DAT(I)
                   ELSEIF (J .EQ. 12) THEN
                      XLAM  = DAT(I)
                   ELSEIF (J .EQ. 13) THEN
                      XHIN  = DAT(I)
                   ELSEIF (J .EQ. 14) THEN
                      XTHR  = DAT(I)
                   ELSEIF (J .EQ. 15) THEN
                      XNGR  = DAT(I)
                   ELSEIF (J .EQ. 16) THEN
                      XNTP  = DAT(I)
                   ELSEIF (J .EQ. 17) THEN
                      XNST  = DAT(I)
                   ELSEIF (J .EQ. 18) THEN
                      XNPS  = DAT(I)
                   ELSEIF (J .EQ. 19) THEN
                      XCWAA = DAT(I)
                   ELSEIF (J .EQ. 20) THEN
                      XCNAA = DAT(I)
                   ELSEIF (J .EQ. 21) THEN
                      XLFNO = DAT(I)
                  ENDIF
               ENDIF
            END DO
         END DO

         GO TO 100

 200     CALL ERROR (ERRKEY,1,FILEA,LINEXP)

 100     CONTINUE

         READ (XFLR(1:6),1000) IFLR
         READ (XFPD(1:6),1000) IFPD
         READ (XFSD(1:6),1000) IFSD
         READ (XMAT(1:6),1000) IMAT

         IF (IFLR .GT. 0 .AND. IFLR .LT. 1000) THEN
            IF (IFLR .GT. ISIM) THEN
               IFLR = YR*1000 + IFLR
             ELSE
               IFLR = (YR+1)*1000 + IFLR
            ENDIF
         ENDIF

         IF (IFPD .GT. 0 .AND. IFPD .LT. 1000) THEN
            IF (IFPD .GT. ISIM) THEN
               IFPD = YR*1000 + IFPD
             ELSE
               IFPD = (YR+1)*1000 + IFPD
            ENDIF
         ENDIF

         IF (IFSD .GT. 0 .AND. IFSD .LT. 1000) THEN
            IF (IFSD .GT. ISIM) THEN
               IFSD = YR*1000 + IFSD
             ELSE
               IFSD = (YR+1)*1000 + IFSD
            ENDIF
         ENDIF

         IF (IMAT .GT. 0 .AND. IMAT .LT. 1000) THEN
            IF (IMAT .GT. ISIM) THEN
               IMAT = YR*1000 + IMAT
             ELSE
               IMAT = (YR+1)*1000 + IMAT
            ENDIF
         ENDIF

         CLOSE (LUNA)
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  300 FORMAT (1X,A5)
  500 FORMAT (2X,I4)
  700 FORMAT (A6)
 1000 FORMAT (I6)

      END

C=======================================================================
C  OPSTRS, Subroutine
C
C  Writes stress summary to file
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      2-28-93
C-----------------------------------------------------------------------
C  INPUT  : IDETO,IDETV,NOUTDO,CROP,NYRS
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPHARV
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPSTRS (IDETO,RNMODE,NOUTDO,CROP,NYRS)

      IMPLICIT     NONE

      INCLUDE     'COWTH.BLK'

      CHARACTER*1  IDETO,RNMODE
      CHARACTER*2  CROP
      CHARACTER*23 STAG(5)

      INTEGER      NOUTDO,I,STTOT,NYRS

      STTOT = 5

      IF (INDEX ('PI',CROP) .GT. 0) THEN
         STAG(1) = 'Emerg - end stem growth'
         STAG(2) = 'End ZSG to forcing     '
         STAG(3) = 'Forcing to SCY         '
         STAG(4) = 'SCY to 1st open flower '
         STAG(5) = 'Fruit enlargement      '
      ENDIF

      IF (RNMODE .EQ. 'I' .AND. NYRS .LE. 1) THEN
         WRITE (*,500)
      ENDIF
      IF (IDETO .EQ. 'Y') WRITE (NOUTDO,500)

      DO I = 1, STTOT
      IF (RNMODE .EQ. 'I' .AND. NYRS .LE. 1) THEN
            WRITE (*,600) STAG(I),NNR(I),TMAXR(I),TMINR(I),RADR(I),
     &                DAYLR(I),SWFCR(I),TURFR(I),NSTRR(I),AGEFR(I)
         ENDIF
         IF (IDETO .EQ. 'Y') THEN
            WRITE (NOUTDO,600) STAG(I),NNR(I),TMAXR(I),TMINR(I),RADR(I),
     &                     DAYLR(I),SWFCR(I),TURFR(I),NSTRR(I),AGEFR(I)
         ENDIF

      END DO

      IF (RNMODE .EQ. 'I' .AND. NYRS .LE. 1) THEN
         WRITE (*,1100)
      ENDIF
      IF (IDETO .EQ. 'Y') WRITE (NOUTDO,1100)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 500  FORMAT (//,'*ENVIRONMENTAL AND STRESS FACTORS',//,
     & 36('-'),'ENVIRONMENT',17('-'),'STRESS',10('-'),/,1X,
     & '|--DEVELOPMENT PHASE--|-TIME-|-------WEATHER--------|',1X,
     & '|---WATER--| |-NITROGEN-|'/,25X,'DURA',1X,'TEMP  TEMP',
     & '  SOLAR PHOTOP', 1X,'PHOTO GROWTH PHOTO GROWTH',/,25X,'TION',
     & 2X,'MAX   MIN    RAD  [day]',' SYNTH',8X,'SYNTH',9X,/,
     & 25X,'days',3X,'øC',4X,'øC',3X,'MJ/m2',3X,'hr',/,80('-'))
c 500 FORMAT (//,'*ENVIRONMENTAL AND STRESS FACTORS',//,
c    & 36('-'),'ENVIRONMENT',17('-'),'STRESS',10('-'),/,1X,
c    & '|--DEVELOPMENT PHASE--|-TIME-|-------WEATHER--------|',1X,
c    & '|---WATER--| |-NITROGEN-|'/,25X,'DURA',1X,'TEMP  TEMP',
c    & '  SOLAR PHOTOP', 1X,'PHOTO  LEAF  PHOTO  LEAF ',/,25X,'TION',
c    & 2X,'MAX   MIN    RAD  [day]',' SYNTH',' EXPAN.',1X,'SYNTH ',
c    & ' EXPAN.',2X,/,25X,'days',3X,'oC',4X,'oC',3X,'MJ/m2',3X,'hr',
c    & /,80('-'))
  600 FORMAT (1X,A23,1X,I3,2(1X,F6.2),1X,F5.2,1X,F6.2,4(1X,F5.3))
 1100 FORMAT (/,55X,'(0.0 = Minimum Stress',
     &        /,55X,' 1.0 = Maximum Stress)')

      END
