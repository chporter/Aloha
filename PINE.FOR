C=======================================================================
C   PIALO980, Main program
C
C   ALOHA-PINEAPPLE MODEL
C
C   August 1997
C
C
C   Aloha-Pineapple model developed by Jingbo Zhang and Duane Bartholomew
C   using some routines of ceres-maize model developed by Ritchie,Kiniry,
C   Jones,Kneivel,Singh and others in July, 1988. Nitrogen
C   routines adapted from ceres-maize developed by Godwin,Jones, et al
C   IBSNAT DSSAT I/O structures adapted from Soygro by C. Zickos
C   and D. Godwin, ifdc.
C
C   Version 3.5 has :
C
C   1. Population effects on leaf growth, fruit development,
C      and fruit yield.
C   2. Weather effects on growth and development.
C   3. Initial plant size effect on growth.
C   4. Plant size at the time of forcing on fruit size and fruit yield.
C   5. Water balance but nitrogen balance have not been tested.
C
C   The new input/output structures and several new features such as
C   automatic planting/harvesting and crop sequencing were done by
C   Thornton, Singh, and Smallwood (IFDC); Baer (Michigan State);
C   Hoogenboom (Georgia); and Jones, Bowen and Pickering (Florida).
C
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                     See above
C  2. Header revision and minor changes           P.W.W.      2-7-93
C  3. Changed call to WATBAL (Added AIRAMT)       B.D.B.   24-FEB-94
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  :
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : None
C
C  Calls  : ERROR CLEAR SOILRI YR_DOY WEATHER SOILNI NBAL SOILT
C           NTRANS WATBAL AUTPLT PHENOL GROSUB OPDAY OPHARV INPHEN
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  ANS    :
C=======================================================================

      IMPLICIT     NONE

      INCLUDE     'GEN1.BLK'
      INCLUDE     'GEN2.BLK'
      INCLUDE     'GEN3.BLK'
      INCLUDE     'GEN4.BLK'
      INCLUDE     'NTRC1.BLK'
      INCLUDE     'NTRC2.BLK'
      INCLUDE     'PREDOB.BLK'
      INCLUDE     'SWITCH.BLK'

      CHARACTER*1  DAYFAC(10),RADFAC(10),TXFAC (10),TMFAC(10),PRCFAC(10)
      CHARACTER*1  CO2FAC(10),DPTFAC(10),WNDFAC(10),WMODB,TYPEIO,UPCASE
      CHARACTER*2  PRCROP
      CHARACTER*5  REPARG,PLME,SLTX,TIMPL(10),CHMET(10),CHCOD(10)
      CHARACTER*5  CHMCOD(10)
      CHARACTER*6  ERRKEY,VARNO
      CHARACTER*8  FLDNAM,EXPER
      CHARACTER*10 CROPD,STNAME(20)
      CHARACTER*12 FILEIO
      CHARACTER*25 MODEL,TITLET
      CHARACTER*60 ENAME

      INTEGER   TRTNO,NAPNIT,NAPRES,YRDOY,NHAR
      INTEGER   HDATE(3),NEV,WMDATE(10),YRPLT,YRSIM
      INTEGER   YEAR,NREP,NYRS,PWDINF,PWDINL,HLATE,HDLAY,YRIC
      INTEGER   ERRNUM,FTYPEN,HSTG(3),ENDSIM,INCYD
      INTEGER   STGDOY(20),DAP,HAREND,MULTI,TIMDIF,DAS,RSEED1,RSEED(4)
      INTEGER   ROTOPT,ROTNO,CRPNO,NAP,LUNIO,ISENS
      INTEGER   EDATE,TDATE(10),CDATE(10),NTLAP,NCHEM,NPSTAP,NTIL

      REAL      TOTIR,DSOILN,SOILNC,SOILNX,SWPLTL,SWPLTH,SWPLTD,PTX,PTTN
      REAL      CO2ADJ(10),DAYADJ(10),DPTADJ(10),PRCADJ(10),RADADJ(10)
      REAL      TMADJ(10),TXADJ(10),WNDADJ(10)
      REAL      TSWINI,TSIN,TNOX,BWAH,SDWTAH
      REAL      CANWAA,CANNAA,DAYL,TRUNOF,TDRAIN,AMTNIT,SDWTPL
      REAL      PGFAC3,EFINOC,EFNFIX,STHETA(3),XLONG,
     +          SDSOIL(3),AIRAMT,XLAT,SNDN,SNUP,SDRATE,TOPWT,
     +          AINO3,AINH4,WTNSD,PBIOMS,WTNFX,WTNCAN,WTNUP,SDWTAM
      REAL      FDINT,HBPC(3),TDAY,RHUM,XELEV,WTHADJ(2,8)
      REAL      HDAY,CUMDPT,FX,WW,DP,TAV,TAMP,Z(20),B,ALBEDO,ATOT,TMA(5)
      REAL      CHAMT(10),TOTPST(10),H2OLOS(10),SEPLOS(10)
      REAL      SEEDNI,WTNLF,WTNST,WTNSH,WTNRT,WTNLO
      LOGICAL   FEXIST

      INTEGER   TS
      INTEGER*2 IP

      PARAMETER (TS=24)

      REAL    AZZON(TS),BETA (TS),FRDIFP(TS),FRDIFR(TS),
     &        PARHR(TS),RADHR(TS),RHUMHR(TS),TAIRHR(TS),WINDHR(TS)

C-----------------------------------------------------------------------
C     Initialize
C-----------------------------------------------------------------------

      PARAMETER (ERRKEY = 'PINE  ')
      PARAMETER (LUNIO  = 21)

      NOUTDG = 30
      NOUTDW = 31
      NOUTDN = 32
      NOUTDD = 33
      NOUTDC = 34
      NOUTDP = 35
      NOUTDO = 36
      NOUTDS = 37
      NOUTDR = 38
      NOUTDH = 40
      NOUTDE = 41
      NOUTDY = 48
      NOUTDZ = 49

C-----------------------------------------------------------------------
C     MODELV and CROP should be modified when model versions change
C     or when a crop specific model is created.
C
C     CROP   = ALO for the Aloha model Version 3.50 (98.0)
C     CROP   = PI for ALOHA Pineapple  Version 3.50 (98.0)
C
C-----------------------------------------------------------------------
C     Get argument from runtime module to determine path and run mode
C-----------------------------------------------------------------------
C     Microsoft Fortran V5.1 - Standard Fortran F77 Implementation
C-----------------------------------------------------------------------

      CALL GETARG (1,FILEIO,IP)
      CALL GETARG (2,TYPEIO,IP)
      CALL GETARG (3,RNMODE,IP)
      CALL GETARG (4,REPARG,IP)

C-----------------------------------------------------------------------
C     SUN Fortran V1.3.1 - Standard Fortran F77 Implementation
C-----------------------------------------------------------------------
C
C-SUN CALL GETARG (1,FILEIO)
C-SUN CALL GETARG (2,TYPEIO)
C-SUN CALL GETARG (3,RNMODE)
C-SUN CALL GETARG (4,REPARG)
C
C-----------------------------------------------------------------------

      TYPEIO = UPCASE(TYPEIO)
      IF (INDEX('IXR',TYPEIO) .EQ. 0) THEN
         TYPEIO = 'I'
      ENDIF
      RNMODE = UPCASE(RNMODE)
      IF (INDEX('IANQGSF',RNMODE) .EQ. 0) THEN
         RNMODE = 'I'
      ENDIF
      READ (REPARG(1:5),'(I5)') NREP
      IF (NREP .EQ. 0) THEN
         NREP = 1
      ENDIF

C-----------------------------------------------------------------------
C     Open intermediate model input file
C-----------------------------------------------------------------------

      IF (TYPEIO .NE. 'I') THEN
         CALL ERROR (ERRKEY,1,FILEIO,0)
      ENDIF
      OPEN (LUNIO, FILE = FILEIO,STATUS = 'UNKNOWN',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) THEN
         CALL ERROR (ERRKEY,ERRNUM,FILEIO,0)
      ENDIF

C-----------------------------------------------------------------------
C     Beginning of seasonal simulation loop
C-----------------------------------------------------------------------

      MULTI    = 1
      NYRS     = 0

200   CONTINUE
      ENDSIM = 0

      IF (NYRS .GT. 1) THEN
         MULTI = MULTI + 1
      ENDIF

C-----------------------------------------------------------------------
C     Call PLANT initialization routine to set variables to 0
C-----------------------------------------------------------------------

      CALL INPLNT (TOTIR,NAPNIT,TRUNOF,TDRAIN,CANNAA,CANWAA,AMTNIT,NAP,
     &             TLCH,TNOX,CUMDTT,SUMDTT,DTT,SEEDNI,
     &             WTNLF,WTNST,WTNSH,WTNRT,WTNLO,BWAH,SDWTAH,
     &             TOPWT,SDWTAM,WTNUP,NAPRES)

C-----------------------------------------------------------------------
C     Call IPIBS .. Read in IBSNAT31.INP file
C-----------------------------------------------------------------------

      CALL IPIBS (PGFAC3,NEV,AIRAMT,EFINOC,EFNFIX,
     &            WMDATE,DAYFAC,RADFAC,TXFAC,TMFAC,PRCFAC,CO2FAC,
     &            DPTFAC,WNDFAC,DAYADJ,RADADJ,TXADJ,TMADJ,PRCADJ,
     &            CO2ADJ,DPTADJ,WNDADJ,HSTG,HDLAY,HLATE,HDATE,
     &            PWDINF,PWDINL,SWPLTL,SWPLTH,SWPLTD,PTX,PTTN,
     &            DSOILN,SOILNC,SOILNX,ROTNO,ROTOPT,CRPNO,FLDNAM,
     &            MULTI,LUNIO,CROPD,SDSOIL,STHETA,MODEL,NYRS,YRSIM,
     &            TRTNO,YRPLT,NHAR,RSEED1,YEAR,EXPER,ENAME,TITLET,NREP,
     &            VARNO,PLME,FTYPEN,YRIC,HBPC,PRCROP,SLTX,NCHEM,NTIL,
     &            TDATE,TIMPL,CDATE,CHCOD,CHMET,CHAMT,PLANTSIZE,SDWTPL,
     &            ISENS)
      
      WTINITIAL = SDWTPL/(PLTPOP*10.0)        ! kg/ha  --> g/plt

      IF (CROP .NE. 'FA') THEN

C-----------------------------------------------------------------------
C        Call IPCROP
C-----------------------------------------------------------------------

         CALL IPCROP (FILEC,PATHCR,CROP)

C-----------------------------------------------------------------------
C        Call IPPARM Ä¿
C                     ÃÄÄÄÄ Pest and Disease Initialization
C        Call IPPROG ÄÙ
C-----------------------------------------------------------------------

         IF (ISWDIS .EQ. 'Y') THEN
            CALL IPPARM (FILEP,PATHPE)
            CALL IPPROG (YRPLT,TRTNO,FILET)
         ENDIF

      ENDIF

C-----------------------------------------------------------------------
C     Call IPSOIL
C-----------------------------------------------------------------------

      CALL IPSOIL ('RE001',DMINR,RCN,DSNC,RDCHO,RDCEL,RDLIG,PRCHO,
     &              PRCEL,PRLIG)

C-----------------------------------------------------------------------
C     Call SEQuence Analysis INput Variables from previous model run
C-----------------------------------------------------------------------

      IF (INDEX('FQ',RNMODE) .GT. 0) THEN
         INQUIRE (FILE = 'TMP.DAT',EXIST = FEXIST)
         IF (FEXIST) THEN
            MESIC = 'S'
            CALL INSEQ(WRESR,YRSIM,NREP,RSEED,NLAYR,DEPMAX,ESW,
     &                 SUMES1,SUMES2,TLL,PESW,CUMDEP,TSAT,SWDEF,T,TSW,
     &                 ATHETA,SW,DLAYR,GNP,CNSD1,CNSD2,ATANC,ANFAC,
     &                 DMINR,FPOOL,WFY,TFY,PHN,CNI,FOM,FON,HUM,NHUM,
     &                 UREA,UPPM,ATOT,BD,PH,OC,NO3,TMA,NH4,FAC,SNO3,
     &                 SNH4,ANO3,ANH4,TIFON,TIFOM,TSON,EFNFIX,EFINOC,
     &                 PRCHO,PRCEL,PRLIG)

C-----------------------------------------------------------------------
C    Increment doy of year and adjust all date values
C-----------------------------------------------------------------------

            YRSIM = INCYD(YRSIM,1)
            CALL DATECS (FILEW,YRSIM,YRPLT,
     &           JULWTB,JULAPL,NAPW,RESDAY,NRESAP,FDAY,NFERT,NTBL,
     &           HLATE,PWDINF,PWDINL,NHAR,HDATE,MEWTH,CROP,
     &           IIRRI,IFERI,IHARI,IRESI,ISWCHE,ISWTIL,NCHEM,NTIL,
     &           CDATE,TDATE)
            NREP = NREP + 1
          ELSE
            MESIC = 'M'
         ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     Set total number of chemical and tillage applications
C-----------------------------------------------------------------------

      NPSTAP = NCHEM
      NTLAP  = NTIL

C-----------------------------------------------------------------------
C     Call PHENOLGY initialization routine
C-----------------------------------------------------------------------

      CALL INPHEN (CROP,STGDOY,MDATE,YRSIM,STNAME,ISWNIT,XSTAGE,
     &             TANC,PLTPOP,STOVWT,SEEDNI,ROOTN,STOVN,GRAINN,HAREND)

      YRDOY  = INCYD (YRSIM,-1)

C-----------------------------------------------------------------------
C     Are we running all treatments?
C-----------------------------------------------------------------------

      IF (RNMODE .EQ. 'I' .AND. NYRS .LE. 1) THEN
         CALL CLEAR
      ENDIF

C-----------------------------------------------------------------------
C     Beginning of daily simulation loop
C-----------------------------------------------------------------------

      DO WHILE (YRDOY .GT. HAREND)

         YRDOY = INCYD (YRDOY,1)
         CALL YR_DOY (YRDOY,YEAR,DOY)
         DAS   = MAX (0,TIMDIF(YRSIM,YRDOY))
         DAP   = MAX (0,TIMDIF(YRPLT,YRDOY))

         CALL WEATHR (CO2ADJ,CO2FAC,DAYADJ,DAYFAC,DOY,DPTADJ,DPTFAC,
     &     FILEW,PRCADJ,PRCFAC,RADADJ,RADFAC,TMADJ,TMFAC,
     &     TXADJ,TXFAC,WMODB,WNDADJ,WNDFAC,WMDATE,
     &     AZZON,BETA,CLOUDS,CO2,DAYL,FRDIFP,FRDIFR,NEV,PAR,
     &     PARHR,RADHR,RAIN,REFHT,RHUMHR,SNDN,SNUP,SRAD,TAIRHR,TAMP,
     &     TAV,TAVG,TDAY,TDEW,TMAX,TMIN,WINDHR,WINDHT,WINDSP,XLAT,XLONG,
     &     YRDOY,YRSIM,PATHWT,MEWTH,MULTI,NREP,RSEED1,RSEED,RNMODE,
     &     RHUM,XELEV,WTHADJ)

C-----------------------------------------------------------------------
C        Initialize soil water and nitrogen variables for each soil layer
C-----------------------------------------------------------------------

         IF (YRDOY .EQ. YRSIM) THEN
            IF (NINT(XCRD*100.0) .EQ. 0) THEN
               XCRD = XLAT
            ENDIF
            IF (NINT(YCRD*100.0) .EQ. 0) THEN
               YCRD = XLONG
            ENDIF
            IF (NINT(ELEV*100.0) .EQ. 0) THEN
               ELEV = XELEV
            ENDIF
            S1     = SIN (XLAT*0.01745)
            C1     = COS (XLAT*0.01745)

C-----------------------------------------------------------------------
C           Call INSOIL Subroutine to calculate initial conditions
C           for each soil layer
C-----------------------------------------------------------------------

            CALL INSOIL (ISWWAT,AINO3,ANO3,AINH4,ANH4,TSWINI)

C-----------------------------------------------------------------------
C           Call INSOILT to calculate initial soil temperature
C           for each soil layer
C-----------------------------------------------------------------------

            TEMPM = (TMAX + TMIN)*0.5
            IF (ISWWAT .EQ. 'Y') THEN
               CALL INSOILT (MESIC,TAVG,XLAT,TMAX,SRAD,DOY,HDAY,CUMDPT,
     &                      Z,BD,FX,DP,WW,B,ALBEDO,TAV,TAMP,TMA,ATOT,ST,
     &                      NLAYR,DLAYR,SALB,PESW)
            ENDIF

C-----------------------------------------------------------------------
C           Call SOILNI to calculate initial nitrogen conditions
C           for each soil layer
C-----------------------------------------------------------------------

            IF ((ISWNIT .EQ. 'Y') .AND. (MESIC .EQ. 'M')) THEN

               CALL SOILNI (AINO3,AINH4,TSIN,DMOD,WRESR,NLAYR,DLAYR,
     &                  DEPMAX,SW,LL,DUL,SAT,UREA,BD,PH,FON,FOM,
     &                  HUM,FAC,NO3,NH4,SNO3,SNH4,OC,NHUM,FPOOL,
     &                  CNI,WFY,TFY,ST,PHN,TIFON,TIFOM,TSON,TSOC,
     &                  ANO3,ANH4,RCN,PRCHO,PRCEL,PRLIG,TOTN,CNRAT)

C-----------------------------------------------------------------------
C              Call INRES Subroutine to calculate initial application of
C              residue left over from previous crop
C-----------------------------------------------------------------------

               CALL INRES (YRDOY,YRIC,FPOOL,FON,FOM,ICRES,ICRIP,
     &           ICREN,ICRID,DLAYR,NLAYR,TIFOM,TIFON,PRCHO,PRCEL,PRLIG,
     &           DMINR,RCN,DSNC,RDCHO,RDCEL,RDLIG)
            ENDIF

C-----------------------------------------------------------------------
C           Call NBAL to check N balance
C-----------------------------------------------------------------------

            IF (ISWNIT .EQ. 'Y' .AND. IDETN .EQ. 'Y') THEN
               CALL NBAL (1,TSON,TIFON,ANO3,ANH4,TLCH,TNOX,WTNUP,YRDOY,
     &                 WTNFX,SEEDNI,NFERT,ANFER,WTNLF,WTNST,WTNSD,WTNSH,
     &                 WTNRT,WTNLO,NRESAP,RESIDUE,RESN)
            ENDIF
         ENDIF

C-----------------------------------------------------------------------
C        Call SOILT to calculate soil temperature for each soil layer
C-----------------------------------------------------------------------

         TEMPM = (TMAX + TMIN)*0.5
         IF (ISWWAT .EQ. 'Y') THEN
            CALL SOILT (DOY,TMAX,TAVG,SRAD,HDAY,CUMDPT,PESW,NLAYR,
     &                  ATOT,TMA,WW,FX,B,DP,TAV,TAMP,Z,ST,ALBEDO)
         ENDIF

C-----------------------------------------------------------------------
C        Define dates for water balance calculations
C
C        YRSIM = Start of Simulation Date
C        YRPLT = Planting Date
C        EDATE = Emergence Date
C        MDATE = Maturity Date
C        YRDOY = Year - Day of Year (Dynamic Variable)
C-----------------------------------------------------------------------

         EDATE = STGDOY(9)

C-----------------------------------------------------------------------
C        Calculate light interception for transpiration.
C        Note: this is discontinues function at LAI = 3.
C        Consider CROPGRO approach with KCAN later.
C-----------------------------------------------------------------------

         IF (LAI .LE. 3.0) THEN
           FDINT = 1.0 - EXP(-LAI)
         ELSE
           FDINT = 1.0
         ENDIF
C        FDINT = 1.0 - EXP(-(KCAN+0.1)*XHLAI)

C-----------------------------------------------------------------------
C        Generic WATer BALance routine implemented in CERES and CROPGRO
C-----------------------------------------------------------------------

         CALL WATBAL (TOTIR,YRDOY,TRUNOF,TDRAIN,NAP,IIRRI,ISWNIT,
     &                MEEVP,AIRAMT,DAP,YRSIM,YRPLT,MDATE,ISWWAT,
     &                EDATE,FDINT,CROP,ISWCHE,MEHYD)

         IF (ISWWAT .EQ. 'Y') THEN
            !
            !  WRESR growth and depth routine
            !
            IF (GRORT .GT. 0.0) THEN
               CALL ROOTGR (ISWNIT,GRORT)
            ENDIF
         ENDIF

         IF (ISWNIT .EQ. 'Y') THEN
            CALL NTRANS (YRDOY,IFERI,TSIN,NAPNIT,AMTNIT,FTYPEN,DSOILN,
     &           SOILNC,SOILNX,TNOX,DOY,DLAYR,DUL,LL,
     &           SW,SAT,NLAYR,SWEF,DAP,IRESI,NAPRES)
         ENDIF

C-----------------------------------------------------------------------
C        Call AUTPLT if automatic planting on, determine IPLT
C-----------------------------------------------------------------------

         IF (DAP .EQ. 0 .AND. IPLTI .EQ. 'A') THEN
            CALL AUTPLT (DLAYR,LL,DUL,SW,ST,PWDINF,PWDINL,SWPLTL,SWPLTH,
     &                   SWPLTD,PTX,PTTN,YRPLT,YEAR,YRDOY,IPLT,STGDOY,
     &                   MDATE,IDETO,NOUTDO)
         ENDIF

!================================
!     chp temp debug
      nh4 = 1.0
      no3 = 1.0
      sw = dul
!================================

C-----------------------------------------------------------------------
C        Call GROSUB
C----------------------------------------------------------------------
      
         IF (ISTAGE .LT. 6) THEN
            IF (CROP .EQ. 'PI') THEN
               CALL GROSUB  (yrdoy,NOUTDO,ISWNIT,IDETO)
            ENDIF
         ENDIF

         IF (YRDOY .EQ. STGDOY(3)) THEN
            CANNAA = STOVN*PLTPOP
            CANWAA = BIOMAS
         ENDIF

C-----------------------------------------------------------------------
C        Call PHENOL
C-----------------------------------------------------------------------

         IF (YRDOY .EQ. YRPLT .OR. ISTAGE .NE. 7) THEN
            CALL PHENOL (STGDOY,YRDOY,XLAT)
         ENDIF

C-----------------------------------------------------------------------
C        Call AUTHAR to check harvest switch & determine HAREND
C-----------------------------------------------------------------------

         CALL AUTHAR (DLAYR,DUL,HAREND,HDATE,HDLAY,HLATE,HSTG,IHARI,LL,
     &                SW,SWPLTD,SWPLTH,SWPLTL,YRDOY,MDATE,STGDOY,DAP,
     &                IDETO,NOUTDO)

C-----------------------------------------------------------------------
C        Call daily output subroutine
C-----------------------------------------------------------------------

         CALL OPDAY (MODEL,TRTNO,DAP,YRSIM,YRPLT,NREP,TITLET,EXPER,
     &               ENAME,MULTI,CROPD,STGDOY,YRDOY,TOTIR,NYRS,DAS,
     &               DAYL,TRUNOF,TDRAIN,AMTNIT,NAP,HAREND,NPSTAP,
     &               STNAME,WTNLF,WTNST,WTNSD,WTNSH,WTNRT)

C-----------------------------------------------------------------------
C        Call summary output subroutine
C-----------------------------------------------------------------------

         IF (RNMODE .EQ. 'I' .OR. IDETO .EQ. 'Y'
     &                       .OR. IDETS .NE. 'N') THEN
            CALL SUMWTH (ISTAGE,TMAX,TMIN,RAIN,DAYL,SRAD,ET,TURFAC,
     &                   SWFAC,YRDOY,YRSIM,HAREND,NSTRES,AGEFAC)
         ENDIF

         CUMDTT = CUMDTT + DTT

      END DO                                         ! End daily loop

C-----------------------------------------------------------------------
C     Call end of season output routine
C-----------------------------------------------------------------------

      IF (ISWWAT .EQ. 'Y' .AND. IDETW .EQ. 'Y') THEN
          CALL WBAL (YRSIM,YRDOY,TSWINI,TSW,TOTIR,CRAIN,TRUNOF,
     &               TDRAIN,CEP,CES,EFFIRR,CEO)
      ENDIF

      CALL OPHARV (TRTNO,YRDOY,YRSIM,YRPLT,CROP,CROPD,
     &   WTNSD,NAP,TOTIR,CRAIN,CET,TRUNOF,PESW,TDRAIN,TSON,TSOC,TLCH,
     &   NAPNIT,ISDATE,MDATE,YIELD,SKERWT,GPSM,GPP,MAXLAI,
     &   PBIOMS,STOVER,XGNP,TOTNUP,APTNUP,GNUP,BIOMAS,
     &   NYRS,FLDNAM,EXPER,WTNFX,WTNCAN,TSIN,WTNUP,NREP,AMTNIT,
     &   SDWTAM,TITLET,STGDOY,ENAME,SDWT,
     &   ROTNO,ROTOPT,CRPNO,SDRATE,TOPWT,AMTRES,HBPC,FBIOM,EYEWT,
     &   PMDATE,FHDATE,WTINITIAL,BWAH,SDWTAH,
     &   TSEDM,TRON,TOTPST,H2OLOS,SEPLOS,CHMCOD,ISENS)

      IF (ISWNIT .EQ. 'Y' .AND. IDETN .EQ. 'Y') THEN
         CALL NBAL (2,TSON,TIFON,ANO3,ANH4,TLCH,TNOX,WTNUP,YRDOY,
     &                 WTNFX,SEEDNI,NFERT,ANFER,WTNLF,WTNST,WTNSD,WTNSH,
     &                 WTNRT,WTNLO,NRESAP,RESIDUE,RESN)
      ENDIF

C-----------------------------------------------------------------------
C     Call routine to output field operations.
C-----------------------------------------------------------------------

      OPEN (UNIT = NOUTDR, FILE = OUTR, STATUS = 'UNKNOWN')
      IF (RNMODE .EQ. 'F') THEN
         SDRATE = (SKERWT*PLTPOP)/0.8 * 10.0
         CALL OPOPS (YRPLT,NRESAP,RESDAY,NAPNIT,FDAY,NAP,
     &      JULAPL,HAREND,PLME,SDRATE,RESIDUE,RESCOD,ANFER,IFTYPE,
     &      AMIR,YIELD*0.1,BIOMAS,LFWT*PLTPOP,CROP,VARNO,IPLTI,IRESI,
     &      IFERI,IIRRI,IHARI,NOUTDR,HPC,HBPC,IAME,HCOM,
     &      FERCOD,TDATE,NTLAP,ISWTIL,TIMPL,RMET,CHMET,CHAMT,
     &      ISWCHE,CHCOD,NPSTAP,CDATE,IRRCOD)
       ELSE
         CLOSE (UNIT = NOUTDR, STATUS = 'DELETE')
      ENDIF

C-----------------------------------------------------------------------
C     Call SEQuence Analysis OUTput Variables to write for next model run
C-----------------------------------------------------------------------

      IF (INDEX('QF',RNMODE) .GT. 0) THEN
         !
         ! Check .. units are lb/acre .. check all and convert
         !
         CALL OPSEQ (NREP,YRDOY,RSEED,TMA,ATOT,PLTPOP,
     &        DEPMAX,NLAYR,ESW,SUMES1,SUMES2,T,TLL,PESW,TSW,CUMDEP,
     &        TSAT,SWDEF,ATHETA,SW,RTWT,STOVN,ROOTN,GNP,CNSD1,CNSD2,
     &        ATANC,ANFAC,DMINR,FPOOL,WFY,TFY,PHN,CNI,FOM,FON,HUM,
     &        NHUM,UREA,UPPM,BD,PH,OC,NO3,NH4,FAC,SNO3,SNH4,
     &        TOPWT,SDWT,BWAH,SDWTAH,WTNSD)
      ENDIF

C-----------------------------------------------------------------------
C     Are we done?
C-----------------------------------------------------------------------

      IF (MULTI .NE. NYRS) THEN
         REWIND (LUNIO)
         CLOSE  (11)                                 ! Close weather file
         GO TO 200
      ENDIF

      CLOSE (LUNIO)                                  ! Close weather file
      CALL OPDONE                                    ! Close all files

      IF (IDETS .EQ. 'N') THEN
         CLOSE (UNIT = NOUTDR, STATUS = 'DELETE')
         CLOSE (UNIT = NOUTDS, STATUS = 'DELETE')
         CLOSE (UNIT = NOUTDE, STATUS = 'DELETE')
      ELSE IF (IDETS .EQ. 'Y') THEN
         CLOSE (UNIT = NOUTDR)
         CLOSE (UNIT = NOUTDS)
         CLOSE (UNIT = NOUTDE)
      ELSE IF (IDETS .EQ. 'A') THEN
         CLOSE (UNIT = NOUTDR)
         CLOSE (UNIT = NOUTDS)
         CLOSE (UNIT = NOUTDE)
      ENDIF

      END
