C=======================================================================
C  OPDAY, Subroutine
C
C  Generates output for simulated data
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P. Wilkens  2-8-93
C  3. Added switch block, etc.                      P. Wilkens  2-8-93
C-----------------------------------------------------------------------
C  INPUT  : MODEL,TRTNO,DAP,YRSIM,YRPLT,CROPD,STGDOY,
C           YRDOY,TOTIR,NYRS,DAS,DAYL,TRUNOF,TDRAIN,AMTNIT,NAP
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : PINE
C
C  Calls  : CLEAR OPWBAL OPNIT OPGROW OPPEST OPCARB OPVIEW
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPDAY (MODEL,TRTNO,DAP,YRSIM,YRPLT,NREP,TITLET,EXPER,
     &         ENAME,MULTI,CROPD,STGDOY,YRDOY,TOTIR,NYRS,DAS,
     &         DAYL,TRUNOF,TDRAIN,AMTNIT,NAP,HAREND,NPSTAP,STNAME,
     &         WTNLF,WTNST,WTNSD,WTNSH,WTNRT)

      IMPLICIT  NONE

      INCLUDE  'GEN1.BLK'
      INCLUDE  'GEN2.BLK'
      INCLUDE  'GEN3.BLK'
      INCLUDE  'GEN4.BLK'
      INCLUDE  'NTRC1.BLK'
      INCLUDE  'NTRC2.BLK'
      INCLUDE  'PREDOB.BLK'
      INCLUDE  'SWITCH.BLK'
      INCLUDE  'PEST.BLK'
      INCLUDE  'PHOS.BLK'

      CHARACTER*8   EXPER
      CHARACTER*10  CROPD,STNAME(20)
      CHARACTER*25  MODEL,TITLET
      CHARACTER*60  ENAME
      CHARACTER*210 GROHEAD(4)
      CHARACTER*240 WATHEAD(4)
      CHARACTER*252 NITHEAD(4)
      CHARACTER*252 PHOHEAD(4),PSTHEAD(1),CARHEAD(4)
      CHARACTER*512 HEAD

      INTEGER LUNOV,NAVWB,I,M,HAREND,NREP,NPSTAP
      INTEGER TRTNO,DAP,YRSIM,YRPLT,YRDOY,MULTI
      INTEGER NYRS,RSTAGE,DAS,STGDOY(20),NAP

      REAL    DSTRES,STRESP,STRESS,NFIXN,MAINR
      REAL    DAYL,TRUNOF,TDRAIN,WTNCAN,AMTNIT,WTNUP,TSIN
      REAL    SLA,PODWT,PODNO,TOTIR,STRESN,CWSV,CWSR
      REAL    WTPSD,AVEP,AVES,AVET,AVEO,AVTMX,AVTMN,AVWTD
      REAL    WTLF,SEEDNO,WTNLF,WTNST,WTNSD,WTNSH,WTNFX,WTNRT
      REAL    PCNL,PCNST,PCNRT,PCNSH,VSTAGE,CANHT,CANWH
      REAL    DISLA,DISLAP,WLIDOT,WSIDOT,WRIDOT,SWIDOT,WSHIDT,SDIDOT
      REAL    SHIDOT,CASM,PG,GROWTH,GRWRES,CADLF,CADST,CMINEA
      REAL    TOTWT,RHOL,PCINPN,XANC,AVSRAD,PPLTD,LAIDT
      REAL    CPPLTD,CLFM,CSTEM,CSDM,CSDN,CSHM,CSHN,RLVDOT,ASMDOT
      REAL    RHOS,PGN,CRLV,RLFDOT,CRLF,CRTM
      REAL    PCINPD,SLWSLN,SLWSHN,PNLSLN,PNLSHN,LMXSLN,LMXSHN

      PARAMETER (LUNOV  =  6)

      DATA GROHEAD(1)/'!YR    Days   Leaf  Grow                      Dry
     1 Weight                           Pod      Phot. Grow       Leaf  
     2Shell  Spec    Canopy          Root  ³                Root Length 
     3Density                     ³'/
      DATA GROHEAD(2)/'! and  after  Num  Stage  LAI   Leaf  Stem Fruit
     1 Root Basal Crown  Suck   HI   Wgt.   No.    Water     Nit.   Nit 
     2  -ing  Leaf  Hght Brdth       Depth  ³                cm3/cm3  of
     3 soil                        ³'/
      DATA GROHEAD(3)/'!  DOY plant                    ³<---------------
     1 kg/Ha -------------->³       Kg/Ha        ³<Stress (0-1)>³    %  
     2   %   Area    m     m           m   ³<---------------------------
     3--------------------------->³'/
      DATA GROHEAD(4)/'@DATE   CDAY  L#SD  GSTD  LAID  LWAD  SWAD  FWAD 
     1 RWAD  BWAD  CRAD  SUGD  HIAD  EWAD  E#AD  WSPD  WSGD  NSTD  LN%D 
     2 SH%D  SLAD  CHTD  CWID  EWSD  RDPD  RL1D  RL2D  RL3D  RL4D  RL5D 
     3 RL6D  RL7D  RL8D  RL9D  RL10'/

      DATA WATHEAD(1)/'!YR     Days  Daily Evapotran.  PESW       Cumula
     1tive          Ave  Temp. Temp   Day   Day  Cumu. Evap.        No.
     2                          Soil water in Layer                     
     3                    Soil Temperature in Layer'/
      DATA WATHEAD(2)/'! and   after Plant Total Pot.       RunOff Drain
     1 Prcip Irr    Sol   Max   Min   Len  Soil Plant  Soil  Total   of
     2          1     2     3     4     5     6     7     8     9    10
     3     1     2     3     4     5     6     7     8     9    10'/
      DATA WATHEAD(3)/'!  DOY  Plant ³<---- mm ---->³   mm  ³<--------mm
     1--------->³ MJ/m2    C     C     hr  Evap ³<------mm----->³   irr 
     2      ³<----------------------- cm3/cm3 ----------------------->³ 
     3³<-------------------------- C -------------------------->³'/
      DATA WATHEAD(4)/'@DATE   CDAY  EPAA  ETAA  EOAA  SWXD  ROFC  DRNC 
     1 PREC  IRRC  SRAA  TMXA  TMNA  DAYD  ESAA  EPAC  ESAC  ETAC  IR#C 
     2 DTWT  SW1D  SW2D  SW3D  SW4D  SW5D  SW6D  SW7D  SW8D  SW9D  SW10 
     3 TS1D  TS2D  TS3D  TS4D  TS5D  TS6D  TS7D  TS8D  TS9D  TS10'/

      DATA NITHEAD(1)/'!YR     Days      Nitrogen      Nitrogen    Inorg
     1  Fix  Up-  leach  Soil  Soil  Leaf  Stem  Leaf  Stem  Shell Root 
     2 Nfix  Total Soil  ³<------------- Nitrate for Soil Layer --------
     3--------->³  ³<--------------- Ammonium for Soil Layer -----------
     4--->³'/
      DATA NITHEAD(2)/'! and   After Crop  Grain Veg. Grain  Veg.  N Fer
     1t      take       Inorg   Org    N     N     N     N     N     N  
     2 Rate   NO3   NH4    1     2     3     4     5     6     7     8  
     3   9    10     1     2     3     4     5     6     7     8     9  
     4  10'/
      DATA NITHEAD(3)/'!  DOY  Plant ³<--- Kg/Ha -->³ ³<-- % -->³  ³<---
     1--------- kg/ha ----------->³  ³<-kg/ha->³ ³<-------- % ------->³k
     2g/ha/d ³<-kg/ha->³ ³<---------------------------------------------
     3------- ug N/g soil ----------------------------------------------
     4--->³'/
      DATA NITHEAD(4)/'@DATE   CDAY  CNAD  GNAD  VNAD  GN%D  VN%D  NAPC 
     1 NFXC  NUPC  NLCC  NIAD  NOAD  LNAD  SNAD  LN%D  SN%D  SHND  RN%D 
     2 NFXD  NITD  NHTD  NI1D  NI2D  NI3D  NI4D  NI5D  NI6D  NI7D  NI8D 
     3 NI9D  NI10  NH1D  NH2D  NH3D  NH4D  NH5D  NH6D  NH7D  NH8D  NH9D 
     4 NH10'/

      DATA PHOHEAD(1)/'!YR     Days     Phosphorus    Phosphorus   Inorg
     &  P Up'/
      DATA PHOHEAD(2)/'! and   After Crop  Grain Veg. Grain  Veg.  P Fer
     &  take'/
      DATA PHOHEAD(3)/'!  DOY  Plant ³<--- kg/ha -->³ ³<-- % -->³  ³<---
     1-------- kg/ha ----------->³  ³<-kg/ha->³ ³<-------- % ------->³k
     &'/
      DATA PHOHEAD(4)/'@DATE   CDAY  CPAD  GPAD  VPAD  GP%D  VP%D  PAPC
     & PUPC  PIRE  PIAC  PIST'/

      DATA PSTHEAD(1)/'@DATE   CDAY  DLA   DLA%  DLAI  DLFM  DSTM  DSDM
     1 DSD#  DSHM  DSH#  DRTM  DRLV  DRLF  DASM  DPO%  CLAI  CLFM  CSTM
     2 CSDM  CSD#  CSHM  CSH#  CRTM  CRLV  CRLF  CASM  CPO%'/

      DATA CARHEAD(1)/'!YR    Days
     1
     2                    Res           Soil                  Res
     3    Soil                     User   '/
      DATA CARHEAD(2)/'! and  after  OC
     1
     2                     C             C                     N
     3     N                      N    C  '/
      DATA CARHEAD(3)/'!  DOY plant ³t/Ha³
     1
     2                   ³<------------ kg/Ha------------>³  ³<---------
     3--- kg/Ha------------>³  ³<-kg/Ha>³'/
      DATA CARHEAD(4)/'@DATE   CDAY  SOCD  TWAD  LI%D  PHAD  CMAD  CGRD
     1 GRAD  MRAD  CHAD  CL%D  CS%D  OMAC  PHAN  LI%N  SLLN  SLHN  N%LN
     2 N%HN  LMLN  LMHN  TFOM  HUM1  HUM2  HUM3  HUM4  HUM5  TFON  NHU1
     3 NHU2  NHU3  NHU4  NHU5  SNDD  SCDD'/

C
C     Initialize Phosphorus variables to zero, for now
C
      TRPU   = 0.0

      DO I = 1, 20
         PSTABL(I) = 0.0
         PACTIV(I) = 0.0
         PRESIN(I) = 0.0
      END DO

C-----------------------------------------------------------------------
C     Initialize counters, averages
C-----------------------------------------------------------------------

      IF (YRDOY .EQ. YRSIM) THEN
         DSTRES = 0.0
         STRESP = 0.0
         STRESS = 0.0
         STRESN = 0.0
         CWSV   = 0.0
         CWSR   = 0.0
         WTNUP  = 0.0

C-----------------------------------------------------------------------
C        Generate headings for output files
C-----------------------------------------------------------------------

         IF (IDETO .EQ. 'Y') THEN
            OPEN (UNIT=NOUTDO, FILE=OUTO, STATUS='UNKNOWN',
     &            ACCESS='APPEND')
         ENDIF
         IF (IDETG .EQ. 'Y') THEN
            OPEN (UNIT=NOUTDG, FILE=OUTG, STATUS='UNKNOWN',
     &            ACCESS='APPEND')
         ENDIF
         IF (IDETW .EQ. 'Y') THEN
            OPEN (UNIT=NOUTDW, FILE=OUTW, STATUS='UNKNOWN',
     &            ACCESS='APPEND')
         ENDIF
         IF (IDETN .EQ. 'Y') THEN
            OPEN (UNIT=NOUTDN, FILE=OUTN, STATUS='UNKNOWN',
     &            ACCESS='APPEND')
         ENDIF
         IF (IDETC .EQ. 'Y') THEN
            OPEN (UNIT=NOUTDC, FILE=OUTC, STATUS='UNKNOWN',
     &            ACCESS='APPEND')
         ENDIF
         IF (IDETD .EQ. 'Y') THEN
            OPEN (UNIT=NOUTDD, FILE=OUTD, STATUS='UNKNOWN',
     &            ACCESS='APPEND')
         ENDIF
         IF (IDETP .EQ. 'Y') THEN
            OPEN (UNIT=NOUTDP, FILE=OUTP, STATUS='UNKNOWN',
     &            ACCESS='APPEND')
         ENDIF
         IF (MULTI .GT. 1) THEN
            IF (IDETG .EQ. 'Y') THEN
               WRITE (NOUTDG,100) NREP,TITLET,MODEL,CROPD,EXPER,CROP,
     &                            ENAME,TRTNO,TITLET
            ENDIF
            IF (IDETW .EQ. 'Y') THEN
               WRITE (NOUTDW,100) NREP,TITLET,MODEL,CROPD,EXPER,CROP,
     &                            ENAME,TRTNO,TITLET
            ENDIF
            IF (IDETN .EQ. 'Y') THEN
               WRITE (NOUTDN,100) NREP,TITLET,MODEL,CROPD,EXPER,CROP,
     &                            ENAME,TRTNO,TITLET
            ENDIF
            IF (IDETC .EQ. 'Y') THEN
               WRITE (NOUTDC,100) NREP,TITLET,MODEL,CROPD,EXPER,CROP,
     &                            ENAME,TRTNO,TITLET
            ENDIF
            IF (IDETD .EQ. 'Y') THEN
               WRITE (NOUTDD,100) NREP,TITLET,MODEL,CROPD,EXPER,CROP,
     &                            ENAME,TRTNO,TITLET
            ENDIF
            IF (IDETP .EQ. 'Y') THEN
               WRITE (NOUTDP,100) NREP,TITLET,MODEL,CROPD,EXPER,CROP,
     &                            ENAME,TRTNO,TITLET
            ENDIF
         ENDIF

C-----------------------------------------------------------------------
C        Variable heading for GROWTH.OUT
C-----------------------------------------------------------------------

         IF (IDETG .EQ. 'Y') THEN
            IF (IDETL .EQ. 'Y') THEN
               WRITE (NOUTDG,2192) GROHEAD(1)
               WRITE (NOUTDG,2192) GROHEAD(2)
               WRITE (NOUTDG,2192) GROHEAD(3)
               WRITE (NOUTDG,2192) GROHEAD(4)
             ELSE
               WRITE (NOUTDG,2190) GROHEAD(1)
               WRITE (NOUTDG,2190) GROHEAD(2)
               WRITE (NOUTDG,2190) GROHEAD(3)
               WRITE (NOUTDG,2190) GROHEAD(4)
            ENDIF
         ENDIF

C-----------------------------------------------------------------------
C        Variable heading for WATER.OUT
C-----------------------------------------------------------------------

         IF (IDETW .EQ. 'Y') THEN
            IF (IDETL .EQ. 'Y') THEN
               WRITE (NOUTDW,2090) WATHEAD(1)
               WRITE (NOUTDW,2090) WATHEAD(2)
               WRITE (NOUTDW,2090) WATHEAD(3)
               WRITE (NOUTDW,2090) WATHEAD(4)
             ELSE
               WRITE (NOUTDW,2190) WATHEAD(1)
               WRITE (NOUTDW,2190) WATHEAD(2)
               WRITE (NOUTDW,2190) WATHEAD(3)
               WRITE (NOUTDW,2190) WATHEAD(4)
            ENDIF
         ENDIF

C-----------------------------------------------------------------------
C        Variable heading for NITROGEN.OUT
C-----------------------------------------------------------------------

         IF (IDETN .EQ. 'Y') THEN
            IF (IDETL .EQ. 'Y') THEN
               WRITE (NOUTDN,2240) NITHEAD(1)
               WRITE (NOUTDN,2240) NITHEAD(2)
               WRITE (NOUTDN,2240) NITHEAD(3)
               WRITE (NOUTDN,2240) NITHEAD(4)
             ELSE
               WRITE (NOUTDN,2190) NITHEAD(1)
               WRITE (NOUTDN,2190) NITHEAD(2)
               WRITE (NOUTDN,2190) NITHEAD(3)
               WRITE (NOUTDN,2190) NITHEAD(4)
            ENDIF
         ENDIF

C-----------------------------------------------------------------------
C        Variable heading for PHOSPHOR.OUT
C-----------------------------------------------------------------------

         IF (IDETP .EQ. 'Y') THEN
            IF (IDETL .EQ. 'Y') THEN
               WRITE (NOUTDP,2340) PHOHEAD(1)
               WRITE (NOUTDP,2340) PHOHEAD(2)
               WRITE (NOUTDP,2340) PHOHEAD(3)
               WRITE (NOUTDP,2340) PHOHEAD(4)
             ELSE
               WRITE (NOUTDP,2190) PHOHEAD(1)
               WRITE (NOUTDP,2190) PHOHEAD(2)
               WRITE (NOUTDP,2190) PHOHEAD(3)
               WRITE (NOUTDP,2190) PHOHEAD(4)
            ENDIF
         ENDIF

C-----------------------------------------------------------------------
C        Variable heading for PEST.OUT
C-----------------------------------------------------------------------

         IF (IDETD .EQ. 'Y') THEN
            IF (IDETL .EQ. 'Y') THEN
               WRITE (NOUTDD,2090) PSTHEAD(1)
             ELSE
               WRITE (NOUTDD,2190) PSTHEAD(1)
            ENDIF
         ENDIF

C-----------------------------------------------------------------------
C        Variable heading for CHEMICAL.OUT
C-----------------------------------------------------------------------

         IF (IDETH .EQ. 'Y' .AND. NPSTAP .GT. 0) THEN
            WRITE (HEAD(1:12),'(A12)') '@DATE   CDAY'
            DO I = 1, NPSTAP
               M = 13 + (I-1)*24
               WRITE (HEAD(M:M+24),2196)
     &         ' PRN',I,' PSD',I,' PDR',I,' PTR',I
            END DO
            IF (IDETL .EQ. 'Y') THEN
               M = 13 + 24*NPSTAP
               DO I = 1, NPSTAP
                  WRITE (HEAD(M:M+60),2197)
     &            ' P1D',I,' P2D',I,' P3D',I,' P4D',I,
     &            ' P5D',I,' P6D',I,' P7D',I,' P8D',I,' P9D',I,' P10',I
                  M = M + 60
               END DO
            ENDIF
            WRITE(NOUTDH,'(A512)') HEAD
         ENDIF

C-----------------------------------------------------------------------
C        Variable heading for CARBON.OUT
C-----------------------------------------------------------------------

         IF (IDETC .EQ. 'Y') THEN
            IF (IDETL .EQ. 'Y') THEN
               WRITE (NOUTDC,2090) CARHEAD(1)
               WRITE (NOUTDC,2090) CARHEAD(2)
               WRITE (NOUTDC,2090) CARHEAD(3)
               WRITE (NOUTDC,2090) CARHEAD(4)
             ELSE
               WRITE (NOUTDC,2190) CARHEAD(1)
               WRITE (NOUTDC,2190) CARHEAD(2)
               WRITE (NOUTDC,2190) CARHEAD(3)
               WRITE (NOUTDC,2190) CARHEAD(4)
            ENDIF
         ENDIF

      ENDIF


C-----------------------------------------------------------------------
C   Calculate average values as a function of the output interval
C-----------------------------------------------------------------------

      NAVWB  = NAVWB  + 1
      AVEP   = AVEP   + EP
      AVES   = AVES   + ES
      AVET   = AVET   + ET
      AVEO   = AVEO   + EO
      AVTMX  = AVTMX  + TMAX
      AVTMN  = AVTMN  + TMIN
      AVSRAD = AVSRAD + SRAD
      AVWTD  = AVWTD  + WTDEP
      WTLF   = LFWT   * PLTPOP
      SDWT   = GRNWT  * PLTPOP
      XLAI   = LAI
      IF (WTLF .GT. 0.0) THEN
         SLA  = LAI * 10000 / WTLF
       ELSE
         SLA = 0.0
      ENDIF
      SEEDNO = GPSM
      PODWT  = CRWNWT
      IF (GPP .GT. 0.0) THEN
         PODNO = SEEDNO/GPP
       ELSE
         PODNO = 0.0
      ENDIF

      WTNUP = WTNUP + TRNU * PLTPOP
C
C     Check for output frequency
C
      IF (YRDOY .EQ. HAREND) FROP = 1

      IF ((MOD(DAS,FROP) .EQ. 0) .OR. YRDOY .EQ. STGDOY(6)
     &                           .OR. YRDOY .EQ. YRPLT) THEN
         AVES   = AVES   / NAVWB
         AVEP   = AVEP   / NAVWB
         AVET   = AVET   / NAVWB
         AVEO   = AVEO   / NAVWB
         AVTMX  = AVTMX  / NAVWB
         AVTMN  = AVTMN  / NAVWB
         AVSRAD = AVSRAD / NAVWB
         AVWTD  = AVWTD  / NAVWB

C-----------------------------------------------------------------------
C        The following generates output for file WATER.OUT
C-----------------------------------------------------------------------

         IF (IDETW .EQ. 'Y') THEN
             CALL OPWBAL (NOUTDW,YRDOY,DAP,DAYL,CRAIN,TOTIR,TRUNOF,
     &          TDRAIN,CES,CEP,CET,PESW,SW,ST,IDETL,AVES,AVEP,AVET,
     &          AVEO,AVTMX,AVTMN,AVSRAD,NAP,AVWTD)
         ENDIF

         NAVWB  = 0
         AVES   = 0.0
         AVEP   = 0.0
         AVET   = 0.0
         AVEO   = 0.0
         AVTMX  = 0.0
         AVTMN  = 0.0
         AVSRAD = 0.0
         AVWTD  = 0.0

C-----------------------------------------------------------------------
C        The following generates output for file NITROGEN.OUT
C-----------------------------------------------------------------------

         IF (IDETN .EQ. 'Y' .AND. ISWNIT .EQ. 'Y') THEN
            WTNCAN = (STOVN + GRAINN) * PLTPOP
            IF ((LFWT+STMWT) .GT. 0.0) THEN
               WTNLF = STOVN * (LFWT  / STOVWT) * PLTPOP
               WTNST = STOVN * (STMWT / (LFWT + STMWT)) * PLTPOP
             ELSE
               WTNLF = 0.0
               WTNST = 0.0
            ENDIF
            WTNSD = GRAINN * PLTPOP
            WTNRT = ROOTN * PLTPOP        ! Is this right?
            WTNSH = 0.0
            WTNFX = 0.0
            NFIXN = 0.0
            ANO3  = 0.0
            ANH4  = 0.0
            DO I = 1, NLAYR
               !
               ! Changed * to / -- 12-27-93 ... WTB & PWW
               !
               ANO3 = ANO3 + NO3(I) / FAC(I)
               ANH4 = ANH4 + NH4(I) / FAC(I)
            END DO
            TSIN = ANO3 + ANH4

            IF (LFWT .GT. 0.0) THEN
               PCNL = WTNLF /( LFWT * PLTPOP) * 100.0
             ELSE
               PCNL = 0.0
            ENDIF
            IF (STMWT .GT. 0.0) THEN
               PCNST = WTNST/(STMWT * PLTPOP) * 100.0
             ELSE
               PCNST = 0.0
            ENDIF
            IF (RTWT .GT. 0.0) THEN
               PCNRT = ROOTN/RTWT * 100.0
             ELSE
               PCNRT = 0.0
            ENDIF
            CALL OPNIT (NOUTDN,YRDOY,DAP,WTNCAN,WTNLF,WTNST,
     &          WTNSD,WTNSH,WTNUP,TLCH,TSIN,WTNFX,
     &          NO3,NH4,WTLF,STMWT,IDETL,SDWT,TSON,NFIXN,
     &          ANO3,ANH4,AMTNIT,PCNL,PCNST,PCNRT,PCNSH,PLTPOP)
         ENDIF

C-----------------------------------------------------------------------
C        The following generates output for file PHOSPHOR.OUT
C-----------------------------------------------------------------------

         IF (IDETP .EQ. 'Y') THEN
            APIRE  = 0.0
            APIAC  = 0.0
            APIST  = 0.0
            WTPCAN = (STOVP + GRAINP) * PLTPOP
            IF ((LFWT+STMWT) .GT. 0) THEN
               WTPLF = STOVP * (LFWT  / (LFWT + STMWT)) * PLTPOP
               WTPST = STOVP * (STMWT / (LFWT + STMWT)) * PLTPOP
             ELSE
               WTPLF = 0.0
               WTPST = 0.0
            ENDIF
            WTPSD = GRAINP * PLTPOP
            WTPSH = 0.0
            WTPUP = WTPUP + TRPU * PLTPOP
            DO I = 1, NLAYR
               APIRE = APIRE + PRESIN(I) / FAC(I)
               APIAC = APIAC + PACTIV(I) / FAC(I)
               APIST = APIST + PSTABL(I) / FAC(I)
            END DO
            IF (LFWT .GT. 0) THEN
               PCPL = WTPLF/(LFWT  * PLTPOP) * 100.0
             ELSE
               PCPL = 0.0
            ENDIF
            IF (STMWT .GT. 0.0) THEN
               PCPST = WTPST/(STMWT * PLTPOP) * 100.0
             ELSE
               PCPST = 0.0
            ENDIF
            IF (RTWT .GT. 0.0) THEN
               PCPRT = ROOTP/RTWT * 100.0
             ELSE
               PCPRT = 0.0
            ENDIF
            CALL OPPHO (NOUTDP,YRDOY,DAP,WTPCAN,WTPLF,WTPST,
     &         WTPSD,WTPSH,WTPUP,WTLF,STMWT,IDETL,SDWT,
     &         PLTPOP,AMTPHO,APIRE,APIAC,APIST)
         ENDIF

C-----------------------------------------------------------------------
C        The following generates output for GROWTH.OUT
C-----------------------------------------------------------------------

         IF (YRDOY .GE. YRPLT) THEN
            IF (IDETG .EQ. 'Y') THEN
               LEAFNO = LN
               VSTAGE = REAL (LEAFNO)
               IF (LEAFNO .GT. 0.0) THEN
                  RSTAGE = REAL(ISTAGE)
                ELSE
                  RSTAGE = 0.0
               ENDIF
               SDWT = GRNWT

               CALL OPGROW (NOUTDG,YRDOY,DAP,VSTAGE,XLAI,STMWT,
     &              SDWT,WTLF,BIOMAS,RTWT,PODWT,PODNO,SEEDNO,SLA,
     &              PCNL,TURFAC,CANHT,CANWH,RLV,IDETL,RTDEP,RSTAGE,
     &              SWFAC,NSTRES,PLTPOP,FRTWT,BASLFWT,CRWNWT,
     &              SKWT,SATFAC)
            ENDIF

C-----------------------------------------------------------------------
C           The following generates output for file PEST.OUT
C-----------------------------------------------------------------------

            IF (IDETD .EQ. 'Y') THEN
              CALL OPPEST (NOUTDD,YRDOY,DAP,IDETL,DISLA,DISLAP,WLIDOT,
     &           WSIDOT,WRIDOT,SWIDOT,WSHIDT,SDIDOT,SHIDOT,PPLTD,CPPLTD,
     &           CLFM,CLAI,CSTEM,LAIDT,CSDM,CSDN,CSHM,CSHN,RLVDOT,CRLV,
     &           RLFDOT,CRLF,CRTM,ASMDOT,CASM )
            ENDIF

C-----------------------------------------------------------------------
C           The following generates output for file CHEMICAL.OUT
C-----------------------------------------------------------------------

C-AD  IF (IDETH .EQ. 'Y' .AND. NPSTAP .GT. 0) THEN
C-AD &   CALL OPCHEM(NOUTDH,DAP,YRDOY,NPSTAP,H2OLOS,SEDLOS,
C-AD &          DRGLOS,TRNLOS,PST,IDETL)
C-AD  ENDIF

C-----------------------------------------------------------------------
C           The following generates output for file OPCARB
C-----------------------------------------------------------------------

            IF (IDETC .EQ. 'Y') THEN
               CALL OPCARB (NOUTDC,DAP,YRDOY,PCINPD,PG,GROWTH,MAINR,
     &              GRWRES,CADLF,CADST,CMINEA,TSOC,AMTRES,TOTWT,RHOL,
     &              RHOS,IDETL,PGN,PCINPN,SLWSLN,SLWSHN,PNLSLN,PNLSHN,
     &              LMXSLN,LMXSHN,DSNC,NHUM,HUM,TIFOM,TIFON)
            ENDIF
         ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     Call OPVIEW
C-----------------------------------------------------------------------

      CALL OPVIEW (NREP,TITLET,RNMODE,NOUTDO,IDETO,NYRS,DAP,BIOMAS,LAI,
     &    XN,CET,CRAIN,TOTIR,DOY,PESW,TURFAC,SWFAC,NSTRES,CWSV,CWSR,
     &    STGDOY,YRDOY,YRSIM,XANC,STOVN,GRAINN,PLTPOP,LUNOV,STNAME)

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

 100  FORMAT (/,'*RUN ',I3,8X,': ',A25,/,
     &       1X,'MODEL',10X,':'1X,A8,' - ',A10,/,
     &       1X,'EXPERIMENT',5X,':',1X,A8,1X,A2,4X,A47,/,
     &       1X,'TREATMENT',1X,I2, 3X,':',1X,A25,/)
 2190 FORMAT (A80)
 2192 FORMAT (A210)
 2196 FORMAT (4(A5,I1))
 2197 FORMAT (10(A5,I1))
 2090 FORMAT (A240)
 2240 FORMAT (A252)
 2340 FORMAT (A252)

      END

C=======================================================================
C  OPGROW, Subroutine
C
C  Generates output for growth data
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P. Wilkens  2-8-93
C  3. Added switch block, etc.                      P. Wilkens  2-8-93
C-----------------------------------------------------------------------
C  INPUT  : NOUTDG,YRDOY,DAP,VSTAGE,XLAI,STMWT,SDWT,WTLF,BIOMAS,RTWT,PODWT,
C           PODNO,SEEDNO,SLA,PCNL,TURFAC,CANHT,CANWH,RLV,IDETL,RTDEP,RSTAGE,
C           SWFAC,NSTRES,PLTPOP
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPDAY
C
C  Calls  :
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

       SUBROUTINE OPGROW (NOUTDG,YRDOY,DAP,VSTAGE,XLAI,STMWT,
     &    SDWT,WTLF,BIOMAS,RTWT,PODWT,PODNO,SEEDNO,
     &    SLA,PCNL,TURFAC,CANHT,CANWH,RLV,IDETL,RTDEP,RSTAGE,
     &    SWFAC,NSTRES,PLTPOP,FRTWT,BASLFWT,CRWNWT,SKWT,SATFAC)

      IMPLICIT  NONE

      CHARACTER IDETL*1

      INTEGER   DAP,NOUTDG,YRDOY,I,RSTAGE

      REAL      VSTAGE,XLAI,STMWT,SDWT,WTLF,BIOMAS,RTWT,PODWT,SEEDNO
      REAL      SLA,PCNL,TURFAC,CANHT,CANWH,RLV(20),HI,SHELPC,SHELLW
      REAL      SDSIZE,PODNO,RTDEP,NSTRES,SWFAC,PLTPOP,GM2KG
      REAL      FRTWT,BASLFWT,CRWNWT,SKWT,SATFAC
C
C     GM2KG converts gm/plant to kg/ha
C
      GM2KG  = PLTPOP * 10.0
      SHELPC = 0.0
      IF (PODWT .GT. 0.1) THEN
         SHELPC = SDWT*100.0/PODWT
      ENDIF
      SHELLW = PODWT - SDWT
      SDSIZE = 0.0
      IF (SEEDNO .GT. 0.0) THEN
         SDSIZE = SDWT*PLTPOP/SEEDNO*1000.0
      ENDIF
      HI     = 0.0
      IF (BIOMAS .GT. 0.0 .AND. SDWT .GE. 0.0) THEN
         HI = SDWT*PLTPOP/BIOMAS
      ENDIF

      IF (IDETL .EQ. 'N') THEN
         WRITE (NOUTDG,300) MOD(YRDOY,100000),DAP,VSTAGE,RSTAGE,XLAI,
     &      NINT(WTLF*10.0),NINT(STMWT*GM2KG),NINT(FRTWT*GM2KG),
     &      NINT(RTWT*GM2KG),NINT(BASLFWT*10.0),NINT(CRWNWT*GM2KG),
     &      NINT(SKWT*GM2KG),HI
       ELSE IF (IDETL .EQ. 'Y') THEN
         WRITE (NOUTDG,400)MOD(YRDOY,100000),DAP,VSTAGE,RSTAGE,XLAI,
     &      NINT(WTLF*10.0),NINT(STMWT*GM2KG),NINT(FRTWT*GM2KG),
     &      NINT(RTWT*GM2KG),NINT(BASLFWT*10.0),NINT(CRWNWT*GM2KG),
     &      NINT(SKWT*GM2KG),HI,
     &      NINT(PODWT*GM2KG),NINT(PODNO),(1.0-SWFAC),(1.0-TURFAC),
     &      (1.0-NSTRES),PCNL,SHELPC,SLA,CANHT,CANWH,SATFAC,
     &      (RTDEP/100),(RLV(I),I=1,10)
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

 300  FORMAT (2(1X,I5),1X,F5.1,1X,I5,1X,F5.2,7(1X,I5),1X,F5.3)
 400  FORMAT (2(1X,I5),1X,F5.1,1X,I5,1X,F5.2,7(1X,I5),
     &        1X,F5.3,2(1X,I5),3(1X,F5.3),2(1X,F5.2),1X,F5.1,
     &        2(1X,F5.2),1X,F5.3,11(1X,F5.2))

      END

C=======================================================================
C  OPWBAL, Subroutine
C
C  Generates water balance output file
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P. Wilkens  2-8-93
C  3. Added switch block, etc.                      P. Wilkens  2-8-93
C-----------------------------------------------------------------------
C  INPUT  : NOUTDW,YRDOY,DAP,DAYL,CRAIN,TOTIR,TRUNOF,TDRAIN,CES,CEP,CET,
C           PESW,SW,ST,IDETL,AVES,AVEP,AVET,AVEO,AVTMX,AVTMN,AVSRAD,NAP
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPDAY
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

       SUBROUTINE OPWBAL (NOUTDW,YRDOY,DAP,DAYL,CRAIN,TOTIR,TRUNOF,
     &     TDRAIN,CES,CEP,CET,PESW,SW,ST,IDETL,AVES,AVEP,AVET,AVEO,
     &     AVTMX,AVTMN,AVSRAD,NAP,AVWTD)

      IMPLICIT    NONE

      CHARACTER*1 IDETL

      INTEGER     DAP,NOUTDW,YRDOY,I,NAP
      REAL        DAYL,CRAIN,TOTIR,TRUNOF,TDRAIN
      REAL        CES,CEP,CET,PESW,SW(20),ST(20)
      REAL        AVES,AVEP,AVET,AVEO,AVTMX,AVTMN,AVSRAD,AVWTD

      IF (IDETL .EQ. 'N') THEN
         WRITE (NOUTDW,200)MOD(YRDOY,100000),DAP,AVEP,AVET,AVEO,PESW*10,
     &          TRUNOF,TDRAIN,NINT(CRAIN),NINT(TOTIR),AVSRAD,AVTMX,AVTMN
       ELSE IF (IDETL .EQ. 'Y') THEN
         WRITE (NOUTDW,300)MOD(YRDOY,100000),DAP,AVEP,AVET,AVEO,PESW*10,
     &          TRUNOF,NINT(TDRAIN),NINT(CRAIN),NINT(TOTIR),AVSRAD,
     &          AVTMX,AVTMN,DAYL,AVES,CEP,CES,CET,NAP,NINT(AVWTD),
     &          (SW(I),I=1,10),(ST(I),I=1,10)
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

 200  FORMAT(1X,I5,1X,I5,3(1X,F5.2),3(1X,F5.1),2(1X,I5),
     &    3(1X,F5.1))
 300  FORMAT(1X,I5,1X,I5,3(1X,F5.2),2(1X,F5.1),3(1X,I5),
     &    3(1X,F5.1),2(1X,F5.2),3(1X,F5.1),2(1X,I5),
     &    10(1X,F5.3),10(1X,F5.1))

      END

C=======================================================================
C  OPNIT, Subroutine
C
C  Generates nitrogen aspects output file
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P. Wilkens  2-8-93
C  3. Added switch block, etc.                      P. Wilkens  2-8-93
C-----------------------------------------------------------------------
C  INPUT  : NOUTDN,YRDOY,DAP,WTNCAN,WTNLF,WTNST,WTNSD,WTNSH,WTNUP,TLCH,
C           TSIN,WTNFX,NO3,NH4,WTLF,STMWT,IDETL,SDWT,TSON,NFIXN,ANO3,ANH4,
C           AMTNIT,PCNL,PCNST,PCNRT,PCNSH,PLTPOP
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPDAY
C
C  Calls  :
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPNIT (NOUTDN,YRDOY,DAP,WTNCAN,WTNLF,WTNST,
     &       WTNSD,WTNSH,WTNUP,TLCH,TSIN,WTNFX,
     &       NO3,NH4,WTLF,STMWT,IDETL,SDWT,TSON,NFIXN,
     &       ANO3,ANH4,AMTNIT,PCNL,PCNST,PCNRT,PCNSH,PLTPOP)

      IMPLICIT  NONE

      CHARACTER IDETL*1

      INTEGER   NOUTDN,YRDOY,DAP,I
      REAL      WTNCAN,WTNLF,WTNST,WTNSD,WTNSH,WTNUP,WTNFX
      REAL      TLCH,NO3(20),NH4(20)
      REAL      WTLF,STMWT,TSIN,TSON,ANO3,ANH4
      REAL      WTNVEG,WTNGRN,PCNVEG,PCNGRN,SDWT,NFIXN,AMTNIT
      REAL      PCNL,PCNST,PCNRT,PCNSH,PLTPOP

      WTNVEG  = (WTNLF + WTNST)
      WTNGRN  = (WTNSH + WTNSD)
      IF ((WTLF+STMWT) .GT. 0.0) THEN
         PCNVEG = (WTNLF+WTNST)/(WTLF+(STMWT*PLTPOP))*100.0
       ELSE
         PCNVEG = 0.0
      ENDIF
      IF (SDWT .GT. 0.0) THEN
         PCNGRN = WTNSD/SDWT*100
       ELSE
         PCNGRN = 0.0
      ENDIF

      IF (IDETL .EQ. 'N') THEN
        WRITE (NOUTDN,200) MOD(YRDOY,100000),DAP,(WTNCAN*10.0),
     &        (WTNSD*10.0),(WTNVEG*10.0),PCNGRN,PCNVEG,NINT(AMTNIT),
     &        (WTNFX*10.0),(WTNUP*10.0),TLCH,TSIN,NINT(TSON)

       ELSE IF (IDETL .EQ. 'Y') THEN
        WRITE (NOUTDN,300) MOD(YRDOY,100000),DAP,(WTNCAN*10.0),
     &        (WTNSD*10.0),(WTNVEG*10.0),PCNGRN,PCNVEG,NINT(AMTNIT),
     &        (WTNFX*10.0),(WTNUP*10.0),TLCH,TSIN,NINT(TSON),
     &        (WTNLF*10.0),(WTNST*10.0),PCNL,PCNST,PCNSH,PCNRT,NFIXN*10,
     &         ANO3,ANH4,(NO3(I),I=1,10),(NH4(I),I=1,10)
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

 200  FORMAT (2(1X,I5),3(1X,F5.1),2(1X,F5.2),1X,I5,4(1X,F5.1),1X,I5)
 300  FORMAT (2(1X,I5),3(1X,F5.1),2(1X,F5.2),1X,I5,4(1X,F5.1),1X,I5,
     &        2(1X,F5.1),4(1X,F5.2),23(1X,F5.1))

      END

C=======================================================================
C  OPPHO, Subroutine
C
C  Generates phosphorous aspects output file
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P. Wilkens  2-8-93
C-----------------------------------------------------------------------
C  INPUT  : NOUTDP,YRDOY,DAP,WTPCAN,WTPLF,WTPST,WTPSD,WTPSH,WTPUP,WTLF,
C           STMWT,IDETL,SDWT,PCPL,PCPST,PCPRT,PLTPOP
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPDAY
C
C  Calls  :
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPPHO (NOUTDP,YRDOY,DAP,WTPCAN,WTPLF,WTPST,
     &         WTPSD,WTPSH,WTPUP,WTLF,STMWT,IDETL,SDWT,
     &         PLTPOP,AMTPHO,APIRE,APIAC,APIST)

      IMPLICIT  NONE

      CHARACTER IDETL*1

      INTEGER   NOUTDP,YRDOY,DAP

      REAL      WTPCAN,WTPLF,WTPST,WTPSD,WTPSH,WTPUP
      REAL      WTLF,STMWT,AMTPHO,APIRE,APIAC,APIST
      REAL      WTPVEG,WTPGRN,PCPVEG,PCPGRN,SDWT,PLTPOP

      WTPVEG  = (WTPLF + WTPST)
      WTPGRN  = (WTPSH + WTPSD)
      IF ((WTLF+STMWT) .GT. 0.0) THEN
         PCPVEG = (WTPLF+WTPST)/(WTLF+(STMWT * PLTPOP))*100
       ELSE
         PCPVEG = 0.0
      ENDIF
      IF (SDWT .GT. 0.0) THEN
         PCPGRN = WTPSD/SDWT*100
       ELSE
         PCPGRN = 0.0
      ENDIF

      IF (IDETL .EQ. 'N') THEN
          WRITE (NOUTDP,200) MOD(YRDOY,100000),DAP,(WTPCAN*10.0)
      ELSE IF (IDETL .EQ. 'Y') THEN
          WRITE (NOUTDP,300) MOD(YRDOY,100000),DAP,(WTPCAN*10.0),
     &          (WTPSD*10.0),(WTPVEG*10.0),PCPGRN,PCPVEG,NINT(AMTPHO),
     &          (WTPUP*10.0),APIRE,APIAC,APIST
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

 200  FORMAT (2(1X,I5),3(1X,F5.1),2(1X,F5.2),1X,I5,4(1X,F5.1))
 300  FORMAT (2(1X,I5),3(1X,F5.1),2(1X,F5.2),1X,I5,4(1X,F5.1))

      END

C=======================================================================
C  OPCHEM, Subroutine
C
C  Generates chemical output file
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P. Wilkens  2-8-93
C-----------------------------------------------------------------------
C  INPUT  : NOUTDP,YRDOY,DAP,WTPCAN,WTPLF,WTPST,WTPSD,WTPSH,WTPUP,WTLF,
C           STMWT,IDETL,SDWT,PCPL,PCPST,PCPRT,PLTPOP
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPDAY
C
C  Calls  :
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPCHEM (NOUTDX,DAP,YRDOY,NPSTAP,H2OLOS,SEDLOS,
     &                   DRGLOS,TRNLOS,PST,IDETL)

      IMPLICIT NONE

      CHARACTER*1  IDETL

      INTEGER      DAP,NOUTDX,YRDOY,I,K,NPSTAP
      CHARACTER*80 FMT
      REAL         H2OLOS(*),SEDLOS(*),DRGLOS(*),TRNLOS(*),PST(10,*)

      IF (IDETL .EQ. 'N') THEN
         WRITE (FMT,100)'(2(1X,I5),',NPSTAP,'(3(1X,I5),1X,F5.1))'
         WRITE (NOUTDX,FMT) MOD(YRDOY,100000),DAP,
     &         (NINT(H2OLOS(I)*1000),NINT(SEDLOS(I)*1000),
     &          NINT(DRGLOS(I)*1000),TRNLOS(I),I=1,NPSTAP)
       ELSE
         WRITE (FMT,200)'(2(1X,I5),',NPSTAP,'(3(1X,I5),1X,F5.1),',
     &          NPSTAP*10,'(1X,I5))'

         WRITE (NOUTDX,FMT) MOD(YRDOY,100000),DAP,
     &         (NINT(H2OLOS(I)*1000),NINT(SEDLOS(I)*1000),
     &          NINT(DRGLOS(I)*1000),TRNLOS(I),I=1,NPSTAP),
     &        ((NINT(1000*PST(I,K)),K=1,10),I=1,NPSTAP)
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

 100  FORMAT (A10,I2,A20)
 200  FORMAT (A10,I2,A19,I3,A8)

      END

C=======================================================================
C  OPCARB, Subroutine
C
C  Generates carbon balance output file
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P. Wilkens  2-8-93
C-----------------------------------------------------------------------
C  INPUT  : NOUTDC,DAP,YRDOY,PCINPD,PG,GROWTH,MAINR,GRWRES,CADLF,CADST,
C           CMINEA,TSOC,AMTRES,TOTWT,RHOL,RHOS,IDETL,PGN,PCINPN
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPDAY
C
C  Calls  :
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

       SUBROUTINE OPCARB (NOUTDC,DAP,YRDOY,PCINPD,PG,GROWTH,MAINR,
     &      GRWRES,CADLF,CADST,CMINEA,TSOC,AMTRES,TOTWT,RHOL,RHOS,
     &      IDETL,PGN,PCINPN,SLWSLN,SLWSHN,PNLSLN,PNLSHN,LMXSLN,LMXSHN,
     &      DSNC,NHUM,HUM,TIFOM,TIFON)

      IMPLICIT    NONE

      CHARACTER IDETL*1

      INTEGER   NOUTDC,YRDOY,DAP,I

      REAL      PCINPD,PG,MAINR,GROWTH,GRWRES,CADLF,CADST
      REAL      CMINEA,TSOC,AMTRES,TOTWT,RHOL,RHOS,PGN,PCINPN
      REAL      SLWSLN,SLWSHN,PNLSLN,PNLSHN,LMXSLN,LMXSHN
      REAL      SNDD,SCDD,DSNC,INTERPOLATE,NHUM(20),HUM(20),TIFOM,TIFON

      SNDD = INTERPOLATE (NHUM,DSNC)
      SCDD = INTERPOLATE ( HUM,DSNC)

      IF (IDETL .EQ. 'N') THEN
         WRITE (NOUTDC,200) MOD(YRDOY,100000),DAP,NINT(TSOC/1000),
     &       NINT(TOTWT*10.0),PCINPD,PG,CMINEA,GROWTH,
     &       GRWRES,MAINR,(CADLF+CADST),RHOL*100,RHOS*100
       ELSE IF (IDETL .EQ. 'Y') THEN
         WRITE (NOUTDC,300) MOD(YRDOY,100000),DAP,NINT(TSOC/1000),
     &       NINT(TOTWT*10.0),PCINPD,PG,CMINEA,GROWTH,
     &       GRWRES,MAINR,(CADLF+CADST),RHOL*100,RHOS*100,
     &       NINT(AMTRES),PGN,PCINPN,SLWSLN,SLWSHN,PNLSLN,
     &       PNLSHN,LMXSLN,LMXSHN,NINT(TIFOM),(NINT(HUM(I)),I=1,5),
     &       NINT(TIFON),(NINT(NHUM(I)),I=1,5),NINT(SNDD),NINT(SCDD)
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

 200  FORMAT (4(1X,I5),7(1X,F5.2),2(1X,F5.2))
 300  FORMAT (4(1X,I5),7(1X,F5.2),2(1X,F5.1),
     &        1X,I5,2(1X,F5.2),6(1X,F5.2),14(1X,I5))

      END

C=======================================================================
C  INTERPOLATE, Subroutine
C
C  Interpolates and coagulates across/between layers
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P. Wilkens  2-8-93
C-----------------------------------------------------------------------
C  INPUT  : DAP,YRDOY,PCINPD,PG,GROWTH,MAINR,GRWRES,CADLF,CADST,
C           CMINEA,TSOC,AMTRES,TOTWT,RHOL,RHOS,IDETL,PGN,PCINPN
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPDAY
C
C  Calls  :
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      REAL FUNCTION INTERPOLATE (ARRVAR, DEPTH)

      IMPLICIT  NONE

      REAL      ARRVAR(20),X(10),DEPTH,TOTALVAL,TOTDEPTH,DIFF
      INTEGER   I

      DATA X   /5.,10.,15.,15.,15.,30.,30.,30.,30.,30./

      IF (DEPTH .LE. X(1)) THEN
         !
         ! Depth is <= to 5 cm
         !
         INTERPOLATE = ARRVAR(1)
         RETURN
      ENDIF

      TOTDEPTH = 0.0
      TOTALVAL = 0.0

      DO I = 1, 10
         IF (TOTDEPTH + X(I) .LE. DEPTH) THEN
           !
           ! We have not yet reached the queried depth
           !
           TOTDEPTH = TOTDEPTH + X(I)
           TOTALVAL = TOTALVAL + ARRVAR(I)*X(I)
           IF (TOTDEPTH .EQ. DEPTH) THEN
              INTERPOLATE = TOTALVAL / TOTDEPTH
              EXIT
           ENDIF
         ELSE
           !
           ! Ended up in the middle of a layer .. mix it
           !
           DIFF     = DEPTH - TOTDEPTH
           TOTDEPTH = DEPTH
           TOTALVAL = TOTALVAL + ARRVAR(I)*DIFF
           INTERPOLATE = TOTALVAL / TOTDEPTH
           EXIT
         ENDIF
      END DO

      RETURN
      END

C=======================================================================
C  OPPEST, Subroutine
C
C  Writes pest and disease data to a file
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       W.D.B       1-29-92
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       2-28-93
C-----------------------------------------------------------------------
C  INPUT  : NOUTDD,YRDOY,DAP,IDETL,DISLA,DISLAP,WLIDOT,WSIDOT,WRIDOT,SWIDOT,
C           WSHIDT,SDIDOT,SHIDOT,PPLTD,CPPLTD,CLFM,CLAI,CSTEM,LAIDT,
C           CSDM,CSDN,CSHM,CSHN,RLVDOT,CRLV,RLFDOT,CRLF,CRTM,ASMDOT,CASM
C
C  LOCAL  : None
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : OPDAY
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

       SUBROUTINE OPPEST (NOUTDD,YRDOY,DAP,IDETL,DISLA,DISLAP,WLIDOT,
     &        WSIDOT,WRIDOT,SWIDOT,WSHIDT,SDIDOT,SHIDOT,PPLTD,CPPLTD,
     &        CLFM,CLAI,CSTEM,LAIDT,CSDM,CSDN,CSHM,CSHN,RLVDOT,CRLV,
     &        RLFDOT,CRLF,CRTM,ASMDOT,CASM)

      IMPLICIT  NONE

      CHARACTER IDETL*1

      INTEGER   NOUTDD,YRDOY,DAP
      REAL      DISLA,DISLAP,WLIDOT,WSIDOT,WRIDOT,SWIDOT,WSHIDT,SDIDOT
      REAL      SHIDOT,RLVDOT,CRLV,RLFDOT,CRLF,CRTM,ASMDOT,CASM
      REAL      PPLTD,CPPLTD,CLFM,CLAI,CSTEM,LAIDT,CSDM,CSDN,CSHM,CSHN

      IF (IDETL .EQ. 'N') THEN
         WRITE (NOUTDD,200) MOD(YRDOY,100000),DAP,DISLA,DISLAP,
     &          LAIDT,WLIDOT,WSIDOT,SWIDOT,SDIDOT,WSHIDT,SHIDOT,WRIDOT,
     &          RLVDOT,RLFDOT,ASMDOT,PPLTD
       ELSE IF (IDETL .EQ. 'Y') THEN
         WRITE (NOUTDD,300) MOD(YRDOY,100000),DAP,DISLA,DISLAP,
     &          LAIDT,WLIDOT,WSIDOT,SWIDOT,SDIDOT,WSHIDT,SHIDOT,WRIDOT,
     &          RLVDOT,RLFDOT,ASMDOT,PPLTD,CLAI,CLFM,CSTEM,CSDM,CSDN,
     &          CSHM,CSHN,CRTM,CRLV,CRLF,CASM,CPPLTD
      ENDIF
      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 200  FORMAT (1X,I5,1X,I5,1X,F5.0,1X,F5.1,12(1X,F5.2))
 300  FORMAT (1X,I5,1X,I5,1X,F5.0,1X,F5.1,12(1X,F5.2),12(1X,F5.1))

      END

C=======================================================================
C  OPVIEW, Subroutine
C
C  Generates stage specific summary outputs to screen and NOUTDO file
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      2-8-93
C-----------------------------------------------------------------------
C  INPUT  : NREP,TITLET,IDETV,NOUTDO,IDETO,NYRS,DAP,BIOMAS,LAI,XN,CET,CRAIN,
C           TOTIR,DOY,PESW,TURFAC,SWFAC,NSTRES,CWSV,CWSR,STGDOY,YRDOY,
C           YRSIM,XANC,STOVN,GRAINN,PLTPOP,LUNOV
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPDAY
C
C  Calls  : YR_DOY NAILUJ
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPVIEW (NREP,TITLET,RNMODE,NOUTDO,IDETO,NYRS,DAP,
     &    BIOMAS,LAI,XN,CET,CRAIN,TOTIR,DOY,PESW,TURFAC,SWFAC,NSTRES,
     &    CWSV,CWSR,STGDOY,YRDOY,YRSIM,XANC,STOVN,GRAINN,PLTPOP,
     &    LUNOV,STNAME)

      IMPLICIT     NONE

      CHARACTER*1  IDETO,RNMODE
      CHARACTER*3  RMM
      CHARACTER*25 TITLET
      CHARACTER*10 STNAME(20)

      INTEGER      I,NREP,YR,DAP,NOUTDO,IPX,DOY,NYRS
      INTEGER      STGDOY(20),YRDOY,YRSIM,LUNOV

      REAL         BIOMAS,LAI,XN,CET,CRAIN,TOTIR,PESW,TURFAC
      REAL         SWFAC,NSTRES,CWSV,CWSR,DSTRES,STRESS,STRESP,STRESN
      REAL         XANC,AVDSTR,AVNSTR,STOVN,GRAINN,PLTPOP,WTNCAN

C-----------------------------------------------------------------------
C     Initialize counters, averages
C-----------------------------------------------------------------------

      IF (YRDOY .EQ. YRSIM) THEN
         DSTRES = 0.0
         STRESP = 0.0
         STRESS = 0.0
         STRESN = 0.0
         CWSV   = 0.0
         CWSR   = 0.0
      ENDIF

      DSTRES = DSTRES + 1.0
      STRESS = STRESS + TURFAC
      STRESP = STRESP + SWFAC
      STRESN = STRESN + NSTRES

      IF (DSTRES .GE. 1.0) THEN
         AVDSTR = 1.0 - (STRESS/DSTRES)
         AVNSTR = 1.0 - (STRESN/DSTRES)
      ENDIF

C-----------------------------------------------------------------------
C     Write to OUTO output file at defined phenological stages
C-----------------------------------------------------------------------

      IF (RNMODE .EQ. 'I' .AND. YRDOY .EQ. YRSIM .AND. NYRS .LE. 1) THEN
         CALL CLEAR
         WRITE (LUNOV,3050) NREP,TITLET              ! Write header to screen
         WRITE (LUNOV,3200)
      ENDIF

      IF (IDETO .EQ. 'Y' .AND. YRDOY .EQ. YRSIM) THEN
         WRITE (NOUTDO,3050) NREP,TITLET
         WRITE (NOUTDO,3200)
      ENDIF

      DO I = 1, 20
         IF (YRDOY .EQ. STGDOY(I)) THEN
            DSTRES  = 0.0
            STRESS  = 0.0
            STRESP  = 0.0
            STRESN  = 0.0

C-----------------------------------------------------------------------
C           DAP is days after planting, DAP = 0 on day of planting
C-----------------------------------------------------------------------

            WTNCAN = (STOVN+GRAINN)*PLTPOP*10.0
            IF (BIOMAS .GT. 0.0) THEN
               XANC = (WTNCAN/(BIOMAS*10.0))*100.0
             ELSE
               XANC = 0.0
            ENDIF
            IF (YRDOY .EQ. STGDOY(8)) THEN
               XANC = 0.0
            ENDIF
            CALL YR_DOY (YRDOY,YR,DOY)
            CALL NAILUJ (DOY,YR,RMM,IPX)

           IF (RNMODE .EQ. 'I' .AND. NYRS .LE. 1) THEN
              WRITE (LUNOV,4600) IPX,RMM,DAP,STNAME(I),
     &        NINT(BIOMAS*10.0),LAI,XN,NINT(CET),NINT(CRAIN),
     &        NINT(TOTIR),NINT(PESW*10.0),NINT(WTNCAN),
     &        XANC,AVDSTR,AVNSTR
            ENDIF

            IF (IDETO .EQ. 'Y') THEN
               WRITE (NOUTDO,4600) IPX,RMM,DAP,STNAME(I),
     &         NINT(BIOMAS*10.0),LAI,XN,NINT(CET),NINT(CRAIN),
     &         NINT(TOTIR),NINT(PESW*10.0),NINT(WTNCAN),
     &         XANC,AVDSTR,AVNSTR
            ENDIF
         ENDIF
      END DO

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 3050 FORMAT (//,'*SIMULATED CROP AND SOIL STATUS AT MAIN',
     &        ' DEVELOPMENT STAGES',//,' RUN NO. ',I2,4X,A25,/)
 3200 FORMAT (3X,'DATE',1X,'CROP',1X,'GROWTH',3X,
     &       'BIOMASS',2X,'LAI ',1X,'LEAF',3X,'ET',2X,'RAIN',1X,'IRRIG',
     &       1X,'SWATER',1X,'CROP  N',2X,'STRESS',/,8X,'AGE',2X,'STAGE',
     &       5X,'kg/ha',9X,'NUM.',2X,'mm',3X,'mm',4X,'mm',4X,'mm',3X,
     &       'kg/ha %',1X,' H2O  N',/,80('-'))
 4600 FORMAT (1X,I2,1X,A3,2X,I3,1X,A10,I6,1X,F5.2,1X,F4.1,I5,
     &        3I6,I5,F4.1,2F4.2)

      END
