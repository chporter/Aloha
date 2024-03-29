C=======================================================================
C  SENS, Subroutine
C
C  Determines sensitivity analysis
C-----------------------------------------------------------------------
C  Revision history
C
C  01/09/89 GH  Written
C  05/28/93 PWW Header revision and minor changes
C  04/02/96 GH  Minor changes
C  02/02/98 GH  Rename SENSDM to SEPEST
C-----------------------------------------------------------------------
C  INPUT  : NSENS,VARNO,VARTY,VRNAME,FTYPEN,LNIC,LNSA,WRESR,WRESND,ISIM,
C           NYRS,IPLT,WMODI,ECONO,ECONAM,ECOTYP,PRCROP,SWINIT,INO3,INH4,
C           NREP,FROP,YRIC,EFINOC,EFNFIX,CROP,IVRGRP
C
C  LOCAL  : LINE,ERRKEY,MENU,NLOOP,FLAG,EFF,DSOIL,THETAC,AIRAM,TOTAPW
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : INPUT
C
C  Calls  : ERROR CLEAR YR_DOY NAILUJ SETIME SECROP IPVAR IPECO SEVAR
C           SEWTH DATEC SECLI SESOIL SEINIT SEPLT SEHAR SEFERT SEIRR
C           SERES SESIM
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

       SUBROUTINE SENS (NSENS,VARNO,VARTY,VRNAME,FTYPEN,LNIC,LNSA,
     &   WRESR,WRESND,ISIM,NYRS,IPLT,WMODI,ECONO,ECONAM,ECOTYP,
     &   PRCROP,SWINIT,INO3,INH4,NREP,FROP,YRIC,EFINOC,EFNFIX,
     &   CROP,IVRGRP,ISENS)

      IMPLICIT NONE

      INCLUDE 'COMIBS.BLK'
      INCLUDE 'COMSOI.BLK'
      INCLUDE 'COMSWI.BLK'

      CHARACTER*1  ANS,WMODI,SWSPRF
      CHARACTER*2  CROPC,CROP,PRCROP
      CHARACTER*3  MSSIM,MSPLT
      CHARACTER*6  VARTY,VARNO,ERRKEY,ECOTYP,ECONO
      CHARACTER*7  FILEWT
      CHARACTER*12 FILEWP
      CHARACTER*15 NFMANT,INMANT,WTMANT
      CHARACTER*16 VRNAME,ECONAM
      CHARACTER*25 HARMAN,NIMANT,REMANT
      CHARACTER*30 IRMANT
      CHARACTER*40 PSMANT

      INTEGER MENU,FROP,NSENS,NLOOP
      INTEGER LNIC,LNSA,YRIC,ISIM,NYRS
      INTEGER IPLT,IPYRS,DSSIM,IPYRP,DSPLT
      INTEGER NREP,IVRGRP,FTYPEN,ISENS,YRPLTX

      REAL    SWINIT(20),WRESR,WRESND,INO3(20),INH4(20),EFNFIX,EFINOC

      PARAMETER (ERRKEY = 'SENS  ')
      FILEWT = 'WTH.LST'

      MENU   = 0

      IF (NREP .EQ. 1 .AND. RNMODE .EQ. 'I') THEN
         CALL CLEAR
         WRITE (*,100)
      ENDIF

      IF (NREP .EQ. 1) READ (5,'(A1)') ANS
      NLOOP = 0

  400 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 50) CALL ERROR (ERRKEY,1,' ',0)

      IF (MEWTH .EQ. 'M') THEN
         WTMANT = 'OBSERVED   DATA'
       ELSEIF (MEWTH .EQ. 'G') THEN
         WTMANT = 'EXT. SIMUL.    '
       ELSEIF (MEWTH .EQ. 'S') THEN
         WTMANT = 'INT. GENER.    '
      ENDIF

      IF (ISWDIS .EQ. 'Y') THEN
         PSMANT = 'PEST & DISEASE INTERACTION SIMULATED    '
       ELSE
         PSMANT = 'PEST & DISEASE INTERACTION NOT SIMULATED'
      ENDIF

      IF (MESIC .EQ. 'M') THEN
         INMANT = 'AS REPORTED    '
       ELSEIF (MESIC .EQ. 'S') THEN
         INMANT = 'PREVIOUS RUN   '
      ENDIF

      IF (IHARI .EQ. 'A') THEN
         HARMAN = 'AUTOMATIC                '
       ELSEIF (IHARI .EQ. 'G') THEN
         HARMAN = 'AT REPORTED GROWTH STAGES'
       ELSEIF (IHARI .EQ. 'M') THEN
         HARMAN = 'AT HARVEST MATURITY      '
       ELSEIF (IHARI .EQ. 'R') THEN
         HARMAN = 'ON REPORTED DATE(S)      '
       ELSEIF (IHARI .EQ. 'D') THEN
         HARMAN = 'ON REPORTED DAP          '
      ENDIF

      IF (ISWWAT .EQ. 'N') THEN
         IRMANT = 'NO WATER BALANCE SIMULATION   '
       ELSEIF (IIRRI .EQ. 'A') THEN
         IRMANT = 'AUTOMATIC IRRIGAT-REFILL PROF '
       ELSEIF (IIRRI .EQ. 'F') THEN
         IRMANT = 'AUTOMATIC IRRIGAT-FIXED AMOUNT'
       ELSEIF (IIRRI .EQ. 'N') THEN
         IRMANT = 'RAINFED                       '
       ELSEIF (IIRRI .EQ. 'P') THEN
         IRMANT = 'FIELD SCHEDULE & AUT REFILL PR'
       ELSEIF (IIRRI .EQ. 'R') THEN
         IRMANT = 'ON REPORTED DATE(S)           '
       ELSEIF (IIRRI .EQ. 'D') THEN
         IRMANT = 'ON REPORTED DAP               '
       ELSE IF (IIRRI .EQ. 'W') THEN
         IRMANT = 'FIELD SCHEDULE & AUT. FIXED AM'
       ELSE
         IRMANT = '                              '
      ENDIF

      IF (ISWNIT .NE. 'Y') THEN
         NIMANT = 'NO N-BALANCE SIMULATION  '
       ELSEIF (IFERI .EQ. 'A') THEN
         NIMANT = 'AUTOMATIC N-FERTILIZER AP'
       ELSEIF (IFERI .EQ. 'F') THEN
         NIMANT = 'AUTOMATIC N-FERTILIZER AP'
       ELSEIF (IFERI .EQ. 'N') THEN
         NIMANT = 'NO N-FERTILIZER APPLICAT.'
       ELSEIF (IFERI .EQ. 'R') THEN
         NIMANT = 'ON REPORTED DATE(S)      '
       ELSEIF (IFERI .EQ. 'D') THEN
         NIMANT = 'ON REPORTED DAP          '
       ELSE
         NIMANT = '                         '
      ENDIF

      IF (ISWNIT .EQ. 'Y') THEN
         IF (ISWSYM .EQ. 'Y') THEN
            NFMANT = 'N-FIX SIMULAT. '
          ELSEIF (ISWSYM .EQ. 'N') THEN
            NFMANT = 'NO N-FIX SIMUL.'
          ELSEIF (ISWSYM .EQ. 'U') THEN
            NFMANT = 'UNLIMITED N-FIX'
          ELSE
            NFMANT = 'N-FIX NON LIMIT'
         ENDIF
      ENDIF

      IF (ISWNIT .NE. 'Y') THEN
         REMANT = 'NO N-BALANCE SIMULATION  '
       ELSE IF (IRESI .EQ. 'F') THEN
         REMANT = 'AUTOMATIC RESIDUE APPLIC.'
       ELSE IF (IRESI .EQ. 'A') THEN
         REMANT = 'AUTOMATIC RESIDUE APPLIC.'
       ELSE IF (IRESI .EQ. 'N') THEN
         REMANT = 'NO RESIDUE APPLICATION   '
       ELSE IF (IRESI .EQ. 'R') THEN
         REMANT = 'ON REPORTED DATE(S)      '
       ELSEIF (IRESI .EQ. 'D') THEN
         REMANT = 'ON REPORTED DAP          '
       ELSE
         REMANT = '                         '
      ENDIF

      CALL YR_DOY (YRSIM,IPYRS,ISIM)
      CALL NAILUJ (ISIM,IPYRS,MSSIM,DSSIM)
      CALL YR_DOY (YRPLT,IPYRP,IPLT)
      CALL NAILUJ (IPLT,IPYRP,MSPLT,DSPLT)

      IF (RNMODE .EQ. 'I') THEN
         CALL CLEAR
         IF (ISWWAT .EQ. 'Y') THEN
         WRITE(*,500) MSSIM,DSSIM,(1900+IPYRS),
     &     CROPD,FILEC,FILEG,
     &     VRNAME,IVRGRP,ECONAM,
     &     WSTA,WTMANT,WMODI,
     &     PEDON,SLTX,
     &     INMANT,
     &     MSPLT,DSPLT,(1900+IPYRP),ROWSPC,PLTPOP,
     &     HARMAN,
     &     IRMANT,
     &     NIMANT,NFMANT,
     &     REMANT,
     &     PSMANT,
     &     MEHYD,ISWNIT,ISWSYM,ISWDIS,MEPHO,MEWTH,MEEVP
         ELSE
         WRITE(*,500) MSSIM,DSSIM,(1900+IPYRS),
     &     CROPD,FILEC,FILEG,
     &     VRNAME,IVRGRP,ECONAM,
     &     WSTA,WTMANT,WMODI,
     &     PEDON,SLTX,
     &     INMANT,
     &     MSPLT,DSPLT,(1900+IPYRP),ROWSPC,PLTPOP,
     &     HARMAN,
     &     IRMANT,
     &     NIMANT,NFMANT,
     &     REMANT,
     &     PSMANT,
     &     ISWWAT,ISWNIT,ISWSYM,ISWDIS,MEPHO,MEWTH,MEEVP
         ENDIF
         IF (NREP .EQ. 1) THEN
            WRITE (*,550) FROP,IDETO,IDETS,IDETG,IDETW,IDETN,IDETD
         ENDIF
         WRITE (*,575)
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 400) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 0) THEN
          RETURN
      ELSE IF (MENU .EQ. 1) THEN
          CALL SETIME (ISIM,YEAR,NYRS,FILEW,RNMODE,MESIC,YRSIM,YRPLT,
     &         IHARI,HDATE,NHAR,PATHWT)
      ELSE IF (MENU .EQ. 2) THEN
          CROPC = CROP
          CALL SECROP (FILEC,FILEE,FILEG,RNMODE,CROP,CROPD,PATHCR)
          IF (CROP .NE. CROPC) THEN
             CALL IPVAR (FILEG,NSENS,RNMODE,VARNO,VARTY,VRNAME,
     &                   PATHGE,ECONO,CROP)
             IF (INDEX('SBPNBNTMPECHPPPRC3C4G0G1G2G3G4G5G6G7G8G9SCVB',
     &          CROP) .GT. 0) THEN
                NSENS =  0
                CALL IPECO (FILEE,NSENS,RNMODE,PATHEC,ECOTYP,ECONAM,
     &               ECONO,IVRGRP)
                NSENS =  1
             ENDIF
          ENDIF
      ELSE IF (MENU .EQ. 3) THEN
          CALL SEVAR (FILEE,FILEG,NSENS,RNMODE,VARNO,VARTY,VRNAME,
     &         IVRGRP,PATHGE,PATHEC,ECOTYP,ECONAM,ECONO,CROP)
      ELSE IF (MENU .EQ. 4) THEN
          IF (MEWTH .NE. 'S' .AND. MEWTH .NE. 'W') THEN
             FILEWP = FILEW
             CALL SEWTH (FILEW,RNMODE,WMODI,WTHADJ,PATHWT,MEWTH,YEAR,
     &            DSSATP,FILEWT,RSEED1)
             WSTA = FILEW(1:4)
             IF (MEWTH .NE. 'S' .AND. MEWTH .NE. 'W'
     &                          .AND. FILEW .NE. FILEWP) THEN
                CALL DATEC (FILEW,YRSIM,YRPLT,YEAR,PATHWT,
     &               IDLAPL,NIRR,RESDAY,NARES,FDAY,NFERT,
     &               HLATE,PWDINF,PWDINL,NHAR,HDATE,
     &               IIRRI,IFERI,IHARI,IRESI,ISWCHE,ISWTIL,
     &               CDATE,TDATE,YRIC,NTIL,NCHEM)
             ENDIF
           ELSEIF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W' ) THEN
             CALL SECLI (FILEW,RNMODE,WMODI,WTHADJ,PATHWT,MEWTH,YEAR,
     &            RSEED1,DSSATP,FILEWT)
          ENDIF
          IF (WMODI .EQ. 'Y' .AND. NEV .EQ. 0)  THEN
             NEV = 1
             WMDATE(1) = YRSIM
          ENDIF
      ELSE IF (MENU .EQ. 5) THEN
          CALL SESOIL (FILES,FILEX,NSENS,RNMODE,SLNF,SLPF,U,SWCON,CN2,
     &         SALB,DEPMAX,PEDON,SLNO,SLTX,LL,DUL,SAT,SHF,BD,OC,PH,
     &         DLAYR,NLAYR,DS,LNIC,LNSA,YRIC,PRCROP,WRESR,WRESND,
     &         EFINOC,EFNFIX,PATHSL,SWINIT,INO3,INH4,ISWNIT,ISWWAT,
     &         EXTP,ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID,SWCN,ADCOEF,TOTN)
      ELSE IF (MENU .EQ. 6) THEN
          SWSPRF = 'N'
          CALL SEINIT (RNMODE,NLAYR,SWINIT,PRCROP,WRESR,WRESND,DLAYR,
     &                SLPF,DS,SWSPRF,INO3,INH4,ICWD,ICRES,ICREN,
     &                ICREP,ICRIP,ICRID)
      ELSE IF (MENU .EQ. 7) THEN
          YRPLTX = YRPLT
          CALL SEPLT (RNMODE,PLTPOP,ROWSPC,AZIR,SDEPTH,SDWTPL,YRSIM,
     &         YRPLT,IPLT,IPLTI,YEAR,PWDINF,PWDINL,SWPLTL,SWPLTH,
     &         SWPLTD,PTX,PTTN,PLME,SDAGE,ATEMP)
          IF (YRPLTX .NE. YRPLT) THEN
             ISENS = 1
          ENDIF
      ELSE IF (MENU .EQ. 8) THEN
          CALL SEHARV (RNMODE,IHARI,HDLAY,HLATE,HPP,HRP,YEAR,
     &                 HSTG,HCOM,HSIZ,HDATE,HPC,CROP,NHAR,HBPC,
     &                 YRPLT)
      ELSE IF (MENU .EQ. 9) THEN
          CALL SEIRR (IDETW,RNMODE,ISWWAT,IIRRI,IRMANT,EFFIRR,ISWNIT,
     &         AIRAMT,NAPW,IDLAPL,AMT,TOTAPW,NIRR,IRRCOD,DSOIL,THETAC)
      ELSE IF (MENU .EQ. 10) THEN
          CALL SEFERT (IDETN,RNMODE,ISWNIT,ISWWAT,NIMANT,NFMANT,
     &         IFERI,ISWSYM,EFINOC,EFNFIX,DSOILN,SOILNC,SOILNX,FTYPEN,
     &         NFERT,IFTYPE,ANFER,DFERT,TOTNAP,FDAY,NSWITCH,CROP,NCODE,
     &         FERCOD)
      ELSE IF (MENU .EQ. 11) THEN
          WRITE(*,650)
          PAUSE
      ELSE IF (MENU .EQ. 12) THEN
          CALL SERES (RNMODE,CROP,RESIDUE,DEPRES,ISWWAT,ISWNIT,
     &                  RESN,RESP,RESDAY,NARES,RESAMT,RESCOD,RINP,RESK,
     &                  IRESI,NSWITCH,IDETN,REMANT)
      ELSE IF (MENU .EQ. 13) THEN
          CALL SEPEST (IDETD,RNMODE,ISWDIS)
      ELSE IF (MENU .EQ. 14) THEN
          CALL SEFLD (RNMODE,XCRD,YCRD,ELEV,SLOPE)
      ELSE IF (MENU .EQ. 15) THEN
          CALL SESIM (RNMODE,ISWNIT,ISWWAT,NFMANT,ISWSYM,ISWPHO,
     &         ISWDIS,MEWTH,MESIC,MEEVP,MEPHO,FILEW,YEAR,PATHWT,
     &         NSWITCH,CROP,MEHYD,MESOM)
      ELSE IF (MENU .EQ. 16) THEN
          IF (NREP .EQ. 1) THEN
             CALL SEFREQ (RNMODE,IDETO,IDETS,IDETG,IDETC,IDETW,IDETN,
     &            IDETP,IDETD,IOX,FROP,NREP,IDETL)
             IF (NREP .EQ. 1) THEN
                IF (IOX .EQ. 'Y') THEN
                   WRITE(OUTO(1:12),80) EXPER,'.',CG,'O'
                   WRITE(OUTG(1:12),80) EXPER,'.',CG,'G'
                   WRITE(OUTW(1:12),80) EXPER,'.',CG,'W'
                   WRITE(OUTN(1:12),80) EXPER,'.',CG,'N'
                   WRITE(OUTC(1:12),80) EXPER,'.',CG,'C'
                   WRITE(OUTD(1:12),80) EXPER,'.',CG,'D'
                   WRITE(OUTP(1:12),80) EXPER,'.',CG,'P'
                   WRITE(OUTF(1:12),80) EXPER,'.',CG,'F'
                   WRITE(OUTH(1:12),80) EXPER,'.',CG,'H'
                   WRITE(OUTR(1:12),80) EXPER,'.',CG,'R'
                 ELSE
                   OUTO  = 'OVERVIEW.OUT'
                   OUTG  = 'GROWTH.OUT'
                   OUTW  = 'WATER.OUT'
                   OUTN  = 'NITROGEN.OUT'
                   OUTC  = 'CARBON.OUT'
                   OUTD  = 'PEST.OUT'
                   OUTP  = 'PHOSPHOR.OUT'
                   OUTF  = 'FLOOD.OUT'
                   OUTH  = 'CHEMICAL.OUT'
                   OUTR  = 'OPERAT.OUT'
                ENDIF
             ENDIF
          ENDIF
      ENDIF

      GO TO 400

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

   80 FORMAT (A8,A1,A2,A1)
  100 FORMAT (//20X,'MANAGEMENT / SENSITIVITY ANALYSIS',
     &/20X,         '=================================',/,
     &/T10,'The options which follow relate to the initial',
     &/T10,'experiment and treatment you selected.  These ',
     &/T10,'default values allow you to validate the simulation',
     &/T10,'results.  To evaluate alternative management strategies'
     &/T10,'or make tactical or strategical decisions, you can',
     &/T10,'modify or change the default values.',/,
     &/T10,'If you choose not to change any of the default values,',
     &/T10,'press the ENTER key in response to the questions.',/,
     &/T10,'Please press < ENTER > to continue    ===> ',$)
  500 FORMAT(5X,'MANAGEMENT / SENSITIVITY ANALYSIS OPTIONS',/,
     &   5X,'=========================================',//,
     &   1X,' 0. RETURN TO THE MAIN MENU',//,
     &   1X,' 1. Simulation Timing .......',2X,A3,1X,I2,1X,I4,/,
     &   1X,' 2. Crop ....................',2X,A10,10X,A12,1X,A12,/,
     &   1X,' 3. Cultivar ................',2X,A16,4X,'MAT :',I2,
     &   5X,A16,/,
     &   1X,' 4. Weather .................',2X,A4,16X,A11,1X,
     &   'WMOD:',A1,/,
     &   1X,' 5. Soil ....................',2X,A10,10X,A5,/,
     &   1X,' 6. Initial Conditions ......',2X,A15,/,
     &   1X,' 7. Planting ................',2X,A3,1X,I2,1X,I4,
     &   9X,'ROW SP:',F4.0,1X,'PLANTS/m2:',F6.2,/,
     &   1X,' 8. Harvest .................',2X,A25,/,
     &   1X,' 9. Water and Irrigation ....',2X,A30,/
     &   1X,'10. Nitrogen ................',2X,A25,5X,A15,/,
     &   1X,'11. Phosphorus ..............',2X,'N/A',/,
     &   1X,'12. Residue .................',2X,A25,/,
     &   1X,'13. Pests and Diseases ......',2X,A40,/,
     &   1X,'14. Field ...................',2X,/,
     &   1X,'15. Crop Process Options ....',2X,'H20:',A1,
     &   1X,'NIT:',A1,1X,'N-FIX:',A1,1X,'PEST:',A1,
     &   1X,'PHOTO:',A1,1X,'WTH:',A1,1X,'ET:',A1)
  550 FORMAT (1X,'16. Output Control ..........',2X,'FREQ:',I2,
     &   ' OVV:',A1,' SUM:',A1,' GROWTH:',A1,' H20:',A1,
     &   ' NIT:',A1,' PEST:',A1)
  575 FORMAT (/,5X,'SELECTION ?  [Default = 0] ===> ',$)
  650 FORMAT (5X,'Option Currently Not Available')

      END

C=======================================================================
C  DATEC, Subroutine
C
C  This subroutine resets values when a new weather year is
C  selected interactively
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2. Header revision and minor changes           P.W.W.      2-7-93
C  2. Added switch common block, restructured     P.W.W.      2-7-93
C  3. Added additional variables                  G.H.       04-2-96
C-----------------------------------------------------------------------
C  INPUT  : FILEW,YRSIM,YRPLT,YEAR,PATHWT,IDLAPL,NIRR,RESDAY,NARES,FDAY,
C           NFERT,HLATE,PWDINF,PWDINL,NHAR,HDATE
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called :
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  PP3    :
C=======================================================================

      SUBROUTINE DATEC (FILEW,YRSIM,YRPLT,YEAR,PATHWT,
     &           IDLAPL,NIRR,RESDAY,NARES,FDAY,NFERT,
     &           HLATE,PWDINF,PWDINL,NHAR,HDATE,
     &           IIRRI,IFERI,IHARI,IRESI,ISWCHE,ISWTIL,
     &           CDATE,TDATE,YRIC,NTIL,NCHEM)

      IMPLICIT NONE

      CHARACTER*1  BLANK,IIRRI,IFERI,IHARI,IRESI,ISWTIL,ISWCHE
      CHARACTER*6  ERRKEY
      CHARACTER*12 FILEW
      CHARACTER*80 PATHWT,LINE
      CHARACTER*92 FILEWW

      INTEGER YRSIM,YRPLT,YRDIF,IPLT,ISIM,YR,YEAR,NIRR,NARES,NFERT
      INTEGER FDAY(25),IDLAPL(150),RESDAY(10),I,IDATE
      INTEGER LINWTH,LUNWTH,PATHL,ERRNUM,YEARN,FOUND
      INTEGER HLATE,PWDINF,PWDINL,NHAR,HDATE(3)
      INTEGER IDAYIC,YRIC
      INTEGER TDATE(10),CDATE(10),NTIL,NCHEM

      PARAMETER (ERRKEY = 'DATEC ')
      PARAMETER (LUNWTH = 11 )
      PARAMETER (BLANK = ' ')

      LINWTH = 1
      PATHL  = INDEX (PATHWT,BLANK)

      IF (PATHL .LE. 1) THEN
          FILEWW = FILEW
       ELSE
          FILEWW = PATHWT(1:(PATHL-1)) // FILEW
      ENDIF

      OPEN (LUNWTH,FILE=FILEWW,STATUS='OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEW,LINWTH)
C
C     Read in weather file header.
C
   20 CONTINUE
      CALL IGNORE (LUNWTH,LINWTH,FOUND,LINE)
      IF (FOUND .EQ. 2) GO TO 20
      IF (FOUND .EQ. 0) CALL ERROR (ERRKEY,-1,FILEW,LINWTH)
      CALL IGNORE (LUNWTH,LINWTH,FOUND,LINE)
      IF (FOUND .NE. 1) CALL ERROR (ERRKEY,-1,FILEW,LINWTH)
      READ (LINE,'(I2)',IOSTAT=ERRNUM) YEARN
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEW,LINWTH-1)
      CLOSE (LUNWTH)

      YRDIF = YEARN - YEAR
      CALL YR_DOY (YRSIM,YR,ISIM)
      YRSIM = (YR + YRDIF) * 1000 + ISIM
      CALL YR_DOY (YRPLT,YR,IPLT)
      YRPLT = (YR + YRDIF) * 1000 + IPLT
      CALL YR_DOY(YRIC,YR,IDAYIC)
      YRIC  = (YR + YRDIF) * 1000 + IDAYIC
      IF (NIRR .GT. 0 .AND. IIRRI .NE. 'D') THEN
         DO I = 1, NIRR
            IF (IDLAPL(I) .GT. 0) THEN
              CALL YR_DOY (IDLAPL(I),YR,IDATE)
               IDLAPL(I) = (YR + YRDIF) * 1000 + IDATE
            ENDIF
         END DO
      ENDIF
      IF (NFERT .GT. 0 .AND. IFERI .NE. 'D') THEN
         DO I = 1, NFERT
            CALL YR_DOY (FDAY(I),YR,IDATE)
            FDAY(I) = (YR + YRDIF) * 1000 + IDATE
         END DO
      ENDIF
      IF (NARES .GT. 0 .AND. IRESI .NE. 'D') THEN
         DO I = 1, NARES
            CALL YR_DOY (RESDAY(I),YR,IDATE)
            RESDAY(I) = (YR + YRDIF) * 1000 + IDATE
         END DO
      ENDIF
      IF (NHAR .GT. 0 .AND. IHARI .NE. 'D') THEN
         DO I = 1, NHAR
            CALL YR_DOY (HDATE(I),YR,IDATE)
            HDATE(I) = (YR + YRDIF) * 1000 + IDATE
         END DO
      ENDIF
      IF (NTIL .GT. 0 .AND. ISWTIL .NE. 'N') THEN
         DO I = 1, NTIL
            CALL YR_DOY (TDATE(I),YR,IDATE)
            TDATE(I) = (YR + YRDIF) * 1000 + IDATE
         END DO
      ENDIF
      IF (NCHEM .GT. 0 .AND. ISWCHE .NE. 'N') THEN
         DO I = 1, NCHEM
            CALL YR_DOY (CDATE(I),YR,IDATE)
            CDATE(I) = (YR + YRDIF) * 1000 + IDATE
         END DO
      ENDIF
      CALL YR_DOY (HLATE,YR,IDATE)
      HLATE  = (YR +  YRDIF) * 1000 + IDATE
      CALL YR_DOY (PWDINF,YR,IDATE)
      PWDINF = (YR +  YRDIF) * 1000 + IDATE
      CALL YR_DOY (PWDINL,YR,IDATE)
      PWDINL = (YR +  YRDIF) * 1000 + IDATE
      YEAR = YEARN

      RETURN
      END
