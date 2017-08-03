C=======================================================================
C  COPYRIGHT 1998 the University of Georgia, Griffin, Georgia
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  INPUT, Main program
C
C  INPUT MODULE FOR DSSAT MODELS,  DSSAT v3.5
C
C   June 15 1998                   Gerrit Hoogenboom
C
C
C  Reads FileX, includes sensitivity analysis and writes a
C  temporary output file for input by the crop models
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       G.H.        4-26-93
C  2. Modified by                                   G.H.        8-10-93
C  3. Modified by                                   P.W.W       8-25-93
C  4. Modified by                                   G.H.       10-07-93
C  5. Header revision and minor changes             P.W.W       5-28-93
C  5. Modified for soil P model                     W.T.B.      1-12-93
C  6. Set to 1994 version (MINPT940.EXE)            G.H.        6-22-94
C  6. Set to 1995 version (MINPT950.EXE)            G.H.        2-15-95
C  7. Modified to add OILCROP Sunflower model       G.H.        3-25-96
C  8. Add DSSAT v3.1 file structure for fileX       G.H         4-01-96
C  9. Modified to add chickpea and pigeonpea        G.H        12-31-96
C 10. Modified to add ALOHA Pineapple model         G.H        01-03-97
C 11. Modified to add pepper                        G.H        01-19-97
C 12. Modified to add cotton and CSCOT model        G.H        09-29-97
C 13. Changed to May 15, 1998, DSSAT v3.5           G.H        05-06-98
C 14. Modified to add velvetbean                    G.H        05-07-98
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : WMODI,WMODB,CROP,PRCROP,VARNO,VARTY,ERRKEY,ECOTYP,ECONO,
C           MODEL,FILEIO,ECONAM,VRNAME,TITLER,PATHMO,NLOOP,FROP,FTYPEN,NREP
C           LNSA,LNIC,LUNIO,NYRS,ERRNUM,ENDSIM,SENSMS,NSENS,YRIC,SEQNO
C           IVRTEM,IVRGRP,IPLT,ISIM,BEXIST,WRESR,WRESND,TSOC,PM06
C           PM09,SWINIT(20),INO3(20),INH4(20),TOTN(20),EFINOC,EFNFIX
C           AINO3,AINH4,TNMIN,ANO3,ANH4,TSWINI,ESW(20),SW(20),TLL,TSW,TDUL
C           TSAT,TPESW,CUMDEP,PESW,CO2,CLDVAR,THVAR,SDPRO,TRIFOL,SIZELF
C           THRESH,LNGSH,RHGHT,RWIDTH
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called :
C
C  Calls  : ERROR CLEAR INTRO IPEXP IPSOIL IPVAR IPECO IPSLIN IPSLAN
C           SENS INSOIL WEATHR IPIBS3 OPIBS3
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================
      PROGRAM INPUT

      IMPLICIT NONE

      INCLUDE 'COMSOI.BLK'
      INCLUDE 'COMIBS.BLK'
      INCLUDE 'COMSWI.BLK'

      CHARACTER*  1 TYPEIO,UPCASE
      CHARACTER*  1 WMODI
      CHARACTER*  2 CROP,PRCROP
      CHARACTER*  3 EXPARG,TRNARG
      CHARACTER*  6 VARNO,VARTY,ERRKEY,ECOTYP,ECONO
      CHARACTER* 12 MODEL,FILEIO
      CHARACTER* 16 ECONAM,VRNAME
      CHARACTER* 25 TITLET
      CHARACTER* 42 CHEXTR(10)
      CHARACTER* 72 INPUTX            !chp changed to 72 (was 52)
      CHARACTER*120 WTHSTR

      INTEGER       NLOOP,FROP,FTYPEN,NREP,IIRV(150)
      INTEGER       LNSA,LNIC,LUNIO,NYRS,ERRNUM,NSENS,YRIC
      INTEGER       IVRGRP,IPLT,ISIM,EXPP,EXPN,TRTN,TRTALL
      INTEGER       NFORC,NDOF,PMTYPE,ISENS
D	INTEGER iyr, imon, iday, ihr, imin, isec, i100th    !chp

      INTEGER*2     IP
C-SUN INTEGER       LNBLNK

      LOGICAL       FEXIST,INITIAL

      REAL          WRESR,WRESND,TSOC,SWINIT(20),CO2
C-PW  REAL          INO3(20),INH4(20),TOTN(20),EFINOC,EFNFIX
      REAL          INO3(20),INH4(20),EFINOC,EFNFIX
      REAL          AINO3,AINH4,TNMIN,ANO3,ANH4,TSWINI
      REAL          ESW(20),SW(20),TLL,TSW,TDUL,TSAT,TPESW,CUMDEP,PESW
      REAL          PLTFOR

      PARAMETER (ERRKEY = 'INPUT ')
      PARAMETER (LUNIO  = 21)

D     OPEN (100,FILE='RUNARGI.OUT',STATUS='UNKNOWN')      !chp
D	CALL GETDAT (iyr, imon, iday)                       !chp
D     CALL GETTIM (ihr, imin, isec, i100th)               !chp
D
D	WRITE(100,10) imon, iday, iyr, ihr, imin            !chp
D  10 FORMAT(/,'Runtime arguments list file',             !chp
D    &   /,'Date: ',I2,'/',I2,'/',I4,                     !chp
D    &   /,'Time: ',I2,':',I2,/)                          !chp

!	print*, "Starting INPUT"                              !chp

C-----------------------------------------------------------------------
C     FILEIO  and TYPEIO are DSSAT version specific.  If either the
C     format or versions of models and/or input files are changes,
C     FILEIO and TYPEIO need to be modified.
C-----------------------------------------------------------------------
C     Get argument from runtime module to determine path and run mode
C-----------------------------------------------------------------------
C   Microsoft Fortran V5.1 - Standard Fortran F77 Implementation
C-----------------------------------------------------------------------
      CALL GETARG (0,INPUTX,IP)
!--------------------------------------------------------------------
!     Debug lines - ignore path to driver
D     INPUTX = 'MINPT980.EXE'
D     IP = 12
!--------------------------------------------------------------------
      CALL PATHD  (DSSATP,INPUTX,IP)
      CALL GETARG (1,MODEL,IP)
      CALL GETARG (2,FILEIO,IP)
      CALL GETARG (3,TYPEIO,IP)
      CALL GETARG (4,RNMODE,IP)
      CALL GETARG (5,EXPARG,IP)
      CALL GETARG (6,TRNARG,IP)
C-----------------------------------------------------------------------
!     Write run-time arguments to console.                !chp
C-----------------------------------------------------------------------
D     WRITE(100,20) INPUTX, MODEL, FILEIO, TYPEIO,          !chp
D    &   RNMODE, EXPARG, TRNARG                           !chp
D  20 FORMAT(/,'Runtime arguments passed to INPUT:',      !chp
D    &   /,'INPUTX: ',A72,                                !chp
D    &   /,'MODEL:  ',A12,                                !chp
D    &   /,'FILEIO: ',A12,                                !chp
D    &   /,'TYPEIO: ',A1,                                 !chp
D    &   /,'RNMODE: ',A1,                                 !chp
D    &   /,'EXPARG: ',A3,                                 !chp
D    &   /,'TRNARG: ',A3)                                 !chp

!	print *,'Read arguments for INPUT'                  !chp
 
C-----------------------------------------------------------------------
C   SUN Fortran V1.3.1 - Standard Fortran F77 Implementation
C-----------------------------------------------------------------------
C-SUN CALL GETARG (0,INPUTX)
C-SUN IP = LNBLNK(INPUTX)
C-SUN CALL PATHD  (DSSATP,INPUTX,IP)
C-SUN CALL GETARG (1,MODEL)
C-SUN CALL GETARG (2,FILEIO)
C-SUN CALL GETARG (3,TYPEIO)
C-SUN CALL GETARG (4,RNMODE)
C-SUN CALL GETARG (5,EXPARG)
C-SUN CALL GETARG (6,TRNARG)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      TYPEIO = UPCASE (TYPEIO)
      IF (INDEX('IX',TYPEIO) .EQ. 0) THEN
         TYPEIO = 'I'
      ENDIF
      RNMODE = UPCASE (RNMODE)
      IF (INDEX('IANQGSF',RNMODE) .EQ. 0) THEN
         RNMODE = 'I'
      ENDIF

C-----------------------------------------------------------------------
C    Initialize
C-----------------------------------------------------------------------

      IF (TYPEIO .EQ. 'I') THEN
         OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERRNUM)
         IF (ERRNUM .NE. 0 .OR. RNMODE .EQ. 'G') THEN
            NREP = 1
          ELSE
            READ (LUNIO,40) NREP,EXPP,TRTN,TRTALL
            READ (LUNIO,50) NYRS
            NREP = NREP + NYRS
            READ (LUNIO,60) MEWTH
            READ (LUNIO,70,IOSTAT=ERRNUM) IOX,IDETO,IDETS,FROP,IDETG,
     &            IDETC,IDETW,IDETN,IDETP,IDETD,IDETL,IDETH,IDETR
         ENDIF
         CLOSE(LUNIO)
       ELSE
         NREP = 1
      ENDIF
C-----------------------------------------------------------------------
C    Delete previouse copy of FILEIO
C-----------------------------------------------------------------------
      INQUIRE (FILE = FILEIO,EXIST = FEXIST)
      IF (FEXIST) THEN
          OPEN (LUNIO, FILE = FILEIO,STATUS = 'UNKNOWN',IOSTAT=ERRNUM)
          CLOSE (LUNIO,STATUS = 'DELETE')
      ENDIF
C-----------------------------------------------------------------------
C     BEGINNING of READING INPUT files
C-----------------------------------------------------------------------
      IF (RNMODE .EQ. 'I' .AND. NREP .EQ. 1) THEN
         CALL CLEAR
         CALL INTRO
      ENDIF
      NSENS  = 0
      ISENS  = 0
      IF (RNMODE .EQ. 'I') THEN
         CALL CLEAR
      ENDIF

C-----------------------------------------------------------------------
C     Call IPEXP
C-----------------------------------------------------------------------
      CALL IPEXP (MODEL,NREP,DS,SLNO,LNIC,LNSA,NYRS,VARNO,CROP,WMODI,
     &     FROP,TRTN,EXPP,EXPN,TITLET,TRTALL,EXPARG,TRNARG,TYPEIO,IIRV,
     &     FTYPEN,CHEXTR,NFORC,PLTFOR,NDOF,PMTYPE)
C    &     FTYPEN)
C-----------------------------------------------------------------------
C     Write message to screen
C-----------------------------------------------------------------------

      IF ((INDEX('NQ',RNMODE) .GT. 0 .AND. NREP .EQ. 1) .OR.
     &    (INDEX(  'A',RNMODE) .GT. 0 .AND. TRTN .EQ. 1)) THEN
         CALL CLEAR
         WRITE (*,103)
      ENDIF

C-----------------------------------------------------------------------
C     Call IPSOIL
C-----------------------------------------------------------------------

      CALL IPSOIL (RNMODE,FILES,PATHSL,NSENS,ISWWAT)

C-----------------------------------------------------------------------
C     Call IPVAR & IPECO
C-----------------------------------------------------------------------

      IF (CROP .NE. 'FA') THEN
         CALL IPVAR (FILEG,NSENS,RNMODE,VARNO,VARTY,VRNAME,PATHGE,
     &               ECONO,CROP)
         IF (INDEX ('BNPNSBTMPECHPPPRC3C4G0G1G2G3G4G5G6G7G8G9SCVB',
     &               CROP) .GT. 0) THEN
            CALL IPECO (FILEE,NSENS,RNMODE,PATHEC,ECOTYP,ECONAM,
     &                  ECONO,IVRGRP)
         ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     Call IPSLIN to read initial soil conditions
C-----------------------------------------------------------------------

!      IF (ISWWAT .NE. 'N' .AND. MESIC .EQ. 'M') THEN
         CALL IPSLIN (FILEX,LNIC,NLAYR,DUL,YRIC,PRCROP,WRESR,
     &        WRESND,EFINOC,EFNFIX,PEDON,SLNO,DS,SWINIT,INH4,INO3,
     &        ISWWAT,ISWNIT,ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID)
         IF (ISIMI .EQ. 'I') THEN
           IF (YRIC .LT. YRSIM .AND. YRIC .GT. 0) THEN
             YRSIM = YRIC
             CALL YR_DOY (YRSIM,YEAR,ISIM)
             IF (MEWTH .EQ. 'M' .OR. MEWTH .EQ. 'G') THEN
                WRITE (FILEW(5:6),'(I2)') YEAR
             ENDIF
           ENDIF
         ENDIF
C-----------------------------------------------------------------------
C        Call IPSLAN to read soil analysis information
C-----------------------------------------------------------------------

         IF (ISWNIT .EQ. 'Y') THEN
            CALL IPSLAN (FILEX,LNSA,BD,OC,PH,PEDON,SLNO,DS,EXTP,TOTN)
         ENDIF
!      ENDIF
C-----------------------------------------------------------------------
C        Sensitivity Analysis Section
C-----------------------------------------------------------------------
      IF (RNMODE .EQ. 'I' .AND. NYRS .EQ. 1) THEN
         NLOOP = 0
  300    CONTINUE
         NLOOP = NLOOP + 1
         IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)
         CALL CLEAR
         WRITE (*,400)
         READ (5,'(I2)',ERR = 300) NSENS
         IF (NSENS .EQ. 1) THEN
            INITIAL = (ISWWAT .EQ.'N')
            CALL SENS (NSENS,VARNO,VARTY,VRNAME,FTYPEN,LNIC,LNSA,
     &        WRESR,WRESND,ISIM,NYRS,IPLT,WMODI,ECONO,ECONAM,ECOTYP,
     &        PRCROP,SWINIT,INO3,INH4,NREP,FROP,YRIC,EFINOC,EFNFIX,
     &        CROP,IVRGRP,ISENS)
            IF (INITIAL) THEN
               IF ((ISWNIT .EQ. 'Y') .OR. (ISWWAT .NE.'N')) THEN
                  NSENS = 0
                  CALL IPSOIL (RNMODE,FILES,PATHSL,NSENS,ISWWAT)
                  CALL IPSLIN (FILEX,LNIC,NLAYR,DUL,YRIC,PRCROP,
     &                 WRESR,WRESND,EFINOC,EFNFIX,PEDON,SLNO,DS,
     &                 SWINIT,INH4,INO3,ISWWAT,ISWNIT,
     &                 ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID)
                  CALL IPSLAN (FILEX,LNSA,BD,OC,PH,PEDON,SLNO,
     &                         DS,EXTP,TOTN)
                  NSENS = 1
               ENDIF
            ENDIF
         ENDIF
         WRITE (*,1000) NREP
         READ (5,'(A25)') TITLER
         IF (TITLER .EQ. '                         ') THEN
            TITLER = TITLET
         ENDIF
       ELSE
         TITLER = TITLET
      ENDIF

C-----------------------------------------------------------------------
C     Call INSOIL to calculate initial conditions for each soil layer
C-----------------------------------------------------------------------

      CALL INSOIL (ISWWAT,ISWNIT,AINO3,ANO3,AINH4,ANH4,TNMIN,
     &  SWINIT,TSWINI,NLAYR,DUL,LL,ESW,DLAYR,SAT,SW,TLL,TDUL,
     &  TSAT,TPESW,CUMDEP,PESW,TSW,BD,INO3,INH4,TSOC,OC,PH,
     &  RESN,RESP,RESIDUE,RINP,DEPRES,ICRES,ICREN,ICREP,ICRIP,
C-PW &  ICRID,NARES,YRSIM,RESAMT,RESDAY,SLTX,SLTXS)
     &  ICRID,NARES,YRSIM,RESAMT,RESDAY,SLTX,SLTXS,TOTN)

C-----------------------------------------------------------------------
C     Call WEATHR to set CO2 conditions and weather parameter modifications
C-----------------------------------------------------------------------

      CALL WEATHR (CO2ADJ,CO2FAC,DAYADJ,DAYFAC,DPTADJ,DPTFAC,PRCADJ,
     &     PRCFAC,RADADJ,RADFAC,TMADJ,TMFAC,TXADJ,TXFAC,WMODI,WNDADJ,
     &     WNDFAC,WTHADJ,CO2,WTHSTR,NEV)

C-----------------------------------------------------------------------
C     Write temporary output files for runtime modules
C-----------------------------------------------------------------------
C     Write IBSNAT Format Version 3 Output file for input by Version 3
C     models
C-----------------------------------------------------------------------

      IF (TYPEIO .EQ. 'I') THEN
          CALL IPIBS3 (YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,SWINIT,
     &         INH4,INO3,NYRS,VARNO,VRNAME,CROP,MODEL,NREP,FILEIO,
     &         EXPN,ECONO,FROP,TRTALL,IIRV,TRTN,CHEXTR,
     &         NFORC,PLTFOR,NDOF,PMTYPE,ISENS)
      ELSE IF (TYPEIO .EQ. 'X') THEN
          CALL IPFILX (YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,
     &         SWINIT,INH4,INO3,NYRS,VARNO,VRNAME,CROP,NREP,
!chp &         FILEIO,FROP,ECONO)
     &         FILEIO,FROP)
      ENDIF

C-----------------------------------------------------------------------
C     Write IBSNAT Format Version 3 Output files
C-----------------------------------------------------------------------

      CALL OPIBS3 (CUMDEP,TPESW,VRNAME,AINO3,AINH4,TLL,TDUL,TSAT,
     &     TSWINI,ECONAM,NREP,MODEL,CROP,CROPD,TITLET,ECOTYP,VARTY,
     &     ESW,SWINIT,INO3,INH4,TSOC,WTHSTR,NYRS)

D     CLOSE (100)
C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

  40  FORMAT (30X,4(1X,I5))
  50  FORMAT (13(/),18X,I2)
  60  FORMAT (/,14X,5X,A1)
  70  FORMAT (/,14X,3(5X,A1),4X,I2,9(5X,A1))
 103  FORMAT (/////,20X,'The crop model is currently running in a',/
     &             20X,'NON-INTERACTIVE MODE.',//,
     &             20X,'Do not interrupt this SIMULATION.',///,
     &             20X,'Please do not TOUCH the keyboard !!')
 400  FORMAT (/////,5X,'What Would You Like To Do ?',
     &            //,1X,' 0. Run Simulation.',
     &             /,1X,' 1. Select Sensitivity Analysis Options.',
     &            //,1X,'    CHOICE ?   [ Default = 0 ] ===> ',$)
 1000 FORMAT (/,5X,'Please enter Run',I3,' name : ===> ',$)

      END
