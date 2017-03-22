C=======================================================================
C  IPFILX, Subroutine
C  Writes out "gray" file with only one treatment in FILEX format, after
C  reading in a standard FILEX
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by J. W. Jones                                   8-08-93
C  3. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,SWINIT,INH4,INO3,
C           NYRS,VARNO,VRNAME,CROP,PATHMO,FROP,NREP,FILEIO
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : INPUT
C
C  Calls  : ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C=======================================================================

      SUBROUTINE IPFILX (YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,
     &           SWINIT,INH4,INO3,NYRS,VARNO,VRNAME,CROP,NREP,
     &           FILEIO,FROP)

      IMPLICIT NONE

      INCLUDE 'COMIBS.BLK'
      INCLUDE 'COMSOI.BLK'
      INCLUDE 'COMSWI.BLK'
      INCLUDE 'COMGEN.BLK'

      CHARACTER* 1 IC,MI,MF,MR,ME,MH
      CHARACTER* 2 CROP,PRCROP,LABL
      CHARACTER* 6 ERRKEY,VARNO
      CHARACTER*12 FILEIO
      CHARACTER*16 VRNAME

      INTEGER NYRS,NREP,I,LUNIO,ERRNUM,FROP,YRIC
      REAL    SWINIT(20),WRESR,WRESND,EFINOC,EFNFIX,INO3(20),INH4(20)

      PARAMETER (LUNIO = 21)
      PARAMETER (ERRKEY = 'IPFILX')

      IC = '0'
      MI = '0'
      MF = '0'
      MR = '0'
      ME = '0'
      MH = '0'

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO,STATUS = 'UNKNOWN',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,0)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      WRITE (LUNIO,50,IOSTAT=ERRNUM)'*EXP.DETAILS: ',EXPER,CG,ENAME
      WRITE (LUNIO,40)'*FILES              '
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      WRITE (LUNIO,45)'@N EXP.DATA    AFILE        ADIR '
      WRITE (LUNIO,2100,IOSTAT=ERRNUM) FILEA
 2100 FORMAT(' 1 EXP         ',A12,1X,A80)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      WRITE (LUNIO,45)'@N SPECIES     SPFILE       SPDIR'
      WRITE (LUNIO,2200,IOSTAT=ERRNUM) FILEC,PATHCR
 2200 FORMAT(' 1 SPE         ',A12,1X,A80)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      WRITE (LUNIO,45)'@N ECOTYPE     EFILE        EDIR '
      WRITE (LUNIO,2300,IOSTAT=ERRNUM) FILEE,PATHEC
 2300 FORMAT(' 1 ECO         ',A12,1X,A80)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      WRITE (LUNIO,45)'@N CULTIVAR    CFILE        CDIR '
      WRITE (LUNIO,2500,IOSTAT=ERRNUM) FILEG,PATHGE
 2500 FORMAT(' 1 CUL         ',A12,1X,A80)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      WRITE (LUNIO,45)'@N SOIL        SFILE        SDIR '
      WRITE (LUNIO,2700,IOSTAT=ERRNUM) FILES,PATHSL
 2700 FORMAT(' 1 SOIL        ',A12,1X,A80)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      WRITE (LUNIO,45)'@N WEATHER     WFILE        WDIR '
      WRITE (LUNIO,2800,IOSTAT=ERRNUM) FILEW,PATHWT
 2800 FORMAT(' 1 WEATH       ',A12,1X,A80)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      WRITE (LUNIO,40)'*SIMULATION CONTROL '
      LABL = 'GE'
      WRITE(LUNIO,52)'@N GENERAL     NYERS NREPS START SDATE RSEED SNAME
     &....................           '
      WRITE(LUNIO,900,IOSTAT=ERRNUM) LABL,NYRS,NREP,ISIMI,
     &     YRSIM,RSEED1,TITSIM
 900  FORMAT(' 1 ',A2,9X,4X,I2,2X,I4,5X,A1,2(1X,I5),1X,A25)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      WRITE(LUNIO,52)'@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES
     &  CHEM  TILL                   '
      LABL = 'OP'
      WRITE (LUNIO,910,IOSTAT=ERRNUM) LABL,ISWWAT,ISWNIT,ISWSYM,
     &     ISWPHO,ISWPOT,ISWDIS,ISWCHE,ISWTIL
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LABL = 'ME'
      WRITE(LUNIO,52)'@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO
     & HYDRO NSWIT SOMDY            '
      WRITE (LUNIO,915,IOSTAT=ERRNUM) LABL,MEWTH,MESIC,
     &      MELI,MEEVP,MEINF,MEPHO,MEHYD,NSWITCH,MESOM
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LABL = 'MA'
      WRITE(LUNIO,52)'@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS
     &                              '
      WRITE (LUNIO,910,IOSTAT=ERRNUM)LABL,IPLTI,IIRRI,IFERI,IRESI,IHARI
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LABL = 'OU'
      WRITE(LUNIO,52)'@N OUTPUTS     XCODE OVVEW SUMRY FROPT GROTH CARBN
     & WATR  NITR  MINER DISES  LONG'
      WRITE (LUNIO,920,IOSTAT=ERRNUM) LABL,IOX,IDETO,
     &     IDETS,FROP,IDETG,IDETC,IDETW,IDETN,IDETP,IDETD,IDETL
 920  FORMAT(' 1 ',A2,9X,3(5X,A1),4X,I2,7(5X,A1))
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      WRITE (LUNIO,40)'!AUTOMATIC MANAGEM  '
      LABL = 'PL'
      WRITE(LUNIO,52)'@N PLANTING    PFRST PLAST PH2OL PH2OU PH2OD PSTMX PSTMN
     & PSTMN                        '
      WRITE(LUNIO,930,IOSTAT=ERRNUM) LABL,PWDINF,PWDINL,
     &     SWPLTL,SWPLTH,SWPLTD,PTX,PTTN
 930  FORMAT(' 1 ',A2,9X,2(1X,I5),5(1X,F5.0))
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LABL = 'IR'
      WRITE(LUNIO,52)'@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT
     & IREFF                        '
      WRITE(LUNIO,940,IOSTAT=ERRNUM) LABL,DSOIL,THETAC,
     &      IEPT,IOFF,IAME,AIRAMT,EFFIRR
 940  FORMAT(' 1 ',A2,9X,3(1X,F5.0),2(1X,A5),1X,F5.1,1X,F5.3)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LABL = 'NI'
      WRITE(LUNIO,52)'@N NITROGEN    NMDEP NMTHR NAMNT NCODE NAOFF
     &                               '
      WRITE(LUNIO,940,IOSTAT=ERRNUM)LABL,DSOILN,SOILNC,SOILNX,NCODE,NEND
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LABL = 'RE'
      WRITE(LUNIO,53)'@N RESIDUES    RIPCN RTIME RIDEP        '
      WRITE (LUNIO,950,IOSTAT=ERRNUM) LABL,RIP,NRESDL,DRESMG
 950  FORMAT(' 1 ',A2,10X,F5.0,1X,I5,1X,F5.0)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LABL = 'HA'
      WRITE(LUNIO,53)'@N HARVEST     HFRST HLAST HPCNP HRCNR  '
      WRITE (LUNIO,930,IOSTAT=ERRNUM) LABL,HDLAY,HLATE,HPP,HRP
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      WRITE (LUNIO,40)'*TREATMENTS         '
      WRITE(LUNIO,52)'@N R O C TNAME.................... CU FL SA IC MP
     &MI MF MR MC MT ME MH SM       '
      IF (ISWWAT .NE. 'N') IC = '1'
      IF (IIRRI  .NE. 'N') MI = '1'
      IF (NFERT  .GT. 0)   MF = '1'
      IF (NARES  .GT. 0)   MR = '1'
      IF (NEV    .GT. 0)   ME = '1'
      IF (NHAR   .GT. 0)   MH = '1'
      WRITE (LUNIO,55,IOSTAT=ERRNUM)TRTNO,ROTNO,ROTOPT,CRPNO,TITLER,
     &'  1  1  0  ',IC,'  1  ',MI,MF,MR,'0  0  ',ME,MH,'1 '
 55   FORMAT(I2,3(1X,I1),1X,A25,A11,A1,A5,3(A1,2X),A6,2(A1,2X),A1)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      WRITE (LUNIO,40)'*CULTIVARS          '
      WRITE (LUNIO,40)'@C CR INGENO CNAME  '
      WRITE (LUNIO,56,IOSTAT=ERRNUM) CROP,VARNO,VRNAME
 56   FORMAT(' 1 ',A2,1X,A6,1X,A16)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      WRITE (LUNIO,40)'*FIELDS             '
      WRITE(LUNIO,52)'@L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS
     &  FLST SLTX   SLDP ID_SOIL    '
      WRITE(LUNIO,60,IOSTAT=ERRNUM) FLDNAM,FILEW(1:8),SLOPE,
     &   FLOB, DFDRN,FLDD,SFDRN,FLST,SLTX,SLDP,SLNO
 60   FORMAT(' 1 ',A8,1X,A8,1X,F5.1,1X,F5.0,1X,A5,2(1X,F5.0),
     &       2(1X,A5),1X,F5.0,1X,A10)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      IF(ISWWAT .NE. 'N') THEN
      WRITE (LUNIO,40)'*INITIAL CONDITIONS '
      WRITE(LUNIO,53)'@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  '
      WRITE(LUNIO,61,IOSTAT=ERRNUM) PRCROP,YRIC,WRESR,
     &    WRESND,EFINOC,EFNFIX
 61       FORMAT(' 1 ',3X,A2,1X,I5,2(1X,F5.0),2(1X,F5.2))
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      WRITE(LUNIO,53)'@C  ICBL  SH2O  SNH4  SNO3              '
      DO I = 1, NLAYR
        WRITE(LUNIO,62,IOSTAT=ERRNUM) DS(I),SWINIT(I),INH4(I),INO3(I)
 62     FORMAT(' 1 ',F5.0,1X,F5.3,2(1X,F5.1))
      ENDDO
      ENDIF
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      WRITE (LUNIO,40)'*PLANTING DETAILS   '
      WRITE(LUNIO,52)'@P PDATE EDATE  PPOP  PPOE  PLME  PLDS  PLRS  PLRD
     &  PLDP  PLWT  PAGE  PENV  PLPH'
      WRITE(LUNIO,70,IOSTAT=ERRNUM) YRPLT,IEMRG,PLANTS,PLTPOP,
     &     PLME,PLDS,ROWSPC,AZIR,SDEPTH,SDWTPL,SDAGE,ATEMP,PLPH
 70   FORMAT(' 1 ',I5,3X,I3,2(1X,F5.1),2(5X,A1),2(1X,F5.0),1X,F5.1,
     &  2(1X,F5.0),2(1X,F5.1))
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      WRITE (LUNIO,40)'*IRRIGATION         '
      IF(IIRRI .NE. 'N') THEN
      WRITE(LUNIO,54)'@I  IEFF  IDEP  ITHR  IEPT  IOFF  IAME  IAMT
     &'
      WRITE(LUNIO,75,IOSTAT=ERRNUM) EFFIRX,DSOILX,THETCX,IEPTX,
     &     IOFFX,IAMEX,AIRAMX
 75      FORMAT(' 1 ',F5.3,3(1X,F5.0),2(1X,A5),1X,F5.1)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
        IF (NIRR .GT. 0) THEN
      WRITE(LUNIO,53)'@I IDATE  IROP IRVAL                    '
         DO I = 1,NIRR
         WRITE(LUNIO,76,IOSTAT=ERRNUM)IDLAPL(I),IRRCOD(I),AMT(I)
 76      FORMAT(' 1 ',I5,1X,A5,1X,F5.0)
         ENDDO
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
       IF (NFERT .GT. 0) THEN
      WRITE (LUNIO,40)'*FERTILIZERS        '
      WRITE(LUNIO,52)'@F FDATE  FMCD  FACD  FDEP  FAMN  FAMP  FAMK  FAMC
     &  FAMO  FOCD                   '
      DO I = 1,NFERT
      WRITE(LUNIO,77,IOSTAT=ERRNUM)FDAY(I),IFTYPE(I),FERCOD(I),
     &  DFERT(I),ANFER(I),APFER(I),AKFER(I),ACFER(I),AOFER(I),FOCOD(I)
 77   FORMAT(' 1 ',I5,2(1X,A5),6(1X,F5.0),1X,A5)
      ENDDO
      ENDIF
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      IF (NARES .GT. 0) THEN
      WRITE (LUNIO,40)'*RESIDUES           '
      WRITE(LUNIO,54)'@R RDATE  RCOD  RAMT  RESN  RESP  RESK  RINP  RDEP
     &'
      DO I = 1,NARES
      WRITE(LUNIO,79,IOSTAT=ERRNUM)RESDAY(I),RESCOD(I),INT(RESIDUE(I)),
     &   RESN(I),RESP(I),RESK(I),RINP(I),DEPRES(I)
 79   FORMAT(' 1 ',I5,1X,A5,1X,I5,3(1X,F5.2),2(1X,F5.0))
      ENDDO
      ENDIF
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      IF (NEV .GT. 0) THEN
      WRITE (LUNIO,40)'*ENVIRONMENT        '
      WRITE(LUNIO,52)'@E ODATE  EDAY  ERAD  EMAX  EMIN ERAIN  ECO2  EDEW
     & EWIND                         '
      DO I = 1,NEV
      WRITE(LUNIO,90,IOSTAT=ERRNUM)
     &        WMDATE(I),DAYFAC(I),DAYADJ(I),RADFAC(I),
     &        RADADJ(I),TXFAC(I),TXADJ(I),TMFAC(I),TMADJ(I),
     &        PRCFAC(I),PRCADJ(I),CO2FAC(I),INT(CO2ADJ(I)),
     &        DPTFAC(I),DPTADJ(I),WNDFAC(I),WNDADJ(I)
 90   FORMAT(' 1 ',I5,5(1X,A1,F4.1),1X,A1,I4,2(1X,A1,F4.1))
      ENDDO
      ENDIF
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      IF (NHAR .GT. 0) THEN
      WRITE (LUNIO,40)'*HARVEST            '
      WRITE(LUNIO,53)'@H HDATE  HSTG  HCOM  HSIZ   HPC        '
      DO I = 1,NHAR
      WRITE(LUNIO,100,IOSTAT=ERRNUM)HDATE(I),HSTG(I),HCOM(I),
     &     HSIZ(I),HPC(I)
 100  FORMAT(' 1 ',I5,3(1X,A5),1X,F5.0)
      ENDDO
      ENDIF
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      CLOSE(LUNIO)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------
   40 FORMAT (A20)
   45 FORMAT (A33)
   50 FORMAT (A14,A8,A2,1X,A60)
   52 FORMAT (A80)
   53 FORMAT (A40)
   54 FORMAT (A50)
  910 FORMAT(' 1 ',A2,9X,10(5X,A1))
  915 FORMAT(' 1 ',A2,9X,7(5X,A1),1X,I5,5(5X,A1))
      END
