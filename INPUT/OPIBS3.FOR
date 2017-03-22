C=======================================================================
C  OPIBS3, Subroutine
C
C  Generates output for simulated data
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       5-28-93
C  4. Added new output files for Flooding and Chem. G.H.        4-02-96
C-----------------------------------------------------------------------
C  INPUT  : CUMDEP,TPESW,VRNAME,AINO3,AINH4,TLL,TDUL,TSAT,TSWINI,WTHADJ,
C           CO2,ECONAM,NREP,MODEL,CROP,CROPD,TITLER,ECOTYP,VARTY,ESW,
C           SWINIT,INO3,INH4,TSOC
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : INPUT
C
C  Calls  : CLEAR OPHEAD OPSOIL
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPIBS3 (CUMDEP,TPESW,VRNAME,AINO3,AINH4,TLL,TDUL,TSAT,
     &     TSWINI,ECONAM,NREP,MODEL,CROP,CROPD,TITLET,ECOTYP,VARTY,
     &     ESW,SWINIT,INO3,INH4,TSOC,WTHSTR,NYRS)

      IMPLICIT NONE

      INCLUDE 'COMSOI.BLK'
      INCLUDE 'COMSWI.BLK'

      CHARACTER*  1 ANS
      CHARACTER*  2 CROP
      CHARACTER*  6 VARTY,ECOTYP
      CHARACTER* 10 CROPD
      CHARACTER* 12 OUTB,OUTV,MODEL
      CHARACTER* 16 ECONAM,VRNAME
      CHARACTER* 25 TITLET
      CHARACTER*120 WTHSTR

      INTEGER NOUTDG,NOUTDC,NOUTDW,NOUTDP,NOUTDN,NOUTDD,NOUTDO,NOUTDB
      INTEGER NOUTDV,NOUTDF,NOUTDH,NOUTDR
      INTEGER NYRS,NREP
      INTEGER LUNOV

      REAL    AINO3,AINH4
      REAL    SWINIT(20),TSWINI,INO3(20),INH4(20)
      REAL    CUMDEP,TPESW,ESW(20)
      REAL    TLL,TDUL,TSAT,TSOC

      PARAMETER (NOUTDG = 30)
      PARAMETER (NOUTDW = 31)
      PARAMETER (NOUTDN = 32)
      PARAMETER (NOUTDD = 33)
      PARAMETER (NOUTDC = 34)
      PARAMETER (NOUTDP = 35)
      PARAMETER (NOUTDB = 36)
      PARAMETER (NOUTDR = 38)
      PARAMETER (NOUTDF = 39)
      PARAMETER (NOUTDH = 40)
      PARAMETER (NOUTDV = 41)
      PARAMETER (LUNOV  = 6)

      NOUTDO = 37
      OUTB   = 'NBAL.OUT'
      OUTV   = 'WBAL.OUT'

C-----------------------------------------------------------------------
C     Generate a summary output for the screen
C-----------------------------------------------------------------------

      IF (RNMODE .EQ. 'I' .AND. NYRS .LE. 1) THEN
         CALL CLEAR
         WRITE (LUNOV,50)
         CALL OPHEAD (LUNOV,CUMDEP,TPESW,VRNAME,AINO3,AINH4,
     &     ECONAM,NREP,MODEL,CROP,TITLET,WTHSTR)
         ENDIF

C-----------------------------------------------------------------------
C     Generate headings for output files
C-----------------------------------------------------------------------

      IF (NREP .EQ. 1) THEN
         OPEN (UNIT=NOUTDO,FILE=OUTO,STATUS='UNKNOWN')
         OPEN (UNIT=NOUTDG,FILE=OUTG,STATUS='UNKNOWN')
         OPEN (UNIT=NOUTDW,FILE=OUTW,STATUS='UNKNOWN')
         OPEN (UNIT=NOUTDN,FILE=OUTN,STATUS='UNKNOWN')
         OPEN (UNIT=NOUTDC,FILE=OUTC,STATUS='UNKNOWN')
         OPEN (UNIT=NOUTDD,FILE=OUTD,STATUS='UNKNOWN')
         OPEN (UNIT=NOUTDB,FILE=OUTB,STATUS='UNKNOWN')
         OPEN (UNIT=NOUTDP,FILE=OUTP,STATUS='UNKNOWN')
         OPEN (UNIT=NOUTDV,FILE=OUTV,STATUS='UNKNOWN')
         OPEN (UNIT=NOUTDF,FILE=OUTF,STATUS='UNKNOWN')
         OPEN (UNIT=NOUTDH,FILE=OUTH,STATUS='UNKNOWN')
         OPEN (UNIT=NOUTDR,FILE=OUTR,STATUS='UNKNOWN')
       ELSE
         OPEN (UNIT=NOUTDO,FILE=OUTO,STATUS='UNKNOWN',ACCESS='APPEND')
         OPEN (UNIT=NOUTDG,FILE=OUTG,STATUS='UNKNOWN',ACCESS='APPEND')
         OPEN (UNIT=NOUTDW,FILE=OUTW,STATUS='UNKNOWN',ACCESS='APPEND')
         OPEN (UNIT=NOUTDN,FILE=OUTN,STATUS='UNKNOWN',ACCESS='APPEND')
         OPEN (UNIT=NOUTDC,FILE=OUTC,STATUS='UNKNOWN',ACCESS='APPEND')
         OPEN (UNIT=NOUTDD,FILE=OUTD,STATUS='UNKNOWN',ACCESS='APPEND')
         OPEN (UNIT=NOUTDB,FILE=OUTB,STATUS='UNKNOWN',ACCESS='APPEND')
         OPEN (UNIT=NOUTDP,FILE=OUTP,STATUS='UNKNOWN',ACCESS='APPEND')
         OPEN (UNIT=NOUTDV,FILE=OUTV,STATUS='UNKNOWN',ACCESS='APPEND')
         OPEN (UNIT=NOUTDF,FILE=OUTF,STATUS='UNKNOWN',ACCESS='APPEND')
         OPEN (UNIT=NOUTDH,FILE=OUTH,STATUS='UNKNOWN',ACCESS='APPEND')
         OPEN (UNIT=NOUTDR,FILE=OUTR,STATUS='UNKNOWN',ACCESS='APPEND')
      ENDIF

      IF (IDETO .EQ. 'Y') THEN
         IF (NREP .EQ. 1) WRITE (NOUTDO,60)
         CALL OPHEAD (NOUTDO,CUMDEP,TPESW,VRNAME,AINO3,AINH4,
     &     ECONAM,NREP,MODEL,CROP,TITLET,WTHSTR)
      ENDIF

      IF (IDETG .EQ. 'Y') THEN
         IF (NREP .EQ. 1) WRITE (NOUTDG,100)
         CALL OPHEAD (NOUTDG,CUMDEP,TPESW,VRNAME,AINO3,AINH4,
     &     ECONAM,NREP,MODEL,CROP,TITLET,WTHSTR)
      ENDIF

      IF (IDETW .EQ. 'Y') THEN
         IF (NREP .EQ. 1) THEN
            WRITE (NOUTDW,110)
            WRITE (NOUTDV,111)
         ENDIF
         CALL OPHEAD (NOUTDW,CUMDEP,TPESW,VRNAME,AINO3,AINH4,
     &     ECONAM,NREP,MODEL,CROP,TITLET,WTHSTR)
         CALL OPHEAD (NOUTDV,CUMDEP,TPESW,VRNAME,AINO3,AINH4,
     &     ECONAM,NREP,MODEL,CROP,TITLET,WTHSTR)
      ENDIF

      IF (IDETN .EQ. 'Y') THEN
         IF (NREP .EQ. 1) THEN
            WRITE (NOUTDN,120)
            WRITE (NOUTDB,121)
            IF (CROP .EQ. 'RI') THEN
               WRITE (NOUTDF,122)
            ENDIF
         ENDIF
         CALL OPHEAD (NOUTDN,CUMDEP,TPESW,VRNAME,AINO3,AINH4,
     &     ECONAM,NREP,MODEL,CROP,TITLET,WTHSTR)
         CALL OPHEAD (NOUTDB,CUMDEP,TPESW,VRNAME,AINO3,AINH4,
     &     ECONAM,NREP,MODEL,CROP,TITLET,WTHSTR)
         IF (CROP .EQ. 'RI') THEN
           CALL OPHEAD (NOUTDF,CUMDEP,TPESW,VRNAME,AINO3,AINH4,
     &       ECONAM,NREP,MODEL,CROP,TITLET,WTHSTR)
         ENDIF
      ENDIF

      IF (IDETC .EQ. 'Y') THEN
         IF (NREP .EQ. 1) WRITE (NOUTDC,123)
         CALL OPHEAD (NOUTDC,CUMDEP,TPESW,VRNAME,AINO3,AINH4,
     &     ECONAM,NREP,MODEL,CROP,TITLET,WTHSTR)
      ENDIF

      IF (IDETD .EQ. 'Y') THEN
         IF (NREP .EQ. 1) WRITE (NOUTDD,125)
         CALL OPHEAD (NOUTDD,CUMDEP,TPESW,VRNAME,AINO3,AINH4,
     &     ECONAM,NREP,MODEL,CROP,TITLET,WTHSTR)
      ENDIF

      IF (IDETP .EQ. 'Y') THEN
         IF (NREP .EQ. 1) WRITE (NOUTDP,126)
         CALL OPHEAD (NOUTDP,CUMDEP,TPESW,VRNAME,AINO3,AINH4,
     &     ECONAM,NREP,MODEL,CROP,TITLET,WTHSTR)
      ENDIF

      IF (IDETH .EQ. 'Y') THEN
         IF (NREP .EQ. 1) WRITE (NOUTDH,127)
         CALL OPHEAD (NOUTDH,CUMDEP,TPESW,VRNAME,AINO3,AINH4,
     &     ECONAM,NREP,MODEL,CROP,TITLET,WTHSTR)
      ENDIF

      IF (IDETR .EQ. 'Y') THEN
         IF (NREP .EQ. 1) WRITE (NOUTDR,128)
         CALL OPHEAD (NOUTDR,CUMDEP,TPESW,VRNAME,AINO3,AINH4,
     &     ECONAM,NREP,MODEL,CROP,TITLET,WTHSTR)
      ENDIF

C-----------------------------------------------------------------------
C     Changed conditional if statements to delete previous output files
C     if output option is N .. request from PKT
C-----------------------------------------------------------------------

      IF (IDETG .EQ. 'N') THEN
         CLOSE (UNIT=NOUTDG,STATUS='DELETE')
       ELSE
         CLOSE (NOUTDG)
      ENDIF
      IF (IDETW .EQ. 'N') THEN
         CLOSE (UNIT=NOUTDW,STATUS='DELETE')
         CLOSE (UNIT=NOUTDV,STATUS='DELETE')
       ELSE
         CLOSE (NOUTDW)
         CLOSE (NOUTDV)
      ENDIF
      IF (CROP .NE. 'RI') THEN
         CLOSE (UNIT=NOUTDF,STATUS='DELETE')
      ENDIF
      IF (IDETN .EQ. 'N') THEN
         CLOSE (UNIT=NOUTDN,STATUS='DELETE')
         CLOSE (UNIT=NOUTDB,STATUS='DELETE')
         IF (CROP .EQ. 'RI') THEN
            CLOSE (UNIT=NOUTDF,STATUS='DELETE')
         ENDIF
       ELSE
         CLOSE (NOUTDN)
         CLOSE (NOUTDB)
         IF (CROP .EQ. 'RI') THEN
            CLOSE (NOUTDF)
         ENDIF
      ENDIF
      IF (IDETD .EQ. 'N') THEN
         CLOSE (UNIT=NOUTDD,STATUS='DELETE')
       ELSE
         CLOSE (NOUTDD)
      ENDIF
      IF (IDETC .EQ. 'N') THEN
         CLOSE (UNIT=NOUTDC,STATUS='DELETE')
       ELSE
         CLOSE (NOUTDC)
      ENDIF
      IF (IDETP .EQ. 'N') THEN
         CLOSE (UNIT=NOUTDP,STATUS='DELETE')
       ELSE
         CLOSE (NOUTDP)
      ENDIF
      IF (IDETH .EQ. 'N') THEN
         CLOSE (UNIT=NOUTDH,STATUS='DELETE')
       ELSE
         CLOSE (NOUTDH)
      ENDIF
      IF (IDETR .EQ. 'N') THEN
         CLOSE (UNIT=NOUTDR,STATUS='DELETE')
       ELSE
         CLOSE (NOUTDR)
      ENDIF

      CALL OPSOIL (IDETO,RNMODE,NOUTDO,NYRS,LL,DUL,SAT,
     &     DLAYR,SWINIT,DS,NLAYR,ESW,SHF,BD,PH,INO3,INH4,OC,
     &     TLL,TDUL,TSAT,TPESW,TSWINI,AINO3,AINH4,TSOC,
     &     SWCON,U,SALB,CN2,CROPD,VRNAME,VARTY,SLPF,
     &     ECONAM,ECOTYP,SLNF,LUNOV,CROP,ISWWAT)

      IF (IDETO .EQ. 'N') THEN
         CLOSE (UNIT=NOUTDO,STATUS='DELETE')
       ELSE
         CLOSE (NOUTDO)
      ENDIF

      IF (RNMODE .EQ. 'I' .AND.  NYRS .LE. 1) THEN
         WRITE (*,2900)
         READ (5,'(1A1)') ANS
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

  50  FORMAT ('*SIMULATION OVERVIEW')
  60  FORMAT (/,'*SIMULATION OVERVIEW FILE')
 100  FORMAT (/,'*GROWTH ASPECTS OUTPUT FILE')
 110  FORMAT (/,'*WATER BALANCE OUTPUT FILE')
 111  FORMAT (/,'*WATER BALANCE SUMMARY FILE')
 120  FORMAT (/,'*NITROGEN BALANCE OUTPUT FILE')
 121  FORMAT (/,'*NITROGEN BALANCE SUMMARY FILE')
 122  FORMAT (/,'*FLOODWATER NITROGEN OUTPUT FILE')
 123  FORMAT (/,'*CARBON BALANCE OUTPUT FILE')
 125  FORMAT (/,'*PEST ASPECTS OUTPUT FILE')
 126  FORMAT (/,'*PHOSPHOROUS ASPECTS OUTPUT FILE')
 127  FORMAT (/,'*CHEMICAL BALANCE OUTPUT FILE')
 128  FORMAT (/,'*OPERATIONS OUTPUT FILE')
 2900 FORMAT (1X,'Please press < ENTER > key to continue ',$)

      END

C=======================================================================
C  OPSOIL, Subroutine
C
C  Generates output for soil data
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P. Wilkens  5-28-93
C-----------------------------------------------------------------------
C  INPUT  : IDETO,NOUTDO,NYRS,LL,DUL,SAT,DLAYR,SWINIT,DS,NLAYR,ESW
C           SHF,BD,PH,INO3,INH4,OC,TLL,TDUL,TSAT,TPESW,TSWINI,AINO3,AINH4
C           TSOC,SWCON,U,SALB,CN2,CROPD,VRNAME,VARTY,SLPF,ECONAM,ECOTYP
C           SLNF,LUNOV,CROP
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPIBS3
C
C  Calls  : CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPSOIL (IDETO,RNMODE,NOUTDO,NYRS,LL,DUL,SAT,
     &   DLAYR,SWINIT,DS,NLAYR,ESW,SHF,BD,PH,INO3,INH4,OC,
     &   TLL,TDUL,TSAT,TPESW,TSWINI,AINO3,AINH4,TSOC,
     &   SWCON,U,SALB,CN2,CROPD,VRNAME,VARTY,SLPF,
     &   ECONAM,ECOTYP,SLNF,LUNOV,CROP,ISWWAT)

      IMPLICIT NONE

      INCLUDE 'COMGEN.BLK'

      CHARACTER*1  IDETO,RNMODE,ANS,ISWWAT
      CHARACTER*2  CROP
      CHARACTER*6  VARTY,ECOTYP
      CHARACTER*10 CROPD
      CHARACTER*16 VRNAME,ECONAM

      INTEGER      NLAYR,I,L,NOUTDO,NYRS,LUNOV

      REAL         LL(20),DUL(20),SAT(20),DLAYR(20),DS(20),SWINIT(20)
      REAL         ESW(20),SHF(20),BD(20),PH(20),INO3(20),INH4(20)
      REAL         OC(20),TLL,TDUL,TSAT,TPESW,TSWINI,AINO3,AINH4,TSOC
      REAL         SWCON,U,SALB,CN2,SLPF,SLNF

      IF (RNMODE .EQ. 'I' .AND. NYRS .LE. 1) THEN
         WRITE (LUNOV,100)
         READ (5,'(1A1)') ANS
         CALL CLEAR
         WRITE (LUNOV,300)
         IF (ISWWAT .NE. 'N') THEN
            WRITE(LUNOV,350)
            DO I = 1, NLAYR
            WRITE (LUNOV,400) NINT(DS(I)-DLAYR(I)),NINT(DS(I)),
     &        LL(I),DUL(I),SAT(I),ESW(I),SWINIT(I),SHF(I),BD(I),
     &        PH(I),INO3(I),INH4(I),OC(I)
            ENDDO
            WRITE (LUNOV,600) NINT(DS(NLAYR)),TLL,TDUL,TSAT,TPESW,
     &                        TSWINI,AINO3,AINH4,NINT(TSOC)
            WRITE (LUNOV,700) SALB,U,SLNF,CN2,SWCON,SLPF
         ENDIF
         WRITE (LUNOV,800) CROPD,VARTY,VRNAME,ECOTYP,ECONAM
         IF (INDEX ('BNPNSBFAPECHPPVB',CROP) .GT. 0) THEN
            WRITE (LUNOV, 900) CSDVAR,PPSEN,PH2T5,
     &                         PHTHRS(8),PHTHRS(10)
            WRITE (LUNOV,1000) WTPSD,SDPDVR,SFDUR,PODUR,XFRUIT
         ELSEIF (INDEX ('TMPR',CROP) .GT. 0) THEN
            WRITE (LUNOV, 900) CSDVAR,PPSEN,PH2T5,
     &                         PHTHRS(8),PHTHRS(10)
            WRITE (LUNOV,1010) WTPSD,SDPDVR,SFDUR,PODUR,XFRUIT
          ELSEIF (INDEX ('MZWHSGBAML',CROP) .GT. 0) THEN
            IF (CROP .EQ. 'MZ') THEN
               WRITE (LUNOV, 901) P1,P2,P5
               WRITE (LUNOV,1001) G2,G3,PHINT
             ELSE IF (CROP .EQ. 'SG') THEN
               WRITE (LUNOV, 902) P1,P2O,P2R,P5
               WRITE (LUNOV,1002) G1,G2,PHINT
             ELSE IF (CROP .EQ. 'ML') THEN
               WRITE (LUNOV, 903) P1,P2O,P2R,P5
               WRITE (LUNOV,1003) G1,G4,PHINT
             ELSE IF (CROP .EQ. 'BA' .OR. CROP .EQ. 'WH') THEN
               WRITE (LUNOV, 904) P1V,P1D,P5
               WRITE (LUNOV,1004) G1,G2,G3,PHINT
            ENDIF
          ELSEIF (INDEX ('CS',CROP) .GT. 0) THEN
            WRITE (LUNOV,2000) (GCOEFF(L),L=1,15)
          ELSEIF (INDEX ('PT',CROP) .GT. 0) THEN
            WRITE (LUNOV, 905) G2,G3,G4
            WRITE (LUNOV,1005) PD,P2,TC
          ELSEIF (INDEX ('RI',CROP) .GT. 0) THEN
            WRITE (LUNOV, 906) P1,P2R,P5,P2O
            WRITE (LUNOV,1006) G1,G2,G3,G4
          ELSEIF (INDEX ('SC',CROP) .GT. 0) THEN
            WRITE (LUNOV, 907) P1,RATPOT,LFMAX
            WRITE (LUNOV,1007) G1,PI1,PI2,DTTPI
          ELSEIF (INDEX ('SU',CROP) .GT. 0) THEN
            WRITE (LUNOV, 908) P1,P2,P5
            WRITE (LUNOV,1008) G2,G3,O1
          ELSEIF (INDEX ('PI',CROP) .GT. 0) THEN
            WRITE (LUNOV, 909) P2,P3,P4
            WRITE (LUNOV,1009) G2,G3,PHINT
          ELSEIF (INDEX ('TNTR',CROP) .GT. 0) THEN
            WRITE (LUNOV, 911) P1,P3,P4,P5
            WRITE (LUNOV,1011) G3,G4,PHINT,PCINT,PCGRD
          ELSEIF (INDEX ('CO',CROP) .GT. 0) THEN
            WRITE (LUNOV, 912) SCPB,RESPC,SQCON
            WRITE (LUNOV,1012) FCUT,FLAI,DDISQ
         ENDIF
      ENDIF

      IF (IDETO .EQ. 'Y') THEN
         WRITE (NOUTDO,300)
         IF (ISWWAT .NE. 'N') THEN
            WRITE (NOUTDO,350)
            DO I = 1, NLAYR
               WRITE (NOUTDO,400) NINT(DS(I)-DLAYR(I)),NINT(DS(I)),
     &             LL(I),DUL(I),SAT(I),ESW(I),SWINIT(I),SHF(I),BD(I),
     &             PH(I),INO3(I),INH4(I),OC(I)
            END DO
            WRITE (NOUTDO,600) NINT(DS(NLAYR)),TLL,TDUL,TSAT,TPESW,
     &                         TSWINI,AINO3,AINH4,NINT(TSOC)
            WRITE (NOUTDO,700) SALB,U,SLNF,CN2,SWCON,SLPF
         ENDIF
         WRITE (NOUTDO,800) CROPD,VARTY,VRNAME,ECOTYP,ECONAM
         IF (INDEX ('BNPNSBFAPECHPPVB',CROP) .GT. 0) THEN
            WRITE (NOUTDO, 900) CSDVAR,PPSEN,PH2T5,
     &                          PHTHRS(8),PHTHRS(10)
            WRITE (NOUTDO,1000) WTPSD,SDPDVR,SFDUR,PODUR,XFRUIT
          ELSEIF (INDEX ('TMPR',CROP) .GT. 0) THEN
            WRITE (NOUTDO, 900) CSDVAR,PPSEN,PH2T5,
     &                          PHTHRS(8),PHTHRS(10)
            WRITE (NOUTDO,1010) WTPSD,SDPDVR,SFDUR,PODUR,XFRUIT
          ELSEIF (INDEX ('MZWHSGBAML',CROP) .GT. 0) THEN
            IF (CROP .EQ. 'MZ') THEN
               WRITE (NOUTDO, 901) P1,P2,P5
               WRITE (NOUTDO,1001) G2,G3,PHINT
             ELSE IF (CROP .EQ. 'SG') THEN
               WRITE (NOUTDO, 902) P1,P2O,P2R,P5
               WRITE (NOUTDO,1002) G1,G2,PHINT
             ELSE IF (CROP .EQ. 'ML') THEN
               WRITE (NOUTDO, 903) P1,P2O,P2R,P5
               WRITE (NOUTDO,1003) G1,G4,PHINT
             ELSE IF (CROP .EQ. 'BA' .OR. CROP .EQ. 'WH') THEN
               WRITE (NOUTDO, 904) P1V,P1D,P5
               WRITE (NOUTDO,1004) G1,G2,G3,PHINT
            ENDIF
          ELSEIF (INDEX ('CS',CROP) .GT. 0) THEN
            WRITE (NOUTDO,2000) (GCOEFF(L),L=1,15)
          ELSEIF (INDEX ('PT',CROP) .GT. 0) THEN
            WRITE (NOUTDO, 905) G2,G3,G4
            WRITE (NOUTDO,1005) PD,P2,TC
          ELSEIF (INDEX ('RI',CROP) .GT. 0) THEN
            WRITE (NOUTDO, 906) P1,P2R,P5,P2O
            WRITE (NOUTDO,1006) G1,G2,G3,G4
          ELSEIF (INDEX ('SC',CROP) .GT. 0) THEN
            WRITE (NOUTDO, 907) P1,RATPOT,LFMAX
            WRITE (NOUTDO,1007) G1,PI1,PI2,DTTPI
          ELSEIF (INDEX ('SU',CROP) .GT. 0) THEN
            WRITE (NOUTDO, 908) P1,P2,P5
            WRITE (NOUTDO,1008) G2,G3,O1
          ELSEIF (INDEX ('PI',CROP) .GT. 0) THEN
            WRITE (NOUTDO, 909) P2,P3,P4
            WRITE (NOUTDO,1009) G2,G3,PHINT
          ELSEIF (INDEX ('TNTR',CROP) .GT. 0) THEN
            WRITE (NOUTDO, 911) P1,P3,P4,P5
            WRITE (NOUTDO,1011) G3,G4,PHINT
          ELSEIF (INDEX ('CO',CROP) .GT. 0) THEN
            WRITE (NOUTDO, 912) SCPB,RESPC,SQCON
            WRITE (NOUTDO,1012) FCUT,FLAI,DDISQ
         ENDIF
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

  100 FORMAT (1X,'Please press < ENTER > key to continue ',$)
  300 FORMAT ('*SUMMARY OF SOIL AND GENETIC INPUT PARAMETERS',/)
  350 FORMAT (
     &    4X,'SOIL LOWER UPPER   SAT  EXTR  INIT   ROOT   BULK',
     &    5X,'pH    NO3    NH4    ORG',/,
     &    3X,'DEPTH LIMIT LIMIT    SW    SW    SW   DIST   DENS',26X,
     &    'C',/,
     &    4X,'cm',3X,3('cm3/cm3',4X),5X,'g/cm3',9X,'ugN/g  ugN/g',
     &    5X,'%',/,80('-'))
  400 FORMAT (1X,I3,'-',I3,5(1X,F5.3),6(1X,F6.2))
  600 FORMAT (/,1X,'TOT-',I3,5F6.1,2X,'<--cm   -','  kg/ha-->',2F7.1,I7)
  700 FORMAT (1X,'SOIL ALBEDO    :',F5.2,7X,'EVAPORATION LIMIT :',F5.2,
     &        8X,'MIN. FACTOR  :',F5.2,/,1X,'RUNOFF CURVE # :',F5.2,
     &        7X,'DRAINAGE RATE     :',F5.2,8X,'FERT. FACTOR :',F5.2,/)
  800 FORMAT (1X,A10,1X,'CULTIVAR :',A6,'-',A16,3X,'ECOTYPE :',
     &        A6,'-',A16)
  900 FORMAT (1X,'CSDVAR :',F5.2,'  PPSEN  :',F5.2,
     &         '  EMG-FLW:',F5.2,'  FLW-FSD:',F5.2,'  FSD-PHM :',F5.2)
 1000 FORMAT (1X,'WTPSD  :',F5.3,'  SDPDVR :',F5.2,
     &         '  SDFDUR :',F5.2,'  PODDUR :',F5.2,'  XFRUIT  :',F5.2,/)
 1010 FORMAT (1X,'WTPSD  :',F5.3,'  SDPDVR :',F5.1,
     &         '  SDFDUR :',F5.2,'  PODDUR :',F5.2,'  XFRUIT  :',F5.2,/)
  901 FORMAT (1X,'P1     :',F7.2,'  P2     :',F7.4,
     &         '  P5     :',F7.2)
  902 FORMAT (1X,'P1     :',F5.1,'  P2O    :',F5.2,
     &         '  P2R    :',F6.2,'  P5     :',F6.2)
  903 FORMAT (1X,'P1     :',F6.2,'  P2O    :',F6.3,
     &         '  P2R    :',F6.2,'  P5     :',F6.2)
  904 FORMAT (1X,'P1V    :',F8.6,'  P1D    :',F8.6,
     &         '  P5     :',F8.2)
  905 FORMAT (1X,'G2     :',F7.2,'  G3     :',F7.4,'  G4     :',F7.2)
  906 FORMAT (1X,'P1     :',F6.1,'  P2R    :',F6.1,
     &         '  P5     :',F6.1,'  P2O    :',F6.1)
  907 FORMAT (1X,'P1     :',F6.1,'  RATPOT :',F6.1,
     &         '  LFMAX  :',F6.1)
  908 FORMAT (1X,'P1     :',F7.2,'  P2     :',F7.4,
     &         '  P5     :',F7.2)
  909 FORMAT (1X,'P2     :',F6.1,'  P3     :',F6.1,
     &         '  P4     :',F6.0)
  911 FORMAT (1X,'P1     :',F7.1,' P3     :',F7.2,
     &          ' P4     :',F7.1,' P5     :',F7.2)
  912 FORMAT (1X,'SCPB   :',F7.1,' RESPC  :',F7.3,
     &          ' SQCON  :',F7.3)
 1001 FORMAT (1X,'G2     :',F7.2,'  G3     :',F7.3,'  PHINT  :',F7.3)
 1002 FORMAT (1X,'G1     :',F5.1,'  G2     :',F5.2,'  PHINT  :',F6.2)
 1003 FORMAT (1X,'G1     :',F6.2,'  G4     :',F6.2,'  PHINT  :',F6.2)
 1004 FORMAT (1X,'G1     :',F8.3,'  G2     :',F8.3,'  G3     :',F8.3,
     &         '  PHINT  :',F8.3)
 1005 FORMAT (1X,'PD     :',F7.2,'  P2     :',F7.3,'  TC     :',F7.3)
 1006 FORMAT (1X,'G1     :',F6.1,'  G2     :',F6.4,
     &         '  G3     :',F6.2,'  G4     :',F6.2)
 1007 FORMAT (1X,'G1     :',F6.1,'  PI1    :',F6.1,
     &         '  PI2    :',F6.1,'  DTTPI  :',F6.1)
 1008 FORMAT (1X,'G2     :',F7.2,'  G3     :',F7.3,'  O1     :',F4.0)
 1009 FORMAT (1X,'G2     :',F6.1,'  G3     :',F6.2,'  PHINT  :',F6.1)
 1011 FORMAT (1X,'G3     :',F7.1,' G4     :',F7.1,
     &          ' PHINT  :',F7.1,' PCINT  :',F7.1,' PCGRD  :',F7.1)
 1012 FORMAT (1X,'FCUT   :',F7.3,' FLAI   :',F7.2,
     &          ' DDISQ  :',F7.1)
 2000 FORMAT (1X,'DUB1   :',F6.1,'  DUBR   :',F6.1,'  DESP   :',F6.2,
     &         '  PHCX   :',F6.2,'  S#PE   :',F6.1,/,
     &        1X,'S#FX   :',F6.1,'  S#PX   :',F6.1,'  SWNX   :',F6.1,
     &         '  L#IS   :',F6.2,'  L#IP   :',F6.2,/,
     &        1X,'LALX   :',F6.0,'  LAXA   :',F6.2,'  LAL3   :',F6.0,
     &         '  LAWS   :',F6.0,'  LFLI   :',F6.0)

      END
