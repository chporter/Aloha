C=======================================================================
C  OPOPS, Subroutine, J. W. Jones
C  Creates field operations output file
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  12/27/93 J. Hansen
C  03/29/95 JWH Modified for new format
C  04/05/96 GH  Added tillage and chemical applications
C  07/09/96 GH  Modified harvest section
C-----------------------------------------------------------------------
C
C   Header description:
C
C     DATE:  Date of operation, year & day of year
C     CDAY:  Days since planting
C     OPTY:  Operation type:
C            PLNT = Planting
C            IRRI = Irrigation
C            FERT = Fertilizer application
C            RESD = Organic material application
C            CHEM = Pesticide application (not available to crop model)
C            TILL = Tillage (not available to crop model)
C            HARV = Harvest
C     OPOP:  Management option (from Management subsection of
C            SIMULATION CONTROL)
C     OPME:  Method code
C     RAMT:  Amount of material resource produced
C     RUNT:  Units for material resource.
C     RNAM:  Type of material resource involved (none for tillage)
C     CC:    Crop code
C     CLTV:  Cultivar code
C-----------------------------------------------------------------------
      SUBROUTINE OPOPS(YRPLT, NRESAP, RESDAY, NAPNIT, FDAY, NAP,
     &    JULAPL,HAREND,PLME,SDRATE, RESIDUE, RESCOD, ANFER, IFTYPE,
     &    AMIR,YIELD,TOPWT,WTLF,CROP,VARNO,IPLTI,IRESI,IFERI,
     &    IIRRI,IHARI,NOUTDR,HPC,HBPC,IAME,HCOM,
     &    FERCOD,TDATE,NTLAP,ISWTIL,TIMPL,RMET,CHMET,CHAMT,
     &    ISWCHE,CHCOD,NPSTAP,CDATE,IRRCOD)

      IMPLICIT NONE

      CHARACTER*1 PLME, IPLTI, IRESI, IFERI, IIRRI, IHARI, HCOM(3)
      CHARACTER*1 ISWTIL,ISWCHE
      CHARACTER*2 CROP
      CHARACTER*5 FERCOD(25), RESCOD(10),IAME,TIMPL(10)
      CHARACTER*5 RMET(10),CHCOD(10),CHMET(10)
      CHARACTER*6 VARNO

      INTEGER FDAY(25), RESDAY(10), YRPLT, NRESAP, NAPNIT, NAP
      INTEGER JULAPL(150), IFTYPE(25), NOUTDR, HAREND
      INTEGER TDATE(10),NTLAP,CDATE(10),NPSTAP,IRRCOD(150)
      INTEGER OPLIST(190), NOLIST(190), DTLIST(190), GAP, SIZE, I, J, K
      INTEGER INCDAT

      REAL AMIR(150), ANFER(25), RESIDUE(10), HPC(3), SDRATE, YIELD,
     &    TOPWT, WTLF, HBPC(3),CHAMT(10)

      EXTERNAL INCDAT

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C Load dates, types, and numbers of all operations into arrays.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C Planting
C-----------------------------------------------------------------------
      IF (YRPLT .GE. 0) THEN
         DTLIST(1) = YRPLT
         OPLIST(1) = 1
         NOLIST(1) = 1
         J = 1
      ELSE
         J = 0
      ENDIF

C-----------------------------------------------------------------------
C Residue application
C-----------------------------------------------------------------------
      DO I = 1, NRESAP
        IF (IRESI .EQ. 'D') THEN
          DTLIST(I+J) = INCDAT(YRPLT, RESDAY(I))
        ELSE
          DTLIST(I+J) = RESDAY(I)
        END IF
        OPLIST(I+J) = 2
        NOLIST(I+J) = I
      END DO
      J = J + NRESAP

C-----------------------------------------------------------------------
C Fertilizer application
C-----------------------------------------------------------------------
      DO I = 1, NAPNIT
        IF (IFERI .EQ. 'D') THEN
          DTLIST(I+J) = INCDAT(YRPLT, FDAY(I))
        ELSE
          DTLIST(I+J) = FDAY(I)
        END IF
        OPLIST(I+J) = 3
        NOLIST(I+J) = I
      END DO
      J = J + NAPNIT

C-----------------------------------------------------------------------
C Irrigation application
C-----------------------------------------------------------------------
      DO I = 1, NAP
        IF (IIRRI .EQ. 'D') THEN
          DTLIST(I+J) = INCDAT(YRPLT, JULAPL(I))
        ELSE
          DTLIST(I+J) = JULAPL(I)
        END IF
        OPLIST(I+J) = 4
        NOLIST(I+J) = I
      END DO
      J = J + NAP

C-----------------------------------------------------------------------
C Tillage application
C-----------------------------------------------------------------------
      DO I = 1, NTLAP
        IF (ISWTIL .EQ. 'D') THEN
          DTLIST(I+J) = INCDAT(YRPLT, TDATE(I))
        ELSE
          DTLIST(I+J) = TDATE(I)
        END IF
        OPLIST(I+J) = 5
        NOLIST(I+J) = I
      END DO
      J = J + NTLAP

C-----------------------------------------------------------------------
C Chemical applications
C-----------------------------------------------------------------------
      DO I = 1, NPSTAP
        IF (ISWCHE .EQ. 'D') THEN
          DTLIST(I+J) = INCDAT(YRPLT, CDATE(I))
        ELSE
          DTLIST(I+J) = CDATE(I)
        END IF
        OPLIST(I+J) = 6
        NOLIST(I+J) = I
      END DO
      J = J + NPSTAP

C-----------------------------------------------------------------------
C Product harvest
C-----------------------------------------------------------------------
      J = J + 1
      DTLIST(J) = HAREND
      OPLIST(J) = 7
      NOLIST(J) = 1

C-----------------------------------------------------------------------
C  Shell sort arrays.
C----------------------------------------------------------------------
      SIZE = J
      GAP = SIZE / 2
      DO WHILE (GAP .GT. 0)
        DO I = GAP + 1, SIZE
          J = I - GAP
          DO WHILE (J .GT. 0)
            K = J + GAP
            IF ((DTLIST(J) .GT. DTLIST(K)) .OR. ((DTLIST(J) .EQ.
     &          DTLIST(K)) .AND. (OPLIST(J) .GT. OPLIST(K)))) THEN
              CALL SWAP(DTLIST(J), DTLIST(K), OPLIST(J), OPLIST(K),
     &            NOLIST(J), NOLIST(K))
              J = J - GAP
            ELSE
              J = 0
            END IF
          END DO
        END DO
        GAP = GAP / 2
      END DO
      WRITE(NOUTDR, 100)
C-----------------------------------------------------------------------
C Output sorted operations.
C-----------------------------------------------------------------------
      DO I = 1, Size
        CALL OPOP(DTLIST(I), OPLIST(I), NOLIST(I), YRPLT, PLME,
     &      SDRATE, RESIDUE, RESCOD, ANFER, IFTYPE, AMIR, YIELD,
     &      TOPWT, WTLF, CROP, VARNO, IPLTI, IRESI, IFERI, IIRRI,
     &      IHARI,NOUTDR,HPC,HBPC,IAME,HCOM,FERCOD,
     &      TIMPL,ISWTIL,RMET,ISWCHE,CHCOD,CHMET,CHAMT,IRRCOD)
      END DO
      RETURN

 100  FORMAT(/,
     &    '! Timing....  Operation.......  Material resource ',
     &    'use...........', /, '@ DATE  CDAY  OPTY  OPOP  OPME     ',
     &    'RAMT  RUNT  RNAM    CC CLTV')
      END

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------

      SUBROUTINE SWAP(A, B, C, D, E, F)

      IMPLICIT NONE

      INTEGER A, B, C, D, E, F, I

      I = A
      A = B
      B = I
      I = C
      C = D
      D = I
      I = E
      E = F
      F = I
      RETURN
      END

C-----------------------------------------------------------------------
C  Similar to INCYD without the restriction that DELTA <= 365.
C-----------------------------------------------------------------------

      INTEGER FUNCTION INCDAT(ADATE, DELTA)

      IMPLICIT NONE
      INTEGER NDYR, AYR, ADOY, ADATE, DELTA, ENDYR, YDOY
      EXTERNAL ENDYR, YDOY

      CALL YR_DOY(ADATE, AYR, ADOY)
      NDYR = ENDYR(AYR)
      ADOY = ADOY + DELTA
  100 CONTINUE
      IF (ADOY .GT. NDYR) THEN
        AYR = AYR + 1
c        IF (AYR .EQ. 100) THEN
c     &      AYR = 0
        ADOY = ADOY - NDYR
        GO TO 100
      END IF
  200 IF (ADOY .LE. 0) THEN
        AYR = AYR - 1
c        IF (AYR .EQ. -1) THEN
c     &      AYR = 99
        NDYR = ENDYR(AYR)
        ADOY = ADOY + NDYR
        GO TO 200
      END IF
      INCDAT = YDOY(AYR, ADOY)
      END

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------

      SUBROUTINE OPOP(DATE, TYPE, NUMBER, YRPLT, PLME, SDRATE,
     &    RESIDUE, RESCOD, ANFER, IFTYPE, AMIR, YIELD, TOPWT, WTLF,
     &    CROP, VARNO, IPLTI, IRESI, IFERI, IIRRI, IHARI, NOUTDR,
     &    HPC, HBPC,IAME, HCOM, FERCOD,TIMPL,
     &    ISWTIL,RMET,ISWCHE,CHCOD,CHMET,CHAMT,IRRCOD)
C-----------------------------------------------------------------------
C     Writes a single operation to the field operations output file.
C     J. Hansen 12/27/93
C
C    Modified for new format.  JWH 3/29/1995
C-----------------------------------------------------------------------
      IMPLICIT NONE

      REAL AMIR(150),ANFER(25),RESIDUE(10),SDRATE,YIELD,
     &    TOPWT,WTLF,MULT,HPC(3),HBPC(3),CHAMT(10)

      INTEGER IFTYPE(25), DATE, CDAY, TYPE, NUMBER, TIMDIF, YRPLT,
     &    NOUTDR,IRRCOD(150)

      CHARACTER*1 PLME, IPLTI, IRESI, IFERI, IIRRI, IHARI, HCOM(3)
      CHARACTER*1 ISWTIL,ISWCHE
      CHARACTER*2 CROP
      CHARACTER*5 FERCOD(25), RESCOD(10), IAME, RNAM
      CHARACTER*5 RMET(10),CHMET(10)
      CHARACTER*5 TIMPL(10),CHCOD(10)
      CHARACTER*6 VARNO

      CDAY = TIMDIF(YRPLT, DATE)

      IF (TYPE .EQ. 1) THEN
C    Planting
        IF (SDRATE .GT. 0) THEN

c What variable to use for planting material & method?

          IF (PLME .EQ. 'T') THEN
            RNAM = 'PM002'
          ELSE IF (PLME .EQ. 'N') THEN
            RNAM = 'PM003'
          ELSE IF (PLME .EQ. 'P') THEN
            RNAM = 'PM004'
          ELSE
            RNAM = 'PM001'
          END IF
          WRITE(NOUTDR, 100) DATE, CDAY, 'PLNT', IPLTI, RNAM, -SDRATE,
     &        'kg/ha', RNAM, CROP, VARNO
        END IF

      ELSE IF (TYPE .EQ. 2) THEN
C    Residue application
        IF (RESIDUE(NUMBER) .GT. 0)
     &      WRITE(NOUTDR, 100) DATE, CDAY, 'RESD', IRESI,RMET(NUMBER),
     &      -RESIDUE(NUMBER), 'kg/ha', RESCOD(NUMBER), CROP, VARNO

      ELSE IF (TYPE .EQ. 3) THEN
C    Fertilizer application
        IF (ANFER(NUMBER) .GT. 0) THEN
          WRITE(RNAM, 101) IFTYPE(NUMBER)
          WRITE(NOUTDR, 100) DATE, CDAY, 'FERT', IFERI, FERCOD(NUMBER),
     &        -ANFER(NUMBER), 'kg/ha', RNAM, CROP, VARNO
        END IF

      ELSE IF (TYPE .EQ. 4) THEN
C    Irrigation
        IF (AMIR(NUMBER) .GT. 0) THEN
          IF (INDEX('AF', IIRRI) .GT. 0) THEN
            WRITE(NOUTDR, 100) DATE, CDAY,'IRRI',IIRRI,IAME,
     &          -AMIR(NUMBER), 'mm', 'WATER', CROP, VARNO
          ELSE
            WRITE(RNAM, 103) IRRCOD(NUMBER)
            WRITE(NOUTDR, 100) DATE, CDAY,'IRRI',IIRRI,RNAM,
     &          -AMIR(NUMBER), 'mm', 'WATER', CROP, VARNO
          END IF
        END IF

      ELSE IF (TYPE .EQ. 5) THEN
C    Tillage
C       IF (ANFER(NUMBER) .GT. 0) THEN
          WRITE(NOUTDR, 102) DATE, CDAY, 'TILL',ISWTIL,TIMPL(NUMBER),
     &              CROP, VARNO
c       END IF

      ELSE IF (TYPE .EQ. 6) THEN
C    Chemical application
        IF (CHAMT(NUMBER) .GT. 0) THEN
          WRITE(NOUTDR, 100) DATE, CDAY, 'CHEM', ISWCHE,CHMET(NUMBER),
     &        -CHAMT(NUMBER), 'kg/ha',CHCOD(NUMBER), CROP, VARNO
        END IF


      ELSE IF (TYPE .EQ. 7) THEN
C    Harvest
        IF (HPC(1) .GE. 0.0) THEN
          MULT = HPC(1) / 10.0
        ELSE
          MULT = 10.0
        END IF
        IF (TOPWT .GT. 0) THEN
          IF (INDEX('RD', IHARI) .GT. 0) THEN
c  'H' = 'HA001' = Harvest product; 'C' = 'HA002' = Canopy;
c  'L' = 'HA003' = Leaves
            IF ((HCOM(1) .EQ. 'H') .AND. (YIELD .GT. 0)) THEN
              WRITE(NOUTDR, 100) DATE, CDAY, 'HARV', IHARI, 'HA001',
     &            YIELD*MULT, 'kg/ha', 'HA001', CROP, VARNO
            ELSE IF (HCOM(1) .EQ. 'C') THEN
              WRITE(NOUTDR, 100) DATE, CDAY, 'HARV', IHARI, 'HA002',
     &            (TOPWT-YIELD)*MULT, 'kg/ha', 'HA002', CROP, VARNO
            ELSE IF ((HCOM(1) .EQ. 'L') .AND. (WTLF .GT. 0)) THEN
              WRITE(NOUTDR, 100) DATE, CDAY, 'HARV', IHARI, 'HA003',
     &            WTLF*MULT, 'kg/ha', 'HA003', CROP, VARNO
            ELSE
              IF (YIELD .GE. 0.0) THEN
                  WRITE(NOUTDR, 100) DATE, CDAY, 'HARV', IHARI,
     &            'HA001', YIELD*MULT, 'kg/ha', 'HA001', CROP, VARNO
              ENDIF
              IF (HBPC(1) .GT. 0.) THEN
              WRITE(NOUTDR, 100) DATE, CDAY, 'HARV', IHARI, 'HA002',
     &            (TOPWT-YIELD)*HBPC(1)/10., 'kg/ha', 'HA002', CROP,
     &            VARNO
              ENDIF
            END IF
          ELSE
            IF (YIELD .GE. 0.0) THEN
               WRITE(NOUTDR, 100) DATE, CDAY, 'HARV', IHARI, 'HA001',
     &         YIELD*MULT, 'kg/ha', 'HA001', CROP, VARNO
            ENDIF
            IF (HBPC(1) .GT. 0.) THEN
               WRITE(NOUTDR, 100) DATE, CDAY, 'HARV', IHARI, 'HA002',
     &         (TOPWT-YIELD)*HBPC(1)/10., 'kg/ha', 'HA002', CROP,
     &         VARNO
            ENDIF
          END IF
        END IF
      END IF

      RETURN

 100  FORMAT(I6, 1X, I5, 2X, A4, 5X, A1, 1X, A5, F9.1, 2(1X, A5), 4X,
     &    A2, 1X, A6)
 101  FORMAT('FE', I3.3)
 102  FORMAT(I6, 1X, I5, 2X, A4, 5X, A1, 1X, A5, 25X,
     &    A2, 1X, A6)
 103  FORMAT('IR', I3.3)
      END

