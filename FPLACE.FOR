C=======================================================================
C  FPLACE, Subroutine
C
C  Determines fertilizer placement (restructured from NTRANS)
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified for version 3 input/output (matches LeGRO), WTB  8 AUG 92
C  3. Header revision and minor changes             P. Wilkens  2-18-93
C-----------------------------------------------------------------------
C  INPUT  : YRDOY,IFERI,NAPNIT,AMTNIT,FTYPEN,DSOILN,SOILNC,SOILNX,DOY,
C           DLAYR,NLAYR
C
C  LOCAL  : K,M,IDLAYR,KMAX,YRDNIT,L,PROF,DEPTH,CD,FERNIT,FERDEP
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : NTRANS
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE FPLACE (YRDOY,IFERI,NAPNIT,AMTNIT,FTYPEN,DSOILN,SOILNC,
     &                   SOILNX,DOY,DLAYR,NLAYR,DAP)

      IMPLICIT  NONE

      INCLUDE  'NTRC1.BLK'
      INCLUDE  'NTRC2.BLK'

      CHARACTER IFERI*1

      INTEGER   I,K,M,IDLAYR,KMAX,YRDOY,NAPNIT,FTYPEN,YRDNIT,L,NLAYR,DOY
      INTEGER   DAP
      REAL      PROF(20),DLAYR(20),DEPTH,CD
      REAL      FERNIT,FERDEP,AMTNIT,SOILNC,SOILNX,DSOILN

      FERNIT = 0.0

      IF (NAPNIT .EQ. 0) THEN
          YRDNIT = 0
      ENDIF

      IF (NFERT .GT. 0 .AND. IFERI .EQ. 'R') THEN
         DO I = 1, NFERT
            IF (YRDOY .EQ. FDAY(I)) THEN
               NAPNIT = NAPNIT + 1
               AMTNIT = AMTNIT + ANFER(I)
               FERNIT = ANFER(I)
               FERDEP = DFERT(I)
               M = IFTYPE(I)
               IF (M .EQ. 17) THEN
                  M = 12
               ENDIF
               GO TO 100                             ! Was EXIT
             ELSE IF (FDAY(I) .GT. YRDOY) THEN
               GO TO 100                             ! Was EXIT
            ENDIF
         END DO
       ELSE IF (NFERT .GT. 0 .AND. IFERI .EQ. 'D') THEN
         DO I = 1, NFERT
            IF (DAP .EQ. FDAY(I)) THEN
               NAPNIT  = NAPNIT + 1
               AMTNIT  = AMTNIT + ANFER(I)
               FERNIT  = ANFER(I)
               FERDEP  = DFERT(I)
               M = IFTYPE(I)
               IF (M .EQ. 17) THEN
                  M = 12
               ENDIF
               GO TO 100                             ! Was EXIT
             ELSE IF (FDAY(I) .GT. DAP) THEN
               GO TO 100                             ! Was EXIT
            ENDIF
         END DO
       ELSE IF (IFERI .EQ. 'A') THEN
         IF ((1.0 - NSTRES) * 100.0 .GT. SOILNC .AND.
     &      YRDOY .GT. (YRDNIT+1)) THEN
            NAPNIT = NAPNIT + 1
            AMTNIT = AMTNIT + SOILNX
            FERNIT = SOILNX
            FERDEP = DSOILN
            YRDNIT = YRDOY
            M      = FTYPEN
            IF (M .EQ. 17) THEN
                M = 12
            ENDIF
         ENDIF
      ENDIF

  100 CONTINUE                                       ! Sun Fix

      IF (FERNIT .GT. 0.0) THEN
         DO L = 1, NLAYR
            PROF(L) = 0.0
         END DO
         DEPTH = 0.0
         KMAX  = 1
         KMAX  = IDLAYR (NLAYR,DLAYR,FERDEP)
         CD    = 0.0

         IF (KMAX .EQ. 1) THEN
            PROF(1) = 1.0
C
C           Surface layer incorporation
C
          ELSE
            CD      = DLAYR(1)
            PROF(1) = DLAYR(1)/FERDEP
            DO L = 2, KMAX
               CD = CD + DLAYR(L)
               IF (FERDEP .LE. CD) THEN
                   PROF(L) = (FERDEP-(CD-DLAYR(L)))/FERDEP
                 ELSE
                   PROF(L) = DLAYR(L)/FERDEP
               ENDIF
            END DO
         ENDIF
C
C        ษอออออออออออออออออออออออออออออออออออออออออออออออออป
C        บ    Fertilizer types as given in appendix 4,     บ
C        บ    Technical Report 1,IBSNAT (1986).            บ
C        บ                                                 บ
C        บ      1   = Ammonium Nitrate                     บ
C        บ      2   = Ammonium Sulphate                    บ
C        บ      3   = Ammonium Nitrate-Sulphate            บ
C        บ      4   = Anhydrous Ammonia                    บ
C        บ      5   = Urea (Hydrolysis to be added later)  บ
C        บ      6   = Diammonium Phosphate                 บ
C        บ      7   = Monoammonium Phosphate               บ
C        บ      8   = Calcium Nitrate                      บ
C        บ      9   = Aqua Ammonia                         บ
C        บ     10   = Urea Ammonium Nitrate                บ
C        บ     11   = Calcium Ammonium Nitrate             บ
C        บ     17   = Potassium Nitrate                    บ
C        บ                                                 บ
C        ศอออออออออออออออออออออออออออออออออออออออออออออออออผ
C
         IF (M .EQ. 5) THEN
             DO K = 1, KMAX
                UREA(K) = UREA(K) + FERNIT*PROF(K)
             END DO
             IUON = .TRUE.
             IUOF = DOY + 21
             IF (DOY .GT. 344) THEN
                IUOF = 21 - (365-DOY)
             ENDIF
          ELSEIF (M .EQ. 2 .OR. M .EQ. 4 .OR. M .EQ. 6 .OR.
     &            M .EQ. 7 .OR. M .EQ. 9 .OR. M .EQ. 11) THEN
             DO K = 1, KMAX
               SNH4(K) = SNH4(K) + FERNIT*PROF(K)
             END DO
          ELSEIF (M .EQ. 1 .OR. M .EQ. 3) THEN
             DO K = 1, KMAX
                SNH4(K) = SNH4(K) + 0.5*FERNIT*PROF(K)
                SNO3(K) = SNO3(K) + 0.5*FERNIT*PROF(K)
             END DO
             RETURN
          ELSEIF (M .EQ. 8 .OR. M .EQ. 12) THEN
             DO K = 1, KMAX
                SNO3(K) = SNO3(K) + FERNIT*PROF(K)
             END DO
          ELSEIF (M .EQ. 10) THEN
             DO K = 1, KMAX
                SNO3(K) = SNO3(K) + FERNIT*0.25*PROF(K)
                SNH4(K) = SNH4(K) + FERNIT*0.25*PROF(K)
                UREA(K) = UREA(K) + FERNIT*0.50*PROF(K)
             END DO
             IUON = .TRUE.
             IUOF = DOY + 21
             IF (DOY .GT. 344) THEN
                IUOF = 21 - (365-DOY)
             ENDIF
         ENDIF
      ENDIF

      RETURN

      END
C=======================================================================
C  IDLAYR, Function
C
C  Determines layer where fertilizer is placed
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      2-8-93
C-----------------------------------------------------------------------
C  INPUT  : NLAYR FLAYR FDEPTH
C
C  LOCAL  : L
C
C  OUTPUT : IDLAYR
C-----------------------------------------------------------------------
C  Called : NTRANS
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  NLAYR  : Number of layers in soil
C  DLAYR(): Depth increment of soil layer L - cm
C  L      : Loop counter
C  FDEPTH : Fertilizer depth (cm)
C  DEPTH  : Depth to the bottom of a layer from the surface (cm)
C=======================================================================

      INTEGER FUNCTION IDLAYR (NLAYR,DLAYR,FDEPTH)

      IMPLICIT  NONE

      INTEGER   NLAYR,L
      DIMENSION DLAYR (NLAYR)

      REAL      FDEPTH,DLAYR,DEPTH

      DEPTH  = 0.0
      IDLAYR = 1
      DO L = 1, NLAYR
         DEPTH  = DEPTH + DLAYR (L)
         IF (FDEPTH .LE. DEPTH) THEN
            IDLAYR = L
            RETURN
         ENDIF
      END DO

      IDLAYR = NLAYR

      RETURN

      END
