C=======================================================================
C  NFLUX, Subroutine
C
C  Determines nitrogen drainage and leaching
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.     02-10-93
C  4. Modified for CERES and CROPGRO                G.H.       10-18-95
C-----------------------------------------------------------------------
C  INPUT  : IFLAG,ISFLAG,SKGN,FAC
C
C  LOCAL  : KPFLG,L,J,K,XMIN,OUTN
C
C  OUTPUT : SPPM
C-----------------------------------------------------------------------
C  Called : WATBAL
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  IFLAG  : Flag for N source -- 1 for urea, 2 for NO3
C  ISFLAG :
C  KPFLG  :
C  L,J,K  : Loop counters
C  XMIN   :
C  SPPM   :
C  SKGN   :
C  FAC    :
C  OUTN   : Nitrate N leaching from a layer (kg N/ha)
C=======================================================================

      SUBROUTINE NFLUX (IFLAG,ISFLAG,SKGN,SPPM,FAC,MU,DRN,FLOW,TLCH,
     &                  NLAYR,DLAYR,SW,BD,DUL,ADCOEF)

      IMPLICIT  NONE
C
C     Added flag ISFLAG to determine whether UREA or NO3.
C     Set to 1 if UREA, to 2 if NO3.
C

      INTEGER IFLAG,ISFLAG,KPFLG,L,J,K,MU,NLAYR
      REAL    SKGN(20),SPPM(20),FAC(20),DLAYR(20),SW(20)
      REAL    DRN(20),FLOW(20),BD(20),DUL(20),ADCOEF(20)
      REAL    TLCH,OUTN,XMIN,NISFAC

      IF (IFLAG .EQ. 1) GO TO 300
      KPFLG = 0

90    OUTN  = 0.0

      DO L = 1, NLAYR
         SKGN(L) = SKGN(L) + OUTN
         SPPM(L) = SKGN(L) * FAC(L)
         XMIN    = 0.25/FAC(L)
         IF (ISFLAG .EQ. 1) THEN
            XMIN = 0.0
         ENDIF
         !
         ! NISFAC is an intermediate variable used in calculating
         ! reduced nitrate flow in variably charged soils.  ADCOEF is
         ! the array used in calculation (0.0 = no change in flow)
         !
         NISFAC  = 1.0 / (1.0 + ((BD(L) * ADCOEF(L)) / DUL (L)))
         OUTN    = MAX(((SKGN(L)-XMIN)*NISFAC),0.0)*DRN(L) /
     &            (SW(L)*DLAYR(L)+DRN(L))
         IF (KPFLG .EQ. 1) THEN
            OUTN = OUTN * 0.5
         ENDIF
         SKGN(L) = SKGN(L) - OUTN
         SPPM(L) = SKGN(L) * FAC(L)
         IF (L .EQ. NLAYR) THEN
            TLCH = TLCH + OUTN
         ENDIF
      END DO

      RETURN
  300 CONTINUE

      OUTN  = 0.0
      KPFLG = 0
      DO J = 1, MU
         K       = MU + 1 - J
         SKGN(K) = SKGN(K) + OUTN
         IF (K .NE. 1) THEN
            IF (FLOW(K-1) .LT. 0.0) THEN
               OUTN    = 0.0
               KPFLG   = 1
             ELSE
               XMIN    = 0.25/FAC(K)
               IF (ISFLAG .EQ. 1) THEN
                  XMIN = 0.0
               ENDIF
               OUTN    =MAX((SKGN(K)-XMIN),0.0)*(FLOW(K-1)/
     +                  (SW(K)*DLAYR(K)+FLOW(K)))*0.5
               SKGN(K) = SKGN(K) - OUTN
            ENDIF
         ENDIF
      END DO

      IF (KPFLG .NE. 0) THEN
         OUTN = 0.0
         DO L = 1, NLAYR
            DRN(L) = 0.0
            IF (FLOW(L) .LT. 0.0) THEN
                DRN(L) = FLOW(L)*(-1.0)
            ENDIF
         END DO
         GO TO 90
      ENDIF

      RETURN

      END
