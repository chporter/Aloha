C=======================================================================
C  LMATCH, Subroutine
C
C  Converts input soil layer data into fixed output soil layers
C  Created by J. W. Jones to create fixed increments for soil
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                 J. W. Jones       5- 7-91
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : NLAYRI,DI,VI,DLAYR
C
C  LOCAL  : J,K,L,VS,ZIL,ZOL,SUMZ,SUMV,ZT,ZB
C
C  OUTPUT : DS,VS
C-----------------------------------------------------------------------
C  Called : IPSOIL IPSLIN
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE LMATCH (NLAYRI,DLAYR,VI,NLAYRO,DS)

      IMPLICIT NONE

      INTEGER  J,K,L,NLAYRI,NLAYRO

      REAL     VI(20),DS(20),VS(20),DLAYR(20)
      REAL     ZIL,ZOL,SUMZ,SUMV,ZT,ZB

      IF (NLAYRI .LT. 1) RETURN

C*----------------------------------------------------------------------
C     Definition of layers and soil depths moved to IPEXP
C*----------------------------------------------------------------------

C
C     This subroutine assumes that DLAYR(L) values are depths to the
C     bottom of layer L
C
      DO L = 1, 20
         VS(L) = 0.0
      END DO

      K   = 1
      ZIL = 0.0
      ZOL = 0.0

      DO L = 1, 20
         SUMZ = 0.0
         SUMV = 0.0

         DO WHILE (.TRUE.)
            ZT   = MAX (ZOL,ZIL)
            ZB   = MIN (DS(L),DLAYR(K))
            SUMZ = SUMZ + (ZB - ZT)
            SUMV = SUMV + VI(K)*(ZB - ZT)
            IF (DS(L) .LT. DLAYR(K)) GOTO 10
C
C           Either establish last layer or go to next input layer
C
            IF (K .EQ. NLAYRI) GOTO 20
C
C           Go to next input layer to add characteristics
C
            ZIL = DLAYR(K)
            K   = K + 1
            IF (K .GT. 20) THEN
               WRITE (*,15) K
  15           FORMAT(' More than 20 layers in soil profile : ',I3,/,
     &                ' Please fix soil profile.')
            STOP
            ENDIF
         END DO

10       VS(L) = VI(K)
         IF (SUMZ .GT. 0.0) THEN
            VS(L) = SUMV/SUMZ
         ENDIF
         ZOL = DS(L)
      END DO

      RETURN
C
C     Set last layer characteristics, and depth of profile
C
20    VS(L) = VI(K)
      IF (SUMZ .GT. 0.0) THEN
          VS(L) = SUMV/SUMZ
      ENDIF
      NLAYRO = L
      DO J = 1, NLAYRO
         VI(J) = VS(J)
      END DO

      RETURN

      END

C=======================================================================
C  LYRSET, Subroutine
C
C  Converts input soil layer data into fixed output soil layers
C  Created by J. W. Jones to create fixed increments for soil
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                 J. W. Jones       5- 7-91
C  2  Modified by                                   G.H.
C  3. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : NLAYRI,DI,ZLAYR,DS
C
C  LOCAL  : J,K,L,ZIL,ZOL,DI
C
C  OUTPUT : NLAYRO,DS,VS
C-----------------------------------------------------------------------
C  Called : IPSOIL
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE LYRSET (NLAYRI,ZLAYR,NLAYRO,DS,DLAYR,DEPMAX)

      IMPLICIT NONE

      INTEGER  J,K,L,NLAYRI,NLAYRO

      REAL     DI(20),DS(20),ZLAYR(20),DLAYR(20)
      REAL     ZIL,ZOL,DEPMAX

C*----------------------------------------------------------------------
C     Definition of layers and soil depths moved to IPEXP
C*----------------------------------------------------------------------

C
C     This subroutine assumes that DI(L) values are depths to the
C     bottom of layer L
C
      DO L = 1, 20
         DI(L) = ZLAYR(L)
      END DO

      K = 1

      DO L = 1, 20
         DO WHILE (.TRUE.)
            IF (DS(L) .LT. DI(K)) GOTO 10
C
C           Either establish last layer or go to next input layer
C
            IF (K .EQ. NLAYRI) GOTO 20
C
C           Go to next input layer to add characteristics
C
            ZIL = DI(K)
            K   = K + 1
            IF (K .GT. 20) THEN
                WRITE (*,*) 'K = ',K
                pause
            ENDIF
         END DO
 10      CONTINUE
         ZOL = DS(L)
      END DO
C
C     Set last layer characteristics, and depth of profile
C
 20   CONTINUE
      DS(L)    = DI(K)
      NLAYRO   = L
      DLAYR(1) = DS(1)

      IF (NLAYRO .GT. 1) THEN
         DO J = 2, NLAYRO
            DLAYR(J) = DS(J) - DS(J - 1)
         END DO
         IF (DLAYR (NLAYRO) .LT. DLAYR(NLAYRO - 1) .AND.
     &       DLAYR (NLAYRO) .LT. 15.0) THEN
             DLAYR (NLAYRO)   = (DLAYR(NLAYRO) + DLAYR(NLAYRO-1))/2
             DLAYR (NLAYRO-1) =  DLAYR(NLAYRO)
             DS (NLAYRO-1)    = DS (NLAYRO-2)  + DLAYR(NLAYRO-1)
         ENDIF
      ENDIF

      DEPMAX = DS(NLAYRO)
      RETURN

      END
