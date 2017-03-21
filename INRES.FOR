C=======================================================================
C  INRES, Subroutine
C
C  Determines placement of initial organic residue
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                           P.W.W.  1-20-96
C  2. Modified to handle initial residue application    GH      4-01-96
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  :
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : CROPGRO
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C         :
C=======================================================================

      SUBROUTINE INRES(YRDOY,YRIC,FPOOL,FON,FOM,ICRES,ICRIP,
     &           ICREN,ICRID,DLAYR,NLAYR,TIFOM,TIFON,PRCHO,PRCEL,PRLIG,
     &           DMINR,RCN,DSNC,RDCHO,RDCEL,RDLIG)
      IMPLICIT  NONE

      INTEGER   I,YRDOY,YRIC,IOUT,NLAYR

      REAL      FPOOL(20,3),FON(20),FOM(20)
      REAL      RESAPP,RESNIT,RESDEP,DLAYR(20),HOLD,DEPTH
      REAL      FR,ADD,ICRES,ICRIP,ICREN,ICRID,TIFOM,TIFON
      REAL      PRCHO,PRCEL,PRLIG
      REAL      DMINR,RCN,DSNC,RDCHO,RDCEL,RDLIG

      CHARACTER METRES*5

      IF (YRDOY .EQ. YRIC) THEN
         RESAPP = ICRES*ICRIP/100.
         RESNIT = ICREN
         RESDEP = ICRID
         METRES = 'RE001'

         IF (RESAPP .GT. 0.0) THEN
           !
           ! Calculate N contributions
           !
           DEPTH = 0.0
           IOUT  = 1

           DO I = 1, NLAYR
              HOLD  = DEPTH
              DEPTH = DEPTH + DLAYR(I)
              IF (RESDEP .LE. DEPTH) THEN
                 IF (RESDEP .GT. 0) THEN
                    FR = (RESDEP-HOLD)/RESDEP
                 ENDIF
                 IF (I .EQ. 1) THEN
                    FR = 1
                 ENDIF
                 IOUT = 2
               ELSE
                 IF (RESDEP .GT. 0.0) THEN
                    FR = DLAYR(I)/RESDEP
                 ENDIF
              ENDIF
              ADD    = RESAPP*FR
              FOM(I) = FOM(I) + ADD
              FON(I) = FON(I) + ADD * RESNIT/100.
              IF (IOUT .EQ. 2) THEN
                 GOTO 100
              ENDIF
           END DO

  100      CONTINUE

           TIFOM = 0.0
           TIFON = 0.0
           DO I = 1, NLAYR
              TIFOM  = TIFOM + FOM(I)
              TIFON  = TIFON + FON(I)
           END DO
           !
           ! Call IPSOIL and get the indicated parameters for this
           ! residue type, including REDCR(1,3) and the pool sizes
           ! for CHO, cellulose, and lignin
           !
           CALL IPSOIL (METRES,DMINR,RCN,DSNC,RDCHO,RDCEL,RDLIG,PRCHO,
     &                  PRCEL,PRLIG)

           DO I = 1, NLAYR
              FPOOL(I,1) = FOM(I) * PRCHO            ! 0.20
              FPOOL(I,2) = FOM(I) * PRCEL            ! 0.70
              FPOOL(I,3) = FOM(I) * PRLIG            ! 0.10
           END DO
         ENDIF
      ENDIF

      RETURN
      END
