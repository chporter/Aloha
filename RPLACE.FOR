C=======================================================================
C  RPLACE, Subroutine
C
C  Determines residue placement
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                     P.W.W      11-21-95
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  :
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : NTRANS
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  PROF   : proportion of soil destined fertilizer
C=======================================================================

      SUBROUTINE RPLACE (YRDOY,DAP,NLAYR,DLAYR,IRESI,NAPRES)

      IMPLICIT  NONE

      INCLUDE  'NTRC1.BLK'
      INCLUDE  'NTRC2.BLK'

      CHARACTER IRESI*1,METRES*5
      INTEGER   I,DAP,NLAYR,NAPRES,IOUT,L,YRDOY
      REAL      RESAPP,RESNIT,RESDEP,DLAYR(20),HOLD,DEPTH
      REAL      FR,ADD

C     ษอออออออออออออออออออออออออออออออออออออออป
C     บ        Residue types                  บ
C     บ                                       บ
C     บ    1 = Crop residue                   บ
C     บ    2 = Green manure                   บ
C     บ    3 = Barnyard manure                บ
C     บ    4 = Liquid manure                  บ
C     บ                                       บ
C     บ        Residue Application            บ
C     บ                                       บ
C     บ    A = Auto residue for multiyear     บ
C     บ    N = No residue                     บ
C     บ    R = On reported dates              บ
C     บ    D = As reported, in DAP            บ
C     บ    F = Auto, with fixed amounts       บ
C     บ                                       บ
C     ศอออออออออออออออออออออออออออออออออออออออผ

      RESNIT = 0.0
      RESAPP = 0.0

      IF (NRESAP .GT. 0 .AND. IRESI .EQ. 'R') THEN
         DO I = 1, NRESAP
            IF (YRDOY .EQ. RESDAY(I)) THEN
               NAPRES = NAPRES + 1
               AMTRES = AMTRES + RESIDUE(I) * RINP(I) / 100.
               RESAPP = RESIDUE(I) * RINP(I) / 100.
               RESNIT = RESN(I)
               RESDEP = DEPRES(I)
               METRES = RESCOD(I)
               GO TO 100                             ! Was EXIT
             ELSE IF (RESDAY(I) .GT. YRDOY) THEN
               GO TO 100                             ! Was EXIT
            ENDIF
         END DO
       ELSE IF (NRESAP .GT. 0 .AND. IRESI .EQ. 'D') THEN
         DO I = 1, NRESAP
            IF (DAP .EQ. RESDAY(I)) THEN
               NAPRES = NAPRES + 1
               AMTRES = AMTRES + RESIDUE(I)
               RESAPP = RESIDUE(I)
               RESNIT = RESN(I)
               RESDEP = DEPRES(I)
               METRES = RESCOD(I)
               GO TO 100                             ! Was EXIT
             ELSE IF (RESDAY(I) .GT. DAP) THEN
               GO TO 100                             ! Was EXIT
            ENDIF
         END DO
       ELSE IF (IRESI .EQ. 'A') THEN
         IF (DAP .EQ. NRESDL) THEN
            NAPRES = NAPRES + 1
            AMTRES = AMTRES + RESIDUE(1)
            RESAPP = RESIDUE(1)
            RESNIT = RESN(1)
            RESDEP = DRESMG
            METRES = RESCOD(1)
            GO TO 100                                ! Was EXIT
         ELSE IF (NRESDL .GT. DAP) THEN
            GO TO 100                                ! Was EXIT
         ENDIF
      ENDIF

  100 CONTINUE                                       ! Sun Fix

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
              GOTO 200
           ENDIF
        END DO

  200   CONTINUE

        !
        ! Call IPSOIL and get the indicated parameters for this
        ! residue type, including REDCR(1,3) and the pool sizes
        ! for CHO, cellulose, and lignin
        !
        CALL IPSOIL (METRES,DMINR,RCN,DSNC,RDCHO,RDCEL,RDLIG,PRCHO,
     &               PRCEL,PRLIG)

        DO L = 1, NLAYR
           FPOOL(L,1) = FOM(L) * PRCHO               ! 0.20
           FPOOL(L,2) = FOM(L) * PRCEL               ! 0.70
           FPOOL(L,3) = FOM(L) * PRLIG               ! 0.10
        END DO
        RETURN
      ELSE
        RETURN                                       ! No residue applied
      ENDIF

      RETURN
      END

