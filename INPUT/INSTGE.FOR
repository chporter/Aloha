C=======================================================================
C  INSTGE, Subroutine
C
C  Initializes phenological stages
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                         G.H.      3-28-93
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : CROP
C
C  LOCAL  : I
C
C  OUTPUT : STNAME
C-----------------------------------------------------------------------
C  Called : SEHARV
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE INSTGE (STNAME,CROP)

      IMPLICIT     NONE

      CHARACTER*2  CROP
      CHARACTER*10 STNAME(20)

      INTEGER      I

C-----------------------------------------------------------------------
C     Define names of reproductive phases
C-----------------------------------------------------------------------

      DO I = 1, 20
         STNAME(I) = '          '
      END DO

      IF (CROP .EQ. 'BA') THEN
         STNAME( 1) = 'Max Prim  '
         STNAME( 2) = 'End Veg   '
         STNAME( 3) = 'End Ear Gr'
         STNAME( 4) = 'Ear Emerg '
         STNAME( 5) = 'End Gr Fil'
         STNAME( 6) = 'Maturity  '
         STNAME( 7) = 'Sowing    '
         STNAME( 8) = 'Germinate '
         STNAME( 9) = 'Emergence '
       ELSE IF (CROP .EQ. 'BN') THEN
         STNAME( 1) = 'Emergence '
         STNAME( 2) = 'Unifoliate'
         STNAME( 3) = 'End Juven.'
         STNAME( 4) = 'Flower Ind'
         STNAME( 5) = 'Flowering '
         STNAME( 6) = 'First Pod '
         STNAME( 7) = 'Full Pod  '
         STNAME( 8) = 'First Seed'
         STNAME( 9) = 'End Pod   '
         STNAME(10) = 'Phys. Mat '
         STNAME(11) = 'Harv. Mat '
         STNAME(12) = 'End Msnode'
         STNAME(13) = 'End Leaf  '
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'Sowing    '
         STNAME(16) = 'Harvest   '
       ELSE IF (CROP .EQ. 'CH') THEN
         STNAME( 1) = 'Emergence '
         STNAME( 2) = 'Unifoliate'
         STNAME( 3) = 'End Juven.'
         STNAME( 4) = 'Flower Ind'
         STNAME( 5) = 'Flowering '
         STNAME( 6) = 'First Pod '
         STNAME( 7) = 'Full Pod  '
         STNAME( 8) = 'First Seed'
         STNAME( 9) = 'End Pod   '
         STNAME(10) = 'Phys. Mat '
         STNAME(11) = 'Harv. Mat '
         STNAME(12) = 'End Msnode'
         STNAME(13) = 'End Leaf  '
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'Sowing    '
         STNAME(16) = 'Harvest   '
       ELSE IF (INDEX ('C3C4G0G1G2G3G4G5G6G7G8G9',CROP) .GT. 0) THEN
         STNAME( 1) = 'Emergence '
         STNAME( 2) = 'First Leaf'
         STNAME( 3) = 'End Juven.'
         STNAME( 4) = 'Flower Ind'
         STNAME( 5) = 'Flowering '
         STNAME(10) = 'Phys. Mat '
         STNAME(11) = 'Harv. Mat '
         STNAME(12) = 'End Msnode'
         STNAME(13) = 'End Leaf  '
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'Sowing    '
         STNAME(16) = 'Harvest   '
       ELSE IF (CROP .EQ. 'FA') THEN
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'End Sim   '
       ELSE IF (CROP .EQ. 'ML') THEN
         STNAME( 1) = 'End Juveni'
         STNAME( 2) = 'Floral Ini'
         STNAME( 3) = 'End Lf Gro'
         STNAME( 4) = 'Anthesis  '
         STNAME( 5) = 'End Tlr Fl'
         STNAME( 6) = 'Maturity  '
         STNAME( 7) = 'Sowing    '
         STNAME( 8) = 'Germinate '
         STNAME( 9) = 'Emergence '
       ELSE IF (CROP .EQ. 'MZ') THEN
         STNAME( 1) = 'End Juveni'
         STNAME( 2) = 'Floral Ini'
         STNAME( 3) = '75% Silkin'
         STNAME( 4) = 'Beg Gr Fil'
         STNAME( 5) = 'End Gr Fil'
         STNAME( 6) = 'Maturity  '
         STNAME( 7) = 'Sowing    '
         STNAME( 8) = 'Germinate '
         STNAME( 9) = 'Emergence '
       ELSE IF (CROP .EQ. 'PE') THEN
         STNAME( 1) = 'Emergence '
         STNAME( 2) = 'Unifoliate'
         STNAME( 3) = 'End Juven.'
         STNAME( 4) = 'Flower Ind'
         STNAME( 5) = 'Flowering '
         STNAME( 6) = 'First Pod '
         STNAME( 7) = 'Full Pod  '
         STNAME( 8) = 'First Seed'
         STNAME( 9) = 'End Pod   '
         STNAME(10) = 'Phys. Mat '
         STNAME(11) = 'Harv. Mat '
         STNAME(12) = 'End Msnode'
         STNAME(13) = 'End Leaf  '
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'Sowing    '
         STNAME(16) = 'Harvest   '
       ELSE IF (CROP .EQ. 'PI') THEN
         STNAME( 1) = 'Zero Stem '
         STNAME( 2) = 'Forcing   '
         STNAME( 3) = 'SCY       '
         STNAME( 4) = 'Early Flwr'
         STNAME( 5) = 'Fruit Harv'
         STNAME( 6) = 'Maturity  '
         STNAME( 7) = 'Planting  '
         STNAME( 8) = 'Root Init '
         STNAME( 9) = 'Leaf Emerg'
         STNAME(10) = '          '
         STNAME(11) = '          '
         STNAME(12) = '          '
         STNAME(13) = '          '
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'End Sim   '
         STNAME(16) = '          '
         STNAME(17) = '          '
         STNAME(18) = '          '
         STNAME(19) = '          '
         STNAME(20) = 'Harvest   '
       ELSE IF (CROP .EQ. 'PN') THEN
         STNAME( 1) = 'Emergence '
         STNAME( 2) = 'Unifoliate'
         STNAME( 3) = 'End Juven.'
         STNAME( 4) = 'Flower Ind'
         STNAME( 5) = 'Flowering '
         STNAME( 6) = 'First Pod '
         STNAME( 7) = 'Full Pod  '
         STNAME( 8) = 'First Seed'
         STNAME( 9) = 'End Pod   '
         STNAME(10) = 'Phys. Mat '
         STNAME(11) = 'Harv. Mat '
         STNAME(12) = 'End Msnode'
         STNAME(13) = 'End Leaf  '
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'Sowing    '
         STNAME(16) = 'Harvest   '
       ELSE IF (CROP .EQ. 'PP') THEN
         STNAME( 1) = 'Emergence '
         STNAME( 2) = 'Unifoliate'
         STNAME( 3) = 'End Juven.'
         STNAME( 4) = 'Flower Ind'
         STNAME( 5) = 'Flowering '
         STNAME( 6) = 'First Pod '
         STNAME( 7) = 'Full Pod  '
         STNAME( 8) = 'First Seed'
         STNAME( 9) = 'End Pod   '
         STNAME(10) = 'Phys. Mat '
         STNAME(11) = 'Harv. Mat '
         STNAME(12) = 'End Msnode'
         STNAME(13) = 'End Leaf  '
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'Sowing    '
         STNAME(16) = 'Harvest   '
       ELSE IF (CROP .EQ. 'PR') THEN
         STNAME( 1) = 'Emergence '
         STNAME( 2) = 'Unifoliate'
         STNAME( 3) = 'End Juven.'
         STNAME( 4) = 'Flower Ind'
         STNAME( 5) = 'Flowering '
         STNAME( 6) = 'First Pod '
         STNAME( 7) = 'Full Pod  '
         STNAME( 8) = 'First Seed'
         STNAME( 9) = 'End Pod   '
         STNAME(10) = 'Phys. Mat '
         STNAME(11) = 'Harv. Mat '
         STNAME(12) = 'End Msnode'
         STNAME(13) = 'End Leaf  '
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'Sowing    '
         STNAME(16) = 'Harvest   '
       ELSE IF (CROP .EQ. 'SB') THEN
         STNAME( 1) = 'Emergence '
         STNAME( 2) = 'Unifoliate'
         STNAME( 3) = 'End Juven.'
         STNAME( 4) = 'Flower Ind'
         STNAME( 5) = 'Flowering '
         STNAME( 6) = 'First Pod '
         STNAME( 7) = 'Full Pod  '
         STNAME( 8) = 'First Seed'
         STNAME( 9) = 'End Pod   '
         STNAME(10) = 'Phys. Mat '
         STNAME(11) = 'Harv. Mat '
         STNAME(12) = 'End Msnode'
         STNAME(13) = 'End Leaf  '
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'Sowing    '
         STNAME(16) = 'Harvest   '
       ELSE IF (CROP .EQ. 'SC') THEN
         STNAME( 1) = '14th Leaf '
         STNAME( 2) = 'Stalk Beg.'
         STNAME( 3) = 'Full Canop'
         STNAME( 4) = 'Peak Pop  '
         STNAME( 5) = 'Stable Pop'
         STNAME( 6) = 'Maturity  '
         STNAME( 7) = 'Sowing    '
         STNAME( 8) = 'Germinate '
         STNAME( 9) = 'Emergence '
       ELSE IF (CROP .EQ. 'SG') THEN
         STNAME( 1) = 'End Juveni'
         STNAME( 2) = 'Floral Ini'
         STNAME( 3) = 'End Lf Gro'
         STNAME( 4) = 'End Pan Gr'
         STNAME( 5) = 'End Mn Fil'
         STNAME( 6) = 'Maturity  '
         STNAME( 7) = 'Sowing    '
         STNAME( 8) = 'Germinate '
         STNAME( 9) = 'Emergence '
       ELSE IF (CROP .EQ. 'TM') THEN
         STNAME( 1) = 'Emergence '
         STNAME( 2) = 'Unifoliate'
         STNAME( 3) = 'End Juven.'
         STNAME( 4) = 'Flower Ind'
         STNAME( 5) = 'Flowering '
         STNAME( 6) = 'First Pod '
         STNAME( 7) = 'Full Pod  '
         STNAME( 8) = 'First Seed'
         STNAME( 9) = 'End Pod   '
         STNAME(10) = 'Phys. Mat '
         STNAME(11) = 'Harv. Mat '
         STNAME(12) = 'End Msnode'
         STNAME(13) = 'End Leaf  '
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'Sowing    '
         STNAME(16) = 'Harvest   '
       ELSE IF (CROP .EQ. 'VB') THEN
         STNAME( 1) = 'Emergence '
         STNAME( 2) = 'Unifoliate'
         STNAME( 3) = 'End Juven.'
         STNAME( 4) = 'Flower Ind'
         STNAME( 5) = 'Flowering '
         STNAME( 6) = 'First Pod '
         STNAME( 7) = 'Full Pod  '
         STNAME( 8) = 'First Seed'
         STNAME( 9) = 'End Pod   '
         STNAME(10) = 'Phys. Mat '
         STNAME(11) = 'Harv. Mat '
         STNAME(12) = 'End Msnode'
         STNAME(13) = 'End Leaf  '
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'Sowing    '
         STNAME(16) = 'Harvest   '
       ELSE IF (CROP .EQ. 'WH') THEN
         STNAME( 1) = 'Term Spklt'
         STNAME( 2) = 'End Veg   '
         STNAME( 3) = 'End Ear Gr'
         STNAME( 4) = 'Ear Emerg '
         STNAME( 5) = 'End Gr Fil'
         STNAME( 6) = 'Maturity  '
         STNAME( 7) = 'Sowing    '
         STNAME( 8) = 'Germinate '
         STNAME( 9) = 'Emergence '
      ELSEIF (CROP .EQ. 'CS') THEN
         STNAME( 1) = '1st Branch'
         STNAME( 2) = '2nd Branch'
         STNAME( 3) = '3rd Branch'
         STNAME( 4) = '4th Branch'
         STNAME( 5) = '5th Branch'
         STNAME( 6) = '6th Branch'
         STNAME( 7) = '7th Branch'
         STNAME( 8) = '8th Branch'
         STNAME( 9) = 'Maturity  '
         STNAME(10) = 'Sowing    '
         STNAME(11) = 'Germinate '
         STNAME(12) = 'Emergence '
         STNAME(13) = '          '
         STNAME(14) = 'Start Sim '
       ELSEIF (CROP .EQ. 'RI') THEN
         STNAME( 1) = 'End Juveni'
         STNAME( 2) = 'Pan Init  '
         STNAME( 3) = 'Heading   '
         STNAME( 4) = 'Beg Gr Fil'
         STNAME( 5) = 'End Mn Fil'
         STNAME( 6) = 'Maturity  '
         STNAME( 7) = 'Sowing    '
         STNAME( 8) = 'Germinate '
         STNAME( 9) = 'Emergence '
         STNAME(10) = 'Prgerm Sow'
         STNAME(11) = 'Transplant'
         STNAME(12) = 'End Ti Fil'
         STNAME(13) = 'Start Sim '
         STNAME(14) = 'Harvest   '
       ELSE IF (CROP .EQ. 'SU') THEN
         STNAME( 1) = 'End Juveni'
         STNAME( 2) = 'Floral Ini'
         STNAME( 3) = 'First Anth'
         STNAME( 4) = 'Beg Gr Fil'
         STNAME( 5) = 'End Gr Fil'
         STNAME( 6) = 'Maturity  '
         STNAME( 7) = 'Sowing    '
         STNAME( 8) = 'Germinate '
         STNAME( 9) = 'Emergence '
      ENDIF

      RETURN

      END
