C=======================================================================
C  IRRIG, Subroutine
C
C  Determines when irrigation occurs
C-----------------------------------------------------------------------
C  REVISION HISTORY
C
C  09/01/88 B.B   Restructured from WATBAL
C  02/07/93 P.W.W Header revision and minor changes
C  02/25/94 B.D.B Added Fixed amount auto irrigation
C  04/19/94 B.D.B Added NAP calc. to auto irrigations
C  09/19/95 G.H.  Modified to match GRO code
C  07/11/96 G.H.  Separated irrigation and precipitation
C  03/23/97 JWJ   Added P and W options
C-----------------------------------------------------------------------
C  INPUT  :  IIRRI,YRDOY,RAIN,AMIR,JULAPL,NAP,EFFIRR,YRPLT,MDATE,ATHETA,
C            THETAC,SWDEF,TOTIR,PRECIP,AIRAMT,NAPW,DAP
C
C  LOCAL  : J
C
C  OUTPUT : TOTIR
C-----------------------------------------------------------------------
C  Called : WATBAL
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  AIRAMT : Amount of irrigation applied, if fixed, for automatic irr. (mm)
C  DEPIR  : Depth of irrigation (cm)
C  IOFF   :
C  J      : Loop counter
C  NAP    : Number of irrigation applications
C  TOTIR  : Total irrigation (mm)
C  YRDOY  : Day of year
C=======================================================================

      SUBROUTINE IRRIG(IIRRI,YRDOY,AMIR,JULAPL,NAP,EFFIRR,YRPLT,MDATE,
     &        ATHETA,THETAC,SWDEF,TOTIR,IRRAMT,AIRAMT,NAPW,DAP)

      IMPLICIT NONE

      CHARACTER*1 IIRRI

      INTEGER JULAPL(150),NAP,I,NAPW
      INTEGER YRDOY,YRPLT,MDATE,DAP

      REAL DEPIR,SWDEF,IRRAMT,TOTIR,AMIR(150)
      REAL THETAC,ATHETA,EFFIRR,AIRAMT

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      DEPIR  = 0.
C-----------------------------------------------------------------------
C
C** IIRRI = R-As Reported in FIELD SCHEDULE. If day of irrigation then.
C
C-----------------------------------------------------------------------
      IF (IIRRI .EQ. 'R') THEN
        IF (NAPW .GT. 0) THEN
           DO I = 1, NAPW
             IF (JULAPL(I) .EQ. YRDOY) THEN
                DEPIR = AMIR(I)
                GO TO 10
             ELSE IF (JULAPL(I) .GT. YRDOY) THEN
                RETURN
             ENDIF
           END DO
 10        CONTINUE
           IF (DEPIR .LE. 0.01) RETURN
         ELSE
           !
           ! Return if IIRRI = 'R' and NAPW = 0
           !
           RETURN
        ENDIF

C-----------------------------------------------------------------------
C
C** IIRRI = D-As Reported in FIELD SCHEDULE, days after planting
C
C-----------------------------------------------------------------------
      ELSE IF (IIRRI .EQ. 'D' .AND. NAPW .GT. 0) THEN
        DO I = 1, NAPW
          IF (JULAPL(I) .EQ. DAP) THEN
             DEPIR = AMIR(I)
             GO TO 11
          ELSE IF (JULAPL(I) .GT. DAP) THEN
             RETURN
          ENDIF
        END DO
 11     CONTINUE
        IF (DEPIR .LE. 0.01) RETURN
C-----------------------------------------------------------------------
C
C** IIRRI = A-Automatic irrigation or F-Fixed Amount Automatic Irrigation
C
C-----------------------------------------------------------------------
      ELSE IF (IIRRI .EQ. 'A' .OR. IIRRI .EQ. 'F') THEN
        IF (MDATE .NE. -99 .AND. YRDOY .GE. MDATE .OR.
     &      YRDOY .LT. YRPLT) RETURN
        IF (ATHETA .LE. THETAC*0.01) THEN
            DEPIR = SWDEF*10 + 5.0
        ELSE
            DEPIR = 0.0
            RETURN
        ENDIF
C-----------------------------------------------------------------------
C    Compensate for expected water loss due to soil evaporation
C    and transpiration today.
C    Estimate that an average of 5 mm of water will be lost.
C-----------------------------------------------------------------------
        IF (IIRRI .EQ. 'A') THEN
C-----------------------------------------------------------------------
C*** DETERMINE SUPPLIMENTAL IRRIGATION AMOUNT
C-----------------------------------------------------------------------
          DEPIR = MAX(0.,DEPIR)
        ELSE IF (IIRRI .EQ. 'F') THEN
C-----------------------------------------------------------------------
C*** APPLY FIXED IRRIGATION AMOUNT
C-----------------------------------------------------------------------
          DEPIR = AIRAMT
        ENDIF
        JULAPL(NAP+1) = YRDOY
        AMIR(NAP+1)   = DEPIR
      ENDIF

C-----------------------------------------------------------------------
C
C** IIRRI =P-As Reported through last reported day, then automatic
C          to re-fill profile (as in option A)
C   IIRRI = W-AS Reported through last reported day, then automatic
C**         adding AIRAMT each time
C-----------------------------------------------------------------------
      IF (IIRRI .EQ. 'P' .OR. IIRRI .EQ. 'W') THEN
        IF (NAPW .GT. 0) THEN
           DO I = 1, NAPW
             IF (JULAPL(I) .EQ. YRDOY) THEN
                DEPIR = AMIR(I)
                GO TO 20
             ENDIF
           END DO
 20        CONTINUE
         ELSE
           !
           ! Return if IIRRI = 'P' OR 'W', and NAPW = 0
           !
           RETURN
        ENDIF

C-----------------------------------------------------------------------
C       If Today's date is after the last record in the IRRIG section
c           check to see if automatic irrigation is needed (P or W option)
C-----------------------------------------------------------------------

      IF(YRDOY .GT. JULAPL(NAPW))THEN

        IF (MDATE .NE. -99 .AND. YRDOY .GE. MDATE .OR.
     &      YRDOY .LT. YRPLT) RETURN
        IF (ATHETA .LE. THETAC*0.01) THEN
C-----------------------------------------------------------------------
C    Compensate for expected water loss due to soil evaporation
C    and transpiration today.
C    Estimate that an average of 5 mm of water will be lost.
C-----------------------------------------------------------------------
            DEPIR = SWDEF*10 + 5.0
        ELSE
            DEPIR = 0.0
            RETURN
        ENDIF

          IF(IIRRI .EQ. 'P') THEN
C-----------------------------------------------------------------------
C*** DETERMINE SUPPLIMENTAL IRRIGATION AMOUNT
C-----------------------------------------------------------------------
          DEPIR = MAX(0.,DEPIR)
        ELSE IF (IIRRI .EQ. 'W') THEN
C-----------------------------------------------------------------------
C*** APPLY FIXED IRRIGATION AMOUNT
C-----------------------------------------------------------------------
          DEPIR = AIRAMT
        ENDIF
        JULAPL(NAP+1) = YRDOY
        AMIR(NAP+1)   = DEPIR
      ENDIF
      ENDIF

C-----------------------------------------------------------------------
C
C    *********    IRRIGATE     **********
C
C-----------------------------------------------------------------------
      IF (EFFIRR .GT. 0.0) THEN
        IRRAMT = DEPIR*EFFIRR
      ELSE
        IRRAMT = DEPIR
      ENDIF
      IF (DEPIR .GT. 0.00) THEN
         NAP    = NAP + 1
         TOTIR  = TOTIR + DEPIR
      ENDIF
      RETURN
      END

