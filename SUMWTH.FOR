C=======================================================================
C  SUMWTH, Subroutine
C
C  This subroutine generates a summary output for the weather
C  variables as a function of the most critical growth phases
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      2-28-93
C-----------------------------------------------------------------------
C  INPUT  : ISTAGE,YRDOY,YRSIM,HAREND
C
C  LOCAL  :
C
C  OUTPUT : TMAX,TMIN,RAIN,DAYL,SRAD,ET,TURFAC,SWFAC,NSTRES,AGEFAC
C-----------------------------------------------------------------------
C  Called : PINE
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  ISTAGE : Phenological stage
C  TMAX   :
C  TMIN   :
C  RAIN   : Precipitation - mm/day
C  DAYL   : Day length
C  SRAD   : Solar radiation - LY/day
C  ET     : Actual soil and plant evaporation - mm/day
C  TURFAC :
C  SWFAC  :
C  YRDOY  : Year day of year
C  YRSIM  :
C  HAREND :
C  NSTRES :
C  AGEFAC :
C=======================================================================

      SUBROUTINE SUMWTH (ISTAGE,TMAX,TMIN,RAIN,DAYL,SRAD,ET,TURFAC,
     &                   SWFAC,YRDOY,YRSIM,HAREND,NSTRES,AGEFAC)

      IMPLICIT NONE

      INCLUDE 'COWTH.BLK'

      INTEGER  I,ISTAGE,YRDOY,YRSIM,HAREND,ISTAG
      REAL     TMAX,TMIN,RAIN,DAYL,SRAD,ET,TURFAC,SWFAC,NSTRES,AGEFAC

      IF (YRDOY .EQ. YRSIM) THEN
         DO I = 0, 20
            TMAXR(I) = 0.0
            TMINR(I) = 0.0
            RAINR(I) = 0.0
            DAYLR(I) = 0.0
            RADR (I) = 0.0
            RADTR(I) = 0.0
            CETR (I) = 0.0
            TURFR(I) = 0.0
            SWFCR(I) = 0.0
            NSTRR(I) = 0.0
            AGEFR(I) = 0.0
            NNR  (I) = 0
         END DO
      END IF
C
C     Calculate the total weather summary variables
C
C     First zero out the variables at the beginning of the season
C
      IF (YRDOY .GT. YRSIM) THEN
         TMAXR(0) = TMAX   + TMAXR(0)
         TMINR(0) = TMIN   + TMINR(0)
         RAINR(0) = RAIN   + RAINR(0)
         DAYLR(0) = DAYL   + DAYLR(0)
         RADR(0)  = SRAD   + RADR(0)
         CETR(0)  = ET     + CETR(0)
         TURFR(0) = TURFAC + TURFR(0)
         SWFCR(0) = SWFAC  + SWFCR(0)
         NSTRR(0) = NSTRES + NSTRR(0)
         AGEFR(0) = AGEFAC + AGEFR(0)
         NNR(0)   = NNR(0) + 1
      END IF

      TMAXR(ISTAGE) = TMAX   + TMAXR(ISTAGE)
      TMINR(ISTAGE) = TMIN   + TMINR(ISTAGE)
      RAINR(ISTAGE) = RAIN   + RAINR(ISTAGE)
      DAYLR(ISTAGE) = DAYL   + DAYLR(ISTAGE)
      RADR (ISTAGE) = SRAD   + RADR(ISTAGE)
      CETR (ISTAGE) = ET     + CETR(ISTAGE)
      TURFR(ISTAGE) = TURFAC + TURFR(ISTAGE)
      SWFCR(ISTAGE) = SWFAC  + SWFCR(ISTAGE)
      NSTRR(ISTAGE) = NSTRES + NSTRR(ISTAGE)
      AGEFR(ISTAGE) = AGEFAC + AGEFR(ISTAGE)
      NNR(ISTAGE)   = NNR(ISTAGE) + 1

      IF (YRDOY .EQ. HAREND) THEN
         IF (NNR(0) .EQ. 0) THEN
            TMAXR(0) = 0.0
            TMINR(0) = 0.0
            DAYLR(0) = 0.0
            RADTR(0) = 0.0
            RADR(0)  = 0.0
            TURFR(0) = 0.0
            SWFCR(0) = 0.0
            NSTRR(0) = 0.0
            AGEFR(0) = 0.0
          ELSE
            TMAXR(0) = TMAXR(0) / NNR(0)
            TMINR(0) = TMINR(0) / NNR(0)
            DAYLR(0) = DAYLR(0) / NNR(0)
            RADTR(0) = RADR(0)
            RADR(0)  = RADR(0)  / NNR(0)
            TURFR(0) = 1.0 - (TURFR(0) / NNR(0))
            SWFCR(0) = 1.0 - (SWFCR(0) / NNR(0))
            NSTRR(0) = 1.0 - (NSTRR(0) / NNR(0))
            AGEFR(0) = 1.0 - (AGEFR(0) / NNR(0))
         ENDIF
         DO ISTAG = 1, 20
            IF (NNR(ISTAG) .EQ. 0) THEN
               TMAXR(ISTAG) = 0.0
               TMINR(ISTAG) = 0.0
               DAYLR(ISTAG) = 0.0
               RADR(ISTAG)  = 0.0
               RADTR(ISTAG) = 0.0
               TURFR(ISTAG) = 0.0
               SWFCR(ISTAG) = 0.0
               NSTRR(ISTAG) = 0.0
               AGEFR(ISTAG) = 0.0
             ELSE
               TMAXR(ISTAG) = TMAXR(ISTAG) / NNR(ISTAG)
               TMINR(ISTAG) = TMINR(ISTAG) / NNR(ISTAG)
               DAYLR(ISTAG) = DAYLR(ISTAG) / NNR(ISTAG)
               RADTR(ISTAG) = RADR(ISTAG)
               RADR(ISTAG)  = RADR(ISTAG)  / NNR(ISTAG)
               TURFR(ISTAG) = 1.0 - (TURFR(ISTAG) / NNR(ISTAG))
               SWFCR(ISTAG) = 1.0 - (SWFCR(ISTAG) / NNR(ISTAG))
               NSTRR(ISTAG) = 1.0 - (NSTRR(ISTAG) / NNR(ISTAG))
               AGEFR(ISTAG) = 1.0 - (AGEFR(ISTAG) / NNR(ISTAG))
            ENDIF
         END DO
      ENDIF

      RETURN                     
      END
