C=======================================================================
C  SOILNI, Subroutine
C
C  Determines soil N initilization
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/08/93 PWW Header revision and minor changes
C  02/20/96 GH  Written
C  02/26/98 WTB Fixed HUM/NHUM calculations
C
C-----------------------------------------------------------------------
C  INPUT  : AINO3,AINH4,TSIN,TSOC
C
C  LOCAL  : SCN,I,IOUT,L,WRN,RNKG,WSUM,DEPTH,FACTOR,HOLD,
C           FR,ADD,TBD
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : CROPGRO, CERES
C
C  Calls  : SOLT
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  AINO3  :
C  AINH4  :
C  TSIN   :
C  TSOC   :
C  I,L    :
C  IOUT   :
C  WRN    :
C  RNKG   :
C  WSUM   :
C  DEPTH  :
C  FACTOR :
C  HOLD   :
C  FR     :
C  ADD    :
C=======================================================================

      SUBROUTINE SOILNI(AINO3,AINH4,TSIN,DMOD,WRESR,NLAYR,DLAYR,
     &                  DEPMAX,SW,LL,DUL,SAT,UREA,BD,PH,FON,FOM,
     &                  HUM,FAC,NO3,NH4,SNO3,SNH4,OC,NHUM,FPOOL,
     &                  CNI,WFY,TFY,ST,PHN,TIFON,TIFOM,TSON,TSOC,
     &                  ANO3,ANH4,RCN,PRCHO,PRCEL,PRLIG,TOTN,CNRAT)

      IMPLICIT  NONE

      INTEGER   I,L,NLAYR

      LOGICAL   IUON

      REAL      WRN(20),RNKG,WSUM,DEPTH,FACTOR
      REAL      AINO3,AINH4,TSIN,DMOD,WRESR,DEPMAX
      REAL      DLAYR(20),SW(20),LL(20),DUL(20),SAT(20)
      REAL      UREA(20),BD(20),PH(20),FON(20),FOM(20),HUM(20)
      REAL      FAC(20),NO3(20),NH4(20),SNO3(20),SNH4(20),OC(20)
      REAL      NHUM(20),FPOOL(20,3),CNI(20),WFY(20),TFY(20),ST(20)
      REAL      PHN(20),TIFON,TIFOM,TSON,TSOC,ANO3,ANH4
      REAL      RCN,PRCHO,PRCEL,PRLIG,TOTN(20),CNRAT(20)

      IF (DMOD .EQ. 0.0) THEN
          DMOD = 1.0
      ENDIF

      IUON   = .FALSE.
      AINO3  = 0.0
      AINH4  = 0.0
      TSIN   = 0.0

      DO I = 1, NLAYR
         UREA(I) = 0.0
         IF (BD(I) .EQ. 0.0) THEN
             BD(I) = 1.2
         ENDIF
         IF (PH(I) .LE. 1.0 .OR. PH(I) .GT. 10.0) THEN
             PH(I) = 7.0
         ENDIF
      END DO
C
C     Calculate N contributions
C     RCN is now read in from SOIL.PAR .. was set to 40.0
C
      RNKG = WRESR*0.40/RCN
C
C     Distribute WRESR mass
C
      WSUM  = 0.0
      DEPTH = 0.0

      DO I = 1, NLAYR
         DEPTH  = DEPTH + DLAYR (I)
         WRN(I) = EXP(-3.0*DEPTH/DEPMAX)
         WSUM   = WSUM + WRN(I)
      END DO

      DO I = 1, NLAYR
         FACTOR = WRN(I) / WSUM
         FOM(I) = WRESR  * FACTOR
         FON(I) = RNKG   * FACTOR
      END DO

      TIFOM = 0.0
      TIFON = 0.0
      TSON  = 0.0

      DO I = 1, NLAYR
         !
         ! HUM(I) = OC(I) * 1.E03*BD(I)*DLAYR(I)/0.58
         ! HUM is kg C / ha (0.58 was used before to convert to OM) - WTB
         !
         ! Calculate based on OC content
         !
         HUM(I) = OC(I) * 1000. * BD(I) * DLAYR(I)
         TIFOM  = TIFOM + FOM(I)
         TIFON  = TIFON + FON(I)
         TSOC   = TSOC  + HUM(I)
      END DO


      DO L = 1, NLAYR
         FAC(L)     = 1.0/(BD(L)*1.E-01*DLAYR(L))
         NO3(L)     = AMAX1 (NO3(L),0.01)
         NH4(L)     = AMAX1 (NH4(L),0.01)
         SNO3(L)    = NO3(L) / FAC(L)
         SNH4(L)    = NH4(L) / FAC(L)
         IF (TOTN(L) .GT. 0.0) THEN
            !
            ! Calculate NHUM based on TOTN from the SOIL file
            ! Calculate C:N ratio
            !
            NHUM(L)   = TOTN(L)* DLAYR(L)*BD(L)*1.E03
            CNRAT(L)  = HUM(L)/NHUM(L)
          ELSE
            !
            ! If TOTN(J) is not known, calculate the value based on OC and
            ! a C:N ratio of 10:1
            !
            CNRAT(L)  = 10.0
            NHUM(L)   = HUM(L)/CNRAT(L)
         ENDIF
         NHUM(L)    = AMAX1 (NHUM(L),0.01)
         TSON       = TSON   + NHUM(L)
         FPOOL(L,1) = FOM(L) * PRCHO                 ! 0.20
         FPOOL(L,2) = FOM(L) * PRCEL                 ! 0.70
         FPOOL(L,3) = FOM(L) * PRLIG                 ! 0.10
         AINO3      = AINO3  + SNO3(L)
         AINH4      = AINH4  + SNH4(L)
      END DO

      ANO3  = AINO3
      ANH4  = AINH4
      TSIN  = AINO3 + AINH4
C
C     Initialize nitrification routine
C
      DO L = 1, NLAYR
         CNI(L) = 0.1
         WFY(L) = (SW(L)-LL(L))/DUL(L)
         IF (SW(L) .GT. DUL(L)) THEN
             WFY(L) = 1.0-((SW(L)-DUL(L))/(SAT(L)-DUL(L)))
         ENDIF
         WFY(L) = AMAX1 (WFY(L),0.0)
         TFY(L) = 0.0009766*ST(L)*ST(L)
         IF (ST(L) .LT. 5.0) THEN
             TFY(L) = 0.0
         ENDIF
         PHN(L) = 1.0
         IF (PH (L) .LT. 6.0) THEN
             PHN(L) = (PH(L)-4.5)/1.5
         ENDIF
         IF (PH (L) .GT. 8.0) THEN
             PHN(L) = 9.0-PH (L)
         ENDIF
         PHN(L) = AMAX1 (PHN(L),0.0)
      END DO

      END
