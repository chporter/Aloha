C=======================================================================
C  NTRANS, Subroutine
C
C  Determines N transformations
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified for version 3 input/output (matches LeGRO), WTB  8 AUG 92
C  3. Header revision and minor changes             P.W.W.      2-8-93
C-----------------------------------------------------------------------
C  INPUT  : YRDOY,IFERI,TSIN,NAPNIT,AMTNIT,FTYPEN,DSOILN,SOILNC,SOILNX,
C           DOY,TNOX,DLAYR,DUL,LL,SW,SAT,NLAYR,SWEF
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : CERES
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  YRDOY  :
C  IFERI  :
C  TSIN   :
C  NAPNIT :
C  AMTNIT :
C  FTYPEN :
C  DSOILN :
C  SOILNC :
C  SOILNX :
C  DOY    : Day of year
C  TNOX   :
C  DLAYR  : Depth increment of soil layer L - cm
C  DUL    : Drained upper limit soil water for soil layer L - volume fraction
C  SW     : Actual soil water content in Layer L - volume fraction
C  SAT    : Field saturated soil water content in Layer L - volume fraction
C  NLAYR  : Number of layers in soil
C  SWEF   : Soil water evaporation fraction.  The fraction of the LL water
C           content that determines the lowest possible value the top soil
C           layer water content can become by soil evaporation.  The value
C           depends on the depth of the first layer.
C  ANH4   : Total extractable ammonium N in soil profile (kg N/ha)
C  ANO3   : Total extractable nitrate N in soil profile (kg N/ha)
C=======================================================================

      SUBROUTINE NTRANS (YRDOY,IFERI,TSIN,NAPNIT,AMTNIT,FTYPEN,DSOILN,
     &           SOILNC,SOILNX,TNOX,DOY,DLAYR,DUL,LL,
     &           SW,SAT,NLAYR,SWEF,DAP,IRESI,NAPRES)

      IMPLICIT  NONE

      INCLUDE  'NTRC1.BLK'
      INCLUDE  'NTRC2.BLK'

      CHARACTER IFERI*1,IRESI*1

      INTEGER   JP,YRDOY,NAPNIT,FTYPEN,L,NLAYR,DOY,DAP,NAPRES

      REAL      RDECR(3),DLAYR(20),NNOM,MF,DLAG
      REAL      AD,WFD,XL,SWF,TF,AK,UHYDR,TOTIN,CNR,CNRF,GRCOM
      REAL      GRNOM,G1,X,RHMIN,RNAC,XMIN,SANC,ELNC,RP2,A,BB,B2,RNTRF
      REAL      SARNC,XW,XT,DNRATE,FW,SOILC,CW,FT,TSIN
      REAL      AMTNIT,SOILNC,TNOX,SOILNX,DSOILN
      REAL      SW(20),DUL(20),LL(20),SAT(20),SWEF
      !
      ! Assign RDECR values based on RESIDUE parameters in the SOIL.PAR file
      !
      RDECR(1) = RDCHO
      RDECR(2) = RDCEL
      RDECR(3) = RDLIG
      !
      ! The default values of RDECR,  DATA RDECR /0.2,0.05,0.0095/,
      ! are now read in from the SOIL.PAR file.  They specify the
      ! decomposition rates of CHO, cellulose, and lignin, respectively
      !
      TIFON  = 0.0
      TIFOM  = 0.0
      TSOC   = 0.0
      TSON   = 0.0
      ANO3   = 0.0
      ANH4   = 0.0
      !
      ! Call FPLACE
      !
      IF (INDEX('AFRD',IFERI) .GT. 0) THEN
         CALL FPLACE (YRDOY,IFERI,NAPNIT,AMTNIT,FTYPEN,DSOILN,SOILNC,
     &                SOILNX,DOY,DLAYR,NLAYR,DAP)
      ENDIF
      !
      ! Call RPLACE
      !
      IF (INDEX('AFRD',IRESI) .GT. 0) THEN
         CALL RPLACE (YRDOY,DAP,NLAYR,DLAYR,IRESI,NAPRES)
      ENDIF

      IF (DOY .EQ. IUOF) THEN
         !
         ! Assume all UREA was hydrolyzed.
         !
         DO L = 1, NLAYR
            SNH4(L) = SNH4(L) + UREA(L)
            UREA(L) = 0.0
            UPPM(L) = 0.0
         END DO
         IUON = .FALSE.
      ENDIF

      DO L = 1, NLAYR
         AD  = LL(L)
         IF (L .EQ. 1) THEN
            AD = LL(L)*SWEF
         ENDIF
         MF  = (SW(L)-AD)/(DUL(L)-AD)
         WFD = MF

         IF (SW(L) .GT. DUL(L)) THEN
            XL   = (SW(L)-DUL(L))/(SAT(L)-DUL(L))
            MF   = 1.0 - 0.5*XL
            WFD  = 1.0 - XL
         ENDIF

         WFD     = AMAX1 (WFD,0.0)
         NO3(L)  = SNO3(L) * FAC(L)
         NH4(L)  = SNH4(L) * FAC(L)
         UPPM(L) = UREA(L) * FAC(L)

         IF (IUON) THEN
            !
            ! Do some UREA hydrolysis
            !
            SWF   = MF + 0.20
            SWF   = AMIN1 (SWF,1.0)
            SWF   = AMAX1 (SWF,0.0)
            TF    = (ST(L)/40.0) + 0.20
            TF    = AMAX1 (TF,0.0)
            AK    = -1.12+1.31*OC(L)+0.203*PH(L)-0.155*OC(L)*PH(L)
            AK    = AMAX1 (AK,0.25)
            UHYDR = AK*MIN(SWF,TF)*UREA(L)
            IF (UHYDR .GT. UREA(L)) THEN
                UHYDR = UREA(L)
            ENDIF
            UREA(L) = UREA(L) - UHYDR
            SNH4(L) = SNH4(L) + UHYDR
         ENDIF

         TF     = (ST(L)-5.0)/30.0
         IF (ST(L) .LT. 5.0) THEN
            TF = 0.0
         ENDIF
         !
         ! TOTIN is total inorganic N, replaces TOTN, which is an array
         ! of total soil N
         !
         TOTIN  = SNO3(L) + SNH4(L) - 0.5/FAC(L)
         TOTIN  = AMAX1 (TOTIN,0.0)
         CNR    = (0.4*FOM(L))/(FON(L)+TOTIN)
         CNRF   = EXP(-0.693*(CNR-25)/25.0)
         CNRF   = AMIN1 (CNRF,1.0)
         FOM(L) = 0.0
         GRCOM  = 0.0
         GRNOM  = 0.0
         MF     = AMAX1 (MF,0.0)
         DO JP = 1, 3
            FOM(L) = FOM(L) + FPOOL(L,JP)
         END DO

         DO JP = 1, 3
            IF (FOM(L) .LT. 5.0) THEN
               G1          = 0.0
             ELSE
               G1          = TF*MF*CNRF*RDECR(JP)
               X           = FPOOL(L,JP) / FOM(L)
               FPOOL(L,JP) = FPOOL(L,JP) - FPOOL(L,JP)*G1
            ENDIF
            GRCOM = GRCOM + G1*X*FOM(L)
            GRNOM = GRNOM + G1*X*FON(L)
         END DO

         RHMIN   = NHUM(L)*DMINR*TF*MF*DMOD
         !
         ! HUM is now KG C/HA
         !
         ! Use actual C:N ratio (CNRAT) rather than 10.0 if TOTN is known;
         ! If not, CNRAT() is set to 10.0 as a default
         !
         ! HUM(L)  = HUM(L)  - RHMIN *10.0 + 0.2*GRNOM/0.10
         HUM(L)  = HUM(L)  - RHMIN * CNRAT(L) + 0.2*GRNOM/0.10
         NHUM(L) = NHUM(L) - RHMIN + 0.2*GRNOM
         RNAC    = MIN (TOTIN,GRCOM*(0.02-FON(L)/FOM(L)))
         RNAC    = AMAX1 (RNAC,0.0)
         FOM(L)  = FOM(L)    - GRCOM
         FON(L)  = FON(L)    + RNAC  - GRNOM
         NNOM    = 0.8*GRNOM + RHMIN - RNAC
         TIFON   = TIFON     + FON(L)
         TIFOM   = TIFOM     + FOM(L)
         TSOC    = TSOC      + HUM(L)
         TSON    = TSON      + NHUM(L)
         XMIN    = 0.5/FAC(L)
         !
         ! Net mineralization added here.
         !
         ! Immobilization if NNOM is less than 0.0
         ! First got N from AMMONIUM - when too low, get remainder from mitrate
         !
         IF (NNOM .GT. 0.0) THEN
             SNH4(L) = SNH4(L) + NNOM
           ELSE
             IF (ABS(NNOM) .GT. (SNH4(L) - XMIN)) THEN
                NNOM    = NNOM + SNH4(L) - XMIN
                SNH4(L) = XMIN
               ELSE
                SNH4(L) = SNH4(L) + NNOM
                NNOM    = 0.0
             ENDIF
             SNO3(L) = SNO3(L) + NNOM
         ENDIF
         !
         ! Nitrification section
         !
         SANC    = 1.0-EXP(-0.01363*SNH4(L))
         ELNC    = MIN (TF,WFD,SANC)
         RP2     = CNI(L)*EXP(2.302*ELNC)
         RP2     = AMAX1 (RP2,0.05)
         RP2     = AMIN1 (RP2,1.00)
         CNI(L)  = RP2
         A       = MIN (RP2,WFD,TF,PHN(L))
         NH4(L)  = SNH4(L)*FAC(L)
         BB      = (A*40.0*NH4(L)/(NH4(L)+90.0))
         BB      = AMIN1 (BB,0.80)
         BB      = BB*SNH4(L)
         B2      = (NH4(L)-0.5)/FAC(L)
         RNTRF   = MIN (BB,B2)
         SNH4(L) = SNH4(L) - RNTRF
         SNO3(L) = SNO3(L) + RNTRF
         SARNC   = 1.0 - EXP(-0.1363*SNH4(L))
         XW      = MAX (WFD,WFY(L))
         XT      = MAX (TF,TFY(L))
         CNI(L)  = CNI(L)*MIN (XW,XT,SARNC)
         CNI(L)  = AMAX1 (0.05,CNI(L))
         WFY(L)  = WFD
         TFY(L)  = TF
         !
         ! Denitrification section
         !
         DNRATE = 0.0
         NO3(L) = SNO3(L)*FAC(L)

         IF (NO3(L) .GT. 1.0 .AND. SW(L) .GT. DUL(L) .AND.
     &        ST(L) .GE. 5.0) THEN
            DLAG   = 1.0
            FW     = 0.0
            !
            ! Old SOILC = 0.58*HUM(L)
            !
            ! This corrects a previous inconsistency in
            ! how soil OM, HUM, was determined : HUM = kgC/ha
            !
            SOILC   = HUM(L)
            CW      = FAC(L)*(SOILC*0.0031+0.4*FPOOL(L,1))+24.5
            FW      = 1.0 - (SAT(L)-SW(L))/(SAT(L)-DUL(L))
            FT      = 0.1*EXP(0.046*ST(L))
            DNRATE  = 6.0*1.E-05*CW*NO3(L)*BD(L)*FW*FT*DLAYR(L)*DLAG
            DNRATE  = AMAX1 (DNRATE,0.0)
            XMIN    = 0.25/FAC(L)
            DNRATE  = AMIN1 (DNRATE,SNO3(L)-XMIN)
            SNO3(L) = SNO3(L) - DNRATE
            TNOX    = TNOX    + DNRATE
         ENDIF

         NO3(L)  = SNO3(L) * FAC(L)
         NH4(L)  = SNH4(L) * FAC(L)
         ANO3    = ANO3    + SNO3(L)
         ANH4    = ANH4    + SNH4(L)
      END DO

      TSIN = ANH4 + ANO3

      END

