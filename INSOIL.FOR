C=======================================================================
C  INSOIL, Subroutine, G. Hoogenboom
C  Soil initilization
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  08/21/95 GH  Defined
C  1-JUL-97 BDB Changed CN and SMX initialization (removed CN1, CN2,
C               CN3, WF, WX, XX)
C-----------------------------------------------------------------------
C  INPUT  :
C
C  LOCAL  : SWR,L
C
C  OUTPUT : TSWINI,CUMDPI,NAP
C-----------------------------------------------------------------------
C  Called : CROPGRO, CERES
C
C  Calls  : None
C=======================================================================

      SUBROUTINE INSOIL (ISWWAT,AINO3,ANO3,AINH4,ANH4,TSWINI)

      IMPLICIT  NONE

      INCLUDE  'GEN3.BLK'

      CHARACTER ISWWAT*1
      INTEGER   L

      REAL SWR,DL1,DL2,AINO3,AINH4,ANO3,ANH4
      REAL TSWINI,TDUL,TPESW,DDLL1(20),DDLL2(20),USOIL

C-----------------------------------------------------------------------
C     Initialize Soil Information
C-----------------------------------------------------------------------

      PESW   = 0.0
      CUMDEP = 0.0
      TSW    = 0.0
      TSWINI = 0.0
      TPESW  = 0.0
      TDUL   = 0.0
      TLL    = 0.0
      TSAT   = 0.0
      AINO3  = 0.0
      AINH4  = 0.0
      ANO3   = 0.0
      ANH4   = 0.0
      CUMDEP = 0.0

      IF (ISWWAT .NE. 'Y') RETURN

C-----------------------------------------------------------------------
C  Check first stage evaporation limit
C-----------------------------------------------------------------------

      IF (U .LE. 0.0) THEN
          U = 6.0
      ENDIF

C-----------------------------------------------------------------------
C  Set air dry water content for top soil layer
C-----------------------------------------------------------------------

      SWEF = 0.9-0.00038*(DLAYR(1)-30.)**2
      IF (SW(1) .LT. SWEF*LL(1)) THEN
          SW(1) = LL(1)*SWEF
      ENDIF

C-----------------------------------------------------------------------
C  Calculate current soil water content with respect to evaporation sums
C-----------------------------------------------------------------------

      SWR   = MAX (0.0,(SW(1)-LL(1))/(DUL(1)-LL(1)))
      USOIL = (DUL(1) - SW(1)) * DLAYR(1) * 10.0

C-----------------------------------------------------------------------
C  Set totals for stage 1 and stage 2 evaporation
C-----------------------------------------------------------------------

      IF (SWR .GE. 1.0) THEN
         SUMES1 = 0.0
         SUMES2 = 0.0
         T      = 0.0
      ELSE IF (USOIL .LE. U) THEN
         SUMES2 = 0.0
         T      = 0.0
         SUMES1 = USOIL
      ELSE
         SUMES2 = USOIL - U
         SUMES1 = U
         T      = (SUMES2/3.5)**2
      ENDIF

      DL1   = 0.0
      IDRSW = 0

      DO L = 1, NLAYR
         DL2      = DL1+DLAYR(L)
         ESW(L)   = DUL(L)-LL(L)
         DDLL1(L) = DL1
         DDLL2(L) = DL2
         DL1      = DL2
         CUMDEP   = CUMDEP + DLAYR(L)
         TSWINI   = TSWINI + SW(L) *DLAYR(L)
         TPESW    = TPESW  + ESW(L)*DLAYR(L)
         TLL      = TLL    + LL(L) *DLAYR(L)
         TDUL     = TDUL   + DUL(L)*DLAYR(L)
         TSAT     = TSAT   + SAT(L)*DLAYR(L)
      END DO

      TSW  = TSWINI
      PESW = MAX(0.0,TSW - TLL)

C-----------------------------------------------------------------------
C     Initialize curve number (according to J.T. Ritchie) 1-JUL-97 BDB
C-----------------------------------------------------------------------

      CN = AMIN1 (CN,100.0)
      CN = AMAX1 (CN,1.5)
      SMX = 254.0 * (100.0/CN - 1.0)

      RETURN
      END
