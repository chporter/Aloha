C=======================================================================
C  OPSEQ, Subroutine
C
C  Writes variables to a temporary file to pass on to next sequence run
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      2-9-93
C  4. Modifications for passing variables correctly W.Bowen     JUN 95
C-----------------------------------------------------------------------
C  INPUT  : YRDOY
C
C  LOCAL  : WTNVEG
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPHARV
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPSEQ (NREP,YRDOY,RSEED,TMA,ATOT,PLTPOP,
     &           DEPMAX,NLAYR,ESW,SUMES1,SUMES2,T,TLL,PESW,TSW,CUMDEP,
     &           TSAT,SWDEF,ATHETA,SW,RTWT,STOVN,ROOTN,GNP,CNSD1,CNSD2,
     &           ATANC,ANFAC,DMINR,FPOOL,WFY,TFY,PHN,CNI,FOM,FON,HUM,
     &           NHUM,UREA,UPPM,BD,PH,OC,NO3,NH4,FAC,SNO3,SNH4,
     &           TOPWT,SDWT,BWAH,SDWTAH,WTNSD)

      IMPLICIT NONE

      INTEGER  I,YRDOY,RSEED(4),NREP,NLAYR
      REAL     TMA(5),ATOT,PLTPOP,RTWT
      REAL     DEPMAX,ESW(20),SUMES1,SUMES2,T,TLL,PESW,TSW,CUMDEP
      REAL     TSAT,SWDEF,ATHETA,SW(20)
      REAL     STOVN,ROOTN,GNP,CNSD1,CNSD2,ATANC,ANFAC,DMINR
      REAL     FPOOL(20,3),WFY(20),TFY(20),PHN(20),CNI(20),FOM(20)
      REAL     FON(20),HUM(20),NHUM(20),UREA(20),UPPM(20),BD(20)
      REAL     PH(20),OC(20),NO3(20),NH4(20),FAC(20),SNO3(20),SNH4(20)
      REAL     TOPRES,TOPWT,SDWT,BWAH,SDWTAH,RTRES,WTNRES,WTNSD,WTNRRS

C-----------------------------------------------------------------------
C Determine dry weight and N content of stover
C-----------------------------------------------------------------------

      TOPRES = TOPWT - SDWT - BWAH
      IF ((TOPWT - SDWT) .GT. 0.0) THEN
         WTNRES = (STOVN*PLTPOP) * (TOPWT - SDWT - BWAH)/ (TOPWT - SDWT)
      ELSE
         WTNRES = 0.0
      ENDIF
      RTRES  = RTWT  * PLTPOP
      WTNRRS = ROOTN * PLTPOP

C-----------------------------------------------------------------------
C     Add weight of seed not harvested
C-----------------------------------------------------------------------

      TOPRES = TOPRES + MAX(0.0,(SDWT - SDWTAH))
      IF (SDWT .GT. 0.0) THEN
         WTNRES = WTNRES + WTNSD * (MAX(0.0,(SDWT - SDWTAH)))/SDWT
      ENDIF

C-----------------------------------------------------------------------
C Write out variables to be passed to next model
C-----------------------------------------------------------------------

      OPEN (UNIT=29,FILE='TMP.DAT',STATUS='UNKNOWN')

      WRITE (29,*) YRDOY,NREP
      WRITE (29,*) TOPRES*10,WTNRES*10
      WRITE (29,*) RTRES*10,WTNRRS*10,DEPMAX,NLAYR
      WRITE (29,*) ESW
      WRITE (29,*) SUMES1,SUMES2,T,TLL,PESW,TSW,CUMDEP
      WRITE (29,*) TSAT,SWDEF,ATHETA
      WRITE (29,*) GNP,CNSD1,CNSD2,ATANC,ANFAC,DMINR
      WRITE (29,*) FPOOL
      WRITE (29,*) WFY
      WRITE (29,*) TFY
      WRITE (29,*) PHN
      WRITE (29,*) CNI
      WRITE (29,*) FOM
      WRITE (29,*) FON
      WRITE (29,*) HUM
      WRITE (29,*) NHUM
      WRITE (29,*) UREA
      WRITE (29,*) UPPM
      WRITE (29,*) TMA
      WRITE (29,*) ATOT
      WRITE (29,*) RSEED

      DO I = 1, NLAYR
        WRITE (29,*) SW(I),BD(I),PH(I),OC(I)
      END DO

      DO I = 1, NLAYR
        WRITE (29,*) NO3(I),NH4(I),FAC(I),SNO3(I),SNH4(I)
      END DO

      CLOSE (29)

      RETURN

      END
