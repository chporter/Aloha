C=======================================================================
C  HTHETA Subroutine (c=control, i=input,o=output,t=time)
C  Calculates soil water release curve to initialize soil
C
C---------------------------------------------------------------------------
C  REVISION HISTORY
C  04/03/96 Written by J. W. Jones, based on Brooks-Corey function
C           adjusted to match LL(L), DUL(L), and SAT(L) values for soil
C---------------------------------------------------------------------------
C  Variable   Description                                   Units      Class
C---------------------------------------------------------------------------
C  NLAYR      Number of soil layers for this soil             -         i
C  LL         Lower limit (15 atm.) soil water availability  cm3/cm3    i
C  DUL        Drained Upper limit (0.1 atm) soil water       cm3/cm3    i
C  SAT        Saturated soil water availability              cm3/cm3    i
C  SMC(L,K)   Soil water resease point, layer L, SWP K       cm3/cm3    o
C  L          Layer number                                     -
C  K          Number of soil water potential value             -
C  SWP(K)     Soil water potential value for SMC curve         cm
C  Tb(L)      Bubbling pressure for soil, layer L              cm
C  BETA       Exponent in Brooks-Corey equation                -
C-----------------------------------------------------------------------
C  Called :
C
C  Calls  : None
C=======================================================================

      SUBROUTINE HTHETA(NLAYR,LL,DUL,SAT,SMC,SWP)

      IMPLICIT NONE

C     Declaration of Subroutine Arguments
        REAL LL(20),DUL(20),SAT(20),SMC(20,30),SWP(16)
        INTEGER NLAYR

C     Local Variables
        REAL SWPP(16),Tb(20),BETA,RSW,BOT,RATIO,SMB(20,16)
        INTEGER L,K

        DATA SWPP/10,20,30,40,50,60,70,80,90,100,300,1000,3000,5000,
     &           10000,15000/

        DO L = 1,16
           SWP(L) = SWPP(L)
        ENDDO

C---------------------------------------------------------------------------
C     This module is called from an INITIAL section of another module,
C          thus, it has only one section
C---------------------------------------------------------------------------
      DO L = 1,NLAYR
C---------------------------------------------------------------------------
C       Compute Tb, bubbling pressure for this layer
C       This is based on table in DRAINMOD for Tb of different soils
C       and on values of DUL for those soils from the DSSAT soils
C---------------------------------------------------------------------------
        Tb(L) = 0.3 + 74.0 * DUL(L)
        RSW = 0.75 * LL(L)

C---------------------------------------------------------------------------
C         Compute Beta, the Brooks-Corey exponent, for soil water contents
C         assuming that residual SW is 0.75*LL(L)
C---------------------------------------------------------------------------
        BOT = ALOG(Tb(L)/15000.)
        BETA = ALOG((LL(L)-RSW)/(SAT(L)-RSW))/BOT

          DO K = 1,16
            RATIO = MIN(Tb(L)/SWP(K),1.0)
            SMB(L,K) = RSW + (SAT(L)-RSW)*(RATIO)**BETA
          ENDDO
      ENDDO

C---------------------------------------------------------------------------
C    Adjust SMC, forcing DUL at 100 cm water potential
C---------------------------------------------------------------------------
      DO L = 1,NLAYR
        DO K = 1,10
          SMC(L,K) = SAT(L)-(SAT(L)-DUL(L))*(SMB(L,K)-SAT(L))/
     &                                      (SMB(L,10)-SAT(L))
        ENDDO
        DO K = 11,16
          SMC(L,K) = DUL(L)-(DUL(L)-LL(L))*(SMB(L,K)-SMB(L,10))/
     &                                     (SMB(L,16)-SMB(L,10))
        ENDDO
      ENDDO
      END
