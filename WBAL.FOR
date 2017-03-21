C=====================================================================
C  WBAL, Subroutine, Gerrit Hoogenboom
C  Provides output Water balance
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/20/96 GH  Written
C
C---------------------------------------------------------------------
C  INPUT :
C
C
C  LOCAL :
C
C
C  OUTPUT : Prints output file called WBAL.OUT
C---------------------------------------------------------------------
C                         DEFINITIONS
C
C=====================================================================

      SUBROUTINE WBAL(YRSIM,YRDOY,TSWINI,TSW,TOTIR,CRAIN,TRUNOF,
     &                TDRAIN,CEP,CES,EFFIRR,CEO)

      IMPLICIT NONE

      INTEGER YRSIM,YRDOY

      REAL TSWINI,TSW,TOTIR,CRAIN,TRUNOF,TDRAIN,CEP,CES,EFFIRR
      REAL WBALAN,CEO

      OPEN(UNIT=78,FILE='WBAL.OUT',STATUS='UNKNOWN',ACCESS = 'APPEND')

      WRITE (78,320)
 320  FORMAT(/,5X,'WATER BALANCE PARAMETERS',
     &       /,5X,'========================',T42,'--mm--')
          WRITE (78,400)
     &                   YRSIM,TSWINI*10,
     &                   YRDOY,TSW*10,TOTIR,TOTIR*EFFIRR,
     &                   (1.0-EFFIRR)*TOTIR,CRAIN,TDRAIN,TRUNOF,
     &                   CES,CEP,CES+CEP,CEO
 400  FORMAT(
     &       /,5X,'Soil H20 (start) on day ',I6,T38,F10.2,
     &       /,5X,'Soil H20 (final) on day ',I6,T38,F10.2,
     &       /,5X,'Irrigation',T38,F10.2,
     &       /,5X,'Effective Irrigation',T38,F10.2,
     &       /,5X,'Irrigation Lost',T38,F10.2,
     &       /,5X,'Precipitation',T38,F10.2,
     &       /,5X,'Drainage',T38,F10.2,
     &       /,5X,'Runoff',T38,F10.2,
     &       /,5X,'Soil Evaporation',T38,F10.2,
     &       /,5X,'Transpiration',T38,F10.2,
     &       /,5X,'Evapotranspiration',T38,F10.2,
     &       /,5X,'Potential ET',T38,F10.2,/)

      WBALAN = TOTIR*EFFIRR+CRAIN-TDRAIN-TRUNOF-CES-CEP-TSW*10+
     &         TSWINI*10

         WRITE  (78,500) WBALAN
 500  FORMAT(
     &       /,5X,'Final Balance ',T36,F12.4,/)

      CLOSE(UNIT=78)

      END
 
