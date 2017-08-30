C=======================================================================
C  NBAL, Subroutine
C
C  Determines N balance
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      2-8-93
C-----------------------------------------------------------------------
C  INPUT  : IFLAG
C
C  LOCAL  : L,K,THUMN,ANO3,ANH4,TFERT,SEEDN,PLANTN,TALLN
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : CERES
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  IFLAG  :
C  I      : Loop counter
C  ANO3   : Total extractable nitrate N in soil profile (kg N/ha)
C  ANH4   : Total extractable ammonium N in soil profile (kg N/ha)
C  TFERT  : Total applied N fertilizer
C  SEEDN  : Seed N content
C  PLANTN : Plant N content
C  TALLN  : Total nitrogen
C=======================================================================

      SUBROUTINE NBAL (IFLAG,TSON,TIFON,ANO3,ANH4,TLCH,TNOX,WTNUP,YRDOY,
     &                 WTNFX,SEEDNI,NFERT,ANFER,WTNLF,WTNST,WTNSD,WTNSH,
     &                 WTNRT,WTNLO,NRESAP,RESIDUE,RESN)

      IMPLICIT NONE
      !
      ! Passed variables
      !
      INTEGER  YRDOY,NFERT,NRESAP
      REAL     TSON,TIFON,ANO3,ANH4,TLCH,TNOX,WTNUP,WTNFX,SEEDNI
      REAL     WTNLF,WTNST,WTNSD,WTNSH,WTNRT,ANFER(25)
      REAL     RESIDUE(10),RESN(10)
      !
      ! Local variables
      !
      INTEGER  IFLAG,I,YRDOYI
      REAL     TSONI,TIFONI,SEEDNF,ANO3I,ANH4I,TLCHI,TNOXI
      REAL     TFERTI,WTNFXI,WTNUPI,TALLNI,TFERT,TALLN,TRESN,TRESNI
      REAL     WTNSHO,WTNSHA,WTNLO,WTNLA,WTNSO,WTNSA
      REAL     WTNSDO,WTNSDA,WTNRO,WTNRA,WTNNOD,WTNNO,WTNNA
      REAL     WTNHAR,WTNOFF,WTNALL,TNLEAK

      TFERT  = 0.0
      SEEDNF = 0.0
      TRESN  = 0.0

      IF (IFLAG .EQ. 1) THEN
         DO I = 1, NFERT
            TFERT = TFERT + ANFER(I)
         END DO
         DO I = 1, NRESAP
            TRESN = TRESN + RESIDUE (I) * RESN(I)/100.0
         END DO
         OPEN (UNIT=77,FILE='NBAL.OUT',STATUS='UNKNOWN',
     &      POSITION='APPEND')
      ENDIF

      WTNSH  = 0.0
      WTNSHO = 0.0
      WTNSHA = WTNSH + WTNSHO
      WTNLA  = WTNLF + WTNLO
      WTNSO  = 0.0
      WTNSA  = WTNST + WTNSO
      WTNSDO = 0.0
      WTNSDA = WTNSD + WTNSDO
      WTNRO  = 0.0
      WTNRA  = WTNRT + WTNRO
      WTNNOD = 0.0
      WTNNO  = 0.0
      WTNNA  = WTNNO + WTNNOD
      TNLEAK = 0.0

      IF (IFLAG .EQ. 1) THEN
         TSONI  = TSON
         TIFONI = TIFON
         ANO3I  = ANO3
         ANH4I  = ANH4
         TLCHI  = TLCH
         TNOXI  = TNOX
         TFERTI = TFERT
         TRESNI = TRESN
         WTNFXI = WTNFX
         WTNUPI = WTNUP*10.0
         TALLNI = TSONI+TIFONI+ANO3I+ANH4I+TLCHI+TNOXI+TFERTI+TRESNI+
     &            SEEDNI*10.0+WTNUPI
         YRDOYI = YRDOY
      ENDIF

      IF (IFLAG .EQ. 2) THEN
         WTNHAR = WTNLF+WTNST+WTNRT+WTNSH+WTNSD+WTNNOD-WTNLO
         WTNUP  = WTNUP+WTNRT       !+WTNSD
         WTNOFF = WTNLO+WTNSO+WTNRO+WTNSHO+WTNSDO+WTNNO
         WTNALL = WTNLA+WTNSA+WTNRA+WTNSHA+WTNSDA+WTNNA-WTNLO
         WTNLF  = WTNLF-WTNLO

         TALLN  = TSON+TIFON+ANO3+ANH4+TLCH+TNOX+TFERT+TRESN+
     &            WTNUP*10.0+WTNLO*10.0
         WRITE (77,320) YRDOYI,YRDOY
         WRITE (77,400) TSONI,TSON,TIFONI,TIFON,
     &       ANO3I,ANO3,ANH4I,ANH4,TLCHI,TLCH,TNOXI,TNOX,
     &       TFERTI,TFERT,TRESNI,TRESN,WTNUPI,WTNUP*10.0,TALLNI,TALLN,
     &       SEEDNI*10.0,SEEDNF*10.0,WTNFXI*10.0,WTNFX*10.0
         WRITE (77,500)
         WRITE (77,600)
     &       WTNLF *10.0,WTNLO *10.0,WTNLA *10.0,
     &       WTNST *10.0,WTNSO *10.0,WTNSA *10.0,
     &       WTNSH *10.0,WTNSHO*10.0,WTNSHA*10.0,
     &       WTNSD *10.0,WTNSDO*10.0,WTNSDA*10.0,
     &       WTNRT *10.0,WTNRO *10.0,WTNRA *10.0,
     &       WTNNOD*10.0,WTNNO *10.0,WTNNA *10.0,
     &       WTNHAR*10.0,WTNOFF*10.0,WTNALL*10.0,
     &       TNLEAK*10.0,
     &       WTNALL*10.0+TNLEAK*10.0,WTNUP*10.0+SEEDNI*10.0+WTNFX*10.0
         CLOSE (77)
      ENDIF


      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 320  FORMAT(/,28X,'Initial, DOY',I6,3X,'Final, DOY',I6,
     &       /,28X,'--------------kg N/ha--------------')
 400  FORMAT  (5X,'Soil Organic N',T30,F10.2,10X,F10.2,
     &       /,5X,'Initial Residue N',T30,F10.2,10X,F10.2,
     &       /,5X,'Soil NO3',T30,F10.2,10X,F10.2,
     &       /,5X,'Soil NH4',T30,F10.2,10X,F10.2,
     &       /,5X,'Leached NO3',T30,F10.2,10X,F10.2,
     &       /,5X,'N Denitrified',T30,F10.2,10X,F10.2,
     &       /,5X,'Fertilizer N',T30,F10.2,10X,F10.2,
     &       /,5X,'Organic Added N',T30,F10.2,10X,F10.2,
     &       /,5X,'N Uptake From Soil',T30,F10.2,10X,F10.2,
     &       /,5X,'Total N',T30,F10.2,10X,F10.2,
     &      //,5X,'Seed N At Planting',T30,F10.2,10X,F10.2,
     &       /,5X,'N2 Fixed',T30,F10.2,10X,F10.2)
 500  FORMAT(/,5X,'Plant Component',T29,'At Harvest',7X,'Senesced',
     &         7X,'Total',/,28X,'---------------kg N/ha---------------')
 600  FORMAT  (5X,'Leaf N',T26,F10.2,T42,F10.2,T56,F10.2,
     &        /5X,'Stem N',T26,F10.2,T42,F10.2,T56,F10.2,
     &        /5X,'Shell N',T26,F10.2,T42,F10.2,T56,F10.2,
     &        /5X,'Seed N',T26,F10.2,T42,F10.2,T56,F10.2,
     &        /5X,'Root N',T26,F10.2,T42,F10.2,T56,F10.2,
     &        /5X,'Nodule N',T26,F10.2,T42,F10.2,T56,F10.2,
     &        /5X,'Total N',T26,F10.2,T42,F10.2,T56,F10.2,
     &        /5X,'N leakage',T56,F10.2,
     &        /5X,'TOTAL N',T56,F10.2,
     &      //,5X,'N Uptake From Soil + Seed N At Planting + '
     &            'N2 Fixed',T56,F10.2)

      END
