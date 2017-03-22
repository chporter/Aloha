C=======================================================================
C  SEFLD, Subroutine
C
C  Determines field related sensitivity analysis options
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       G.H.       04-02-96
C  2.
C-----------------------------------------------------------------------
C  INPUT  :
C
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SENS
C
C  Calls  : ERROR CLEAR SEPLYR VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SEFLD (RNMODE,XCRD,YCRD,ELEV,SLOPE)

      IMPLICIT     NONE

      CHARACTER*1  RNMODE,LINE(80)
      CHARACTER*6  ERRKEY

      INTEGER      MENU,NLOOP

      REAL         EFF,FLAG
      REAL         XCRD,YCRD,SLOPE,ELEV

      PARAMETER (ERRKEY = 'SEFLD ')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (RNMODE .EQ. 'I')  THEN
         CALL CLEAR
         WRITE (*,200) XCRD,YCRD,ELEV,SLOPE
         WRITE (*,250)
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 0) THEN
          RETURN
      ELSE IF (MENU .EQ. 1) THEN
          IF (RNMODE .EQ. 'I') WRITE (*,300) XCRD
          READ (5,1000) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (FLAG .LE. 0) THEN
             XCRD = EFF
          ENDIF
      ELSE IF (MENU .EQ. 2) THEN
          IF (RNMODE .EQ. 'I') WRITE (*,400) YCRD
          READ (5,1000) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (FLAG .LE. 0) THEN
             YCRD = EFF
          ENDIF
      ELSE IF (MENU .EQ. 3) THEN
          IF (RNMODE .EQ. 'I') WRITE (*,500) ELEV
          READ (5,1000) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (FLAG .LE. 0) THEN
             ELEV = EFF
          ENDIF
      ELSE IF (MENU .EQ. 4) THEN
          IF (RNMODE .EQ. 'I') WRITE (*,600) SLOPE
          READ (5,1000) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (FLAG .LE. 0 .AND. EFF .GE. 0.0 .AND. EFF .LE. 100.) THEN
             SLOPE = EFF
          ENDIF
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  200 FORMAT(//,15X,'PLOT/FIELD/REGION RELATED VARIABLES',
     &        /,15X,'===================================',//,
     & 7X,'0. Return to Main Menu ',//,
     & 7X,'1. X Coordinate (Any Reference System) .................]',
     & 1X,F15.5,/,
     & 7X,'2. Y Coordinate (Any Reference System) .................]',
     & 1X,F15.5,/,
     & 7X,'3. Elevation m..........................................]',
     & 1X,F9.2,/,
     & 7X,'4. Slope %..............................................]',
     & 1X,F5.2)
  250 FORMAT (//,9X,'SELECTION ? [ Default = 0 ] ===> ',$)
  300 FORMAT (//,
     &  9X,'X Coordinate                      ===>',1X,F15.5,/,
     &  9X,'NEW COORDINATE                    --->  ',$)
  400 FORMAT (//,
     &  9X,'Y Coordinate                      ===>',1X,F15.5,/,
     &  9X,'NEW COORDINATE                    --->  ',$)
  500 FORMAT (//,
     &  9X,'Elevation                         ===>',1X,F9.2,' m',/,
     &  9X,'NEW ELEVATION                     --->  ',$)
  600 FORMAT (//,
     &  9X,'Slope                             ===>',1X,F5.2,' %',/,
     &  9X,'NEW SLOPE                         --->  ',$)
 1000 FORMAT (80A1)

      END
