!chp      INTERFACE TO INTEGER*2 FUNCTION SYSTEM [C]
!chp     &   (STRING[REFERENCE])
!chp       CHARACTER*1 STRING
!chp      END

!chp	INTERFACE 
!chp        FUNCTION system [C] (string[reference])
!chp          CHARACTER*1 string
!chp          INTEGER*2   system
!chp        END FUNCTION
!chp      END INTERFACE
C=======================================================================
C  COPYRIGHT 1998 the University of Georgia, Griffin, Georgia
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  DRIVER, Main program
C
C  Driver to call input and model for CROPGRO and CERES models
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       G.H.        4-26-93
C  2  Modified by                                   G.H.        8-09-93
C  3. Header revision and minor changes             P.W.W       5-28-93
C  4. Changed temp. file to IBSNAT31.INP            G.H         4-01-96
C  5. Changed temp. file to IBSNAT35.INP            G.H         5-06-98
C-----------------------------------------------------------------------
C  INPUT  : Command Line Arguments
C
C  LOCAL  :
C
C  OUTPUT : Spawn INPUT and MODEL EXEs with appropriate command line
C           arguments
C-----------------------------------------------------------------------
C  Called :
C
C  Calls  : SYSTEM
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  XXXXX  :
C=======================================================================
      PROGRAM      MODELDRV

      USE DFPORT                                          !chp
      IMPLICIT     NONE

      CHARACTER*1   ANS,TYPEIO,RNMODE,BLANK,UPCASE
      CHARACTER*3   EXPARG,TRNARG
      CHARACTER*6   ERRKEY,REPARG
      CHARACTER*12  MODEL,FILEIO,INPUT
      CHARACTER*72  PATHX                 !chp changed to 72 (was 52)
      CHARACTER*100 MODELX,INPUTX

      INTEGER       ERRNUM,LUNIO,TRTALL,TRTNO,EXPNO,NREP
!chp  INTEGER*2     SYSTEM,I,IP,IPX
      INTEGER*2     I,IP,IPX                              !chp
	INTEGER iyr, imon, iday, ihr, imin, isec, i100th    !chp

      LOGICAL       DONE,FEXIST

      PARAMETER     (ERRKEY = 'MODELD')
      PARAMETER     (FILEIO = 'IBSNAT35.INP')
      PARAMETER     (LUNIO  = 20)
      PARAMETER     (BLANK  = ' ')

      DONE = .FALSE.

C-----------------------------------------------------------------------
!     New output file to track run-time arguments passed in system calls.
C-----------------------------------------------------------------------
D     OPEN (200,FILE='RUNARGD.OUT',STATUS='UNKNOWN')      !chp
D	CALL GETDAT (iyr, imon, iday)                       !chp
D     CALL GETTIM (ihr, imin, isec, i100th)               !chp
D
D	WRITE(200,10) imon, iday, iyr, ihr, imin            !chp
D  10 FORMAT(/,'Runtime arguments list file',             !chp
D    &   /,'Date: ',I2,'/',I2,'/',I4,                     !chp
D    &   /,'Time: ',I2,':',I2,/)                          !chp

!	pause "Starting driver, enter to continue"          !chp

C-----------------------------------------------------------------------
C    Get argument from runtime module to determine path of the EXE files
C-----------------------------------------------------------------------
      CALL GETARG(0,PATHX,IPX)
      CALL GETARG(1,INPUT,IP)
      CALL GETARG(2,MODEL,IP)
      CALL GETARG(3,TYPEIO,IP)
      TYPEIO = UPCASE(TYPEIO)
      IF (INDEX('IRX',TYPEIO) .EQ. 0) TYPEIO = 'I'
      CALL GETARG(4,RNMODE,IP)
      RNMODE = UPCASE(RNMODE)
      IF (INDEX('IANQGSF',RNMODE) .EQ. 0) RNMODE = 'I'
      CALL GETARG(5,EXPARG,IP)
      CALL GETARG(6,TRNARG,IP)

D     WRITE(200,20) PATHX, INPUT, MODEL, TYPEIO,          !chp
D    &   RNMODE, EXPARG, TRNARG                           !chp
D  20 FORMAT(/,'Runtime arguments passed to driver:',     !chp
D    &   /,'PATHX:  ',A72,                                !chp
D    &   /,'INPUT:  ',A12,                                !chp
D    &   /,'MODEL:  ',A12,                                !chp
D    &   /,'TYPEIO: ',A1,                                 !chp
D    &   /,'RNMODE: ',A1,                                 !chp
D    &   /,'EXPARG: ',A3,                                 !chp
D    &   /,'TRNARG: ',A3)                                 !chp
D
D     print *,'Read arguments for driver'                 !chp

C-----------------------------------------------------------------------
C    Delete previouse copies of IBSNAT35.INP
C-----------------------------------------------------------------------
      INQUIRE (FILE = FILEIO,EXIST = FEXIST)
      IF (FEXIST) THEN
          OPEN (LUNIO, FILE = FILEIO,STATUS = 'UNKNOWN',IOSTAT=ERRNUM)
          CLOSE (LUNIO,STATUS = 'DELETE')
      ENDIF
C-----------------------------------------------------------------------
C    Create path for input module
C-----------------------------------------------------------------------

      INPUTX = PATHX(1:(IPX-12)) // INPUT
!--------------------------------------------------------------------
!     Debug lines - ignore path to driver
D     INPUTX = INPUT
!--------------------------------------------------------------------
      INQUIRE (FILE = INPUTX,EXIST = FEXIST)
      IF (.NOT. FEXIST) THEN
         WRITE(*,900) INPUT
  900    FORMAT(//,15X,'Unable to locate Input Module ',A12,//,
     &          15X,'Please Check Path and Directory',/)
         STOP
      ENDIF
C-----------------------------------------------------------------------
C    Create path for runtime module
C-----------------------------------------------------------------------
      MODELX = PATHX(1:(IPX-12)) // MODEL
!--------------------------------------------------------------------
!     Debug lines - ignore path to driver
D     MODELX = MODEL
D     IPX = 12
!--------------------------------------------------------------------
      INQUIRE (FILE = MODELX,EXIST = FEXIST)
      IF (.NOT. FEXIST) THEN
         WRITE(*,1400) MODEL
 1400 FORMAT(//,15X,'Unable to locate Model Module ',A12,//,
     &          15X,'Please Check Path and Directory',/)
         STOP
      ENDIF
C-----------------------------------------------------------------------
C    Run INPUT module
C-----------------------------------------------------------------------
      DO WHILE (.NOT. DONE)
      INPUTX = INPUTX(1:IPX) // BLANK // MODEL // BLANK // FILEIO //
     &         BLANK  // TYPEIO // BLANK // RNMODE // BLANK //
     &         EXPARG // BLANK // TRNARG

D     WRITE(200,40) INPUTX(1:IPX), MODEL, FILEIO, TYPEIO,        !chp
D    &   RNMODE, EXPARG, TRNARG                           !chp
D  40 FORMAT(/,'Runtime arguments passed to INPUT:',      !chp
D    &   /,'INPUTX: ',A100,                               !chp
D    &   /,'MODEL:  ',A12,                                !chp
D    &   /,'FILEIO: ',A12,                                !chp
D    &   /,'TYPEIO: ',A1,                                 !chp
D    &   /,'RNMODE: ',A1,                                 !chp
D    &   /,'EXPARG: ',A3,                                 !chp
D    &   /,'TRNARG: ',A3)                                 !chp
D     print *,'Ready to call INPUT'                       !chp
D
      I = SYSTEM(INPUTX)
      IF (I .GT. 0) STOP 'Could not run INPUT module'

C-----------------------------------------------------------------------
C    Check to see if the temporary file exists
C-----------------------------------------------------------------------
      INQUIRE (FILE = FILEIO,EXIST = FEXIST)
      IF (.NOT. FEXIST) THEN
         WRITE(*,1000) FILEIO
 1000 FORMAT(//,15X,'Error In Input Module and Temp. File ',A12,//,
     &          15X,'Please Check Input Conditions',/)
         STOP
      ENDIF
      IF (TYPEIO .EQ. 'I') THEN
        OPEN (LUNIO, FILE = FILEIO,STATUS = 'UNKNOWN',IOSTAT=ERRNUM)
        READ (LUNIO,1100) RNMODE,NREP,EXPNO,TRTNO,TRTALL
 1100   FORMAT(29X,A1,4(1X,I5))
        RNMODE = UPCASE(RNMODE)
        CLOSE(LUNIO)
      ENDIF
      WRITE(REPARG(1:6),'(I6)') NREP
      MODELX = MODELX(1:IPX) // BLANK // FILEIO // BLANK // TYPEIO //
     &           BLANK // RNMODE // BLANK // REPARG

D     WRITE(200,60) MODELX(1:IPX), FILEIO, TYPEIO,        !chp
D    &   RNMODE, REPARG                                   !chp
D  60 FORMAT(/,'Runtime arguments passed to MODEL:',      !chp
D    &   /,'MODELX: ',A100,                               !chp
D    &   /,'FILEIO: ',A12,                                !chp
D    &   /,'TYPEIO: ',A1,                                 !chp
D    &   /,'RNMODE: ',A1,                                 !chp
D    &   /,'REPARG: ',A6)                                 !chp
D     print *,'Ready to call MODEL'                       !chp

C-----------------------------------------------------------------------
C    Run selected model
C-----------------------------------------------------------------------
      I = SYSTEM(MODELX)
      IF (I .GT. 0) STOP 'Could not run MODEL module'
C-----------------------------------------------------------------------
C    Check to see if all treatments have been run for RNMODE = 'A'
C-----------------------------------------------------------------------
      IF (RNMODE .EQ. 'A' .AND. TRTNO .GE. TRTALL) RNMODE = 'I'
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      IF (INDEX('NQGSF',RNMODE) .GT. 0) THEN
        DONE = .TRUE.
      ELSE IF (INDEX('I',RNMODE) .GT. 0) THEN
        WRITE(*,1700)
 1700   FORMAT(/,1X,'Do you want to run more simulations ? ',
     &         /,1X,'Y or N ? [Default = "N"] ===> ',$)
        READ (5,1800) ANS
 1800   FORMAT(A1)
        ANS = UPCASE(ANS)
        IF (ANS .NE. 'Y') DONE = .TRUE.
      ENDIF
      END DO

D	CLOSE (200)                                         !chp

      END PROGRAM MODELDRV
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      CHARACTER*1 FUNCTION UPCASE (INCHAR)

      IMPLICIT  NONE

      CHARACTER INCHAR*1
      INTEGER   CHAVAL

      CHAVAL = ICHAR(INCHAR)

      IF ((CHAVAL .LE. 122) .AND. (CHAVAL .GE. 97)) THEN
         UPCASE = CHAR(CHAVAL-32)
       ELSE
         UPCASE = INCHAR
      ENDIF

      END
