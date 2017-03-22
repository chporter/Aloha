!      INTERFACE TO INTEGER*2 FUNCTION SYSTEM [C]
!     &   (STRING[REFERENCE])
!      CHARACTER*1 STRING
!      END
C=======================================================================
C  COPYRIGHT 1998 the University of Georgia, Griffin, Georgia
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  BATCH DRIVER, Main program
C
C  BATCH Driver to call input and model for CROPGRO and CERES models
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       G.H.        4-05-95
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
      PROGRAM      BATCHDRV

      USE DFPORT                                          !chp
      IMPLICIT     NONE

      CHARACTER*1   TYPEIO,RNMODE,BLANK,UPCASE
      CHARACTER*3   EXPARG,TRNARG
      CHARACTER*6   ERRKEY,REPARG
      CHARACTER*12  MODEL,FILEIO,INPUT,BATCH
      CHARACTER*72  PATHX                 !chp changed to 72 (was 52)
      CHARACTER*100 MODELX,INPUTX

      INTEGER       ERRNUM,LUNIO,TRTALL,TRTNO,EXPNO,NREP,LUNB
!chp  INTEGER*2     SYSTEM,I,IP,IPX
      INTEGER*2     I,IP,IPX                              !chp
	INTEGER iyr, imon, iday, ihr, imin, isec, i100th    !chp

      LOGICAL       DONE,FEXIST

      PARAMETER     (ERRKEY = 'MODELD')
      PARAMETER     (FILEIO = 'IBSNAT35.INP')
      PARAMETER     (LUNIO  = 20)
      PARAMETER     (LUNB   = 21)
      PARAMETER     (BLANK  = ' ')

      DONE = .FALSE.
      BATCH = 'BATCH.RUN'

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

!     PAUSE "Batch driver called"

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

D     WRITE(200,20) PATHX, INPUT, MODEL, TYPEIO, RNMODE   !chp                                    !chp
D  20 FORMAT(/,'Runtime arguments passed to driver:',     !chp
D    &   /,'PATHX:  ',A72,                                !chp
D    &   /,'INPUT:  ',A12,                                !chp
D    &   /,'MODEL:  ',A12,                                !chp
D    &   /,'TYPEIO: ',A1,                                 !chp
D    &   /,'RNMODE: ',A1)                                 !chp
D
D!     print *,'Read arguments for driver'                 !chp

C-----------------------------------------------------------------------
C    Delete previouse copies of IBSNAT35.INP
C-----------------------------------------------------------------------
      INQUIRE (FILE = FILEIO,EXIST = FEXIST)
      IF (FEXIST) THEN
          OPEN (LUNIO, FILE = FILEIO,STATUS = 'UNKNOWN',IOSTAT=ERRNUM)
          CLOSE (LUNIO,STATUS = 'DELETE')
      ENDIF
C-----------------------------------------------------------------------
C    Open BATCH file
C-----------------------------------------------------------------------
      INQUIRE (FILE = BATCH,EXIST = FEXIST)
      IF (FEXIST) THEN
          OPEN (LUNB, FILE = BATCH,STATUS = 'UNKNOWN',IOSTAT=ERRNUM)
      ELSE
         WRITE(*,800) BATCH
  800 FORMAT(//,15X,'Unable to locate Batch File ',A12,//,
     &          15X,'Please Check Path and Directory',/)
         STOP
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
      READ (LUNB,*,ERR=2000,END=2100) EXPNO,TRTNO
      WRITE(EXPARG(1:3),'(I3)') EXPNO
      WRITE(TRNARG(1:3),'(I3)') TRTNO
      RNMODE = 'N'
      INPUTX = INPUTX(1:IPX) // BLANK // MODEL // BLANK // FILEIO //
     &         BLANK  // TYPEIO // BLANK // RNMODE // BLANK //
     &         EXPARG // BLANK // TRNARG

D     WRITE(200,40) INPUTX(1:IPX), MODEL, FILEIO, TYPEIO,        !chp
D    &   RNMODE, EXPARG, TRNARG                           !chp
D  40 FORMAT(/,'Runtime arguments passed to INPUT:',      !chp
D    &   /,'INPUTX: ',A,                                  !chp
D    &   /,'MODEL:  ',A,                                  !chp
D    &   /,'FILEIO: ',A,                                  !chp
D    &   /,'TYPEIO: ',A,                                  !chp
D    &   /,'RNMODE: ',A,                                  !chp
D    &   /,'EXPARG: ',A,                                  !chp
D    &   /,'TRNARG: ',A)                                  !chp
D!     print *,'Ready to call INPUT'                       !chp
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
C
C-----------------------------------------------------------------------
      END DO
 2000 CONTINUE
      WRITE(*,2050) BATCH
 2050 FORMAT(//,15X,'Error while reading Batch File ',A12,//,
     &          15X,'Please Check BATCH file for illegal characters',/)
 2100 CONTINUE
      CLOSE(LUNB)

D	CLOSE (200)                                         !chp

      END PROGRAM BATCHDRV
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