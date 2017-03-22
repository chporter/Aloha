C=======================================================================
C  ERROR, Subroutine, N.B. Pickering
C  Outputs error messages to screen from file ERROR.DAT
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  08/30/91 NBP Written
C  10/31/94 GH  Add option to read MODEL.ERR from model EXE path
C-----------------------------------------------------------------------
C  Input : ERRKEY,ERRNUM,FILE,LNUM)
C  Output: message to screen
C  Local :
C  Ifile : MODEL.ERR
C  FN/SUB: FIND
C=======================================================================

      SUBROUTINE ERROR (ERRKEY,ERRNUM,FILE,LNUM)

      IMPLICIT      NONE

      CHARACTER*(*) ERRKEY,FILE
      CHARACTER     AKEY*6,BLANK*80,KEY*6,LINE*80
      CHARACTER*72  PATHX
      CHARACTER*100 ERRORX

      INTEGER       ANUM,ERRNUM,LNUM
      INTEGER*2     IPX

      LOGICAL       FOUND,FEXIST

      PARAMETER     (BLANK = ' ')

C-SUN CALL GETARG(0,PATHX)
      CALL GETARG(0,PATHX,IPX)
      IPX = INDEX(PATHX,' ')
      IF (IPX .NE. 0) THEN
        IPX = IPX - 1
      ENDIF
      ERRORX = PATHX(1:(IPX-12)) // 'MODEL.ERR   '
      INQUIRE (FILE = ERRORX,EXIST = FEXIST)
      IF (FEXIST) THEN

         OPEN (100,FILE=ERRORX,STATUS='OLD')
C
C        Initialization
C
         FOUND = .FALSE.
         IF (ERRNUM .GT. 6000 .OR. ERRNUM .LT. 0) THEN
            KEY = 'MISC'
         ELSE
            KEY = ERRKEY
         ENDIF
C
C        Loop to search for error message in file MODEL.ERR.
C
         DO WHILE(.TRUE.)
           READ (100,'(A)',END=20) LINE
           AKEY = LINE(1:6)
           IF (AKEY .EQ. KEY) THEN
              READ (LINE,'(6X,I5)') ANUM
              IF (ANUM .EQ. ERRNUM) THEN
                 FOUND = .TRUE.
                 GOTO 20
              ENDIF
            ELSE
              FOUND = .FALSE.
           ENDIF
         ENDDO

   20    IF (FOUND) THEN
            WRITE (*,*)
   30       READ  (100,'(A)',END=20) LINE
            IF (LINE .NE. BLANK) THEN
               WRITE (*,*) LINE
               GOTO 30
            ENDIF
          ELSE
            WRITE (*,'(/A,I5/)')
     &    ' Unknown ERROR.                 Error number: ',ERRNUM
         ENDIF

         IF (FILE .EQ. ' ') THEN
            WRITE (*,'(2A/)') ' Error key: ',ERRKEY
          ELSE
            WRITE (*,'(3A,I5,2A/)')
     &    ' File: ',FILE,'   Line: ',LNUM,'   Error key: ',ERRKEY
         ENDIF
      ELSE
C                                                                       !BDB
C        Tell user that error file can not be found and give a          !BDB
C        generic error message.                                         !BDB
C                                                                       !BDB
         WRITE (*,50)  ERRORX,FILE,LNUM,ERRKEY                          !BDB
      ENDIF

      WRITE (*, *) CHAR(7)
      WRITE (*,60)
      READ  (*, *)
      STOP 99

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 50   FORMAT (' Could not locate error file:  ',A50,//,                 !BDB
     &        ' Error while reading input file.',//,                    !BDB
     &        ' File: ',A12,'   Line: ',I5,' Error key: ',A8,/)
 60   FORMAT (1X,'Please press < ENTER > key to continue ',2X,$)

      END

