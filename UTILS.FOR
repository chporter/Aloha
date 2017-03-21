C=======================================================================
C  UTILS, File, G. Hoogenboom, P.W. Wilkens and B. Baer
C  General utility functions
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/30/98 GH  Combine UTILS based on UPCASE, VERIFY, TABEX, and CLEAR
C=======================================================================

C=======================================================================
C  UPCASE, Function
C
C  Function to return the upper case of a lower case letter.  Otherwise
C  returns the same character
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       B.D.B       5-15-92
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : INCHAR
C
C  LOCAL  :
C
C  OUTPUT : INCHAR
C-----------------------------------------------------------------------
C  Called : INPUT READS IPEXP JULIAN
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  INCHAR :
C  CHAVAL :
C=======================================================================

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
C=======================================================================
C  VERIFY, Subroutine
C
C  I/O utility routine
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : LINE VALUE FLAG
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : IPECO IPSOIL IPVAR SECLI  SECROP SEFERT SEFREQ SEHARV SEINIT
C           SEIRR SEPLT  SERES SESOIL SEVAR  SEWTH  IPEXP  SETIME
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE VERIFY (LINE,VALUE,FLAG)

      IMPLICIT    NONE

      CHARACTER*1 DIGITS(13),LINE(80)
      INTEGER     JKNEW,JSIGNR,JTIME,JVALUE,NC,NDECML,I,J,JSIGN
      REAL        TFRAC,VALUE,FLAG

      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9',
     &            '+','-','.'/

      FLAG   = -1.0
      JKNEW  =    0
      JSIGNR =    1
      JTIME  =    0
      JVALUE =    0
      NC     =    0
      NDECML =   -1
      TFRAC  =  0.0
      VALUE  =  0.0
C
C     Check for CR/LF
C
      DO I = 1, 80
         IF (LINE(I) .NE. ' ') GO TO 100
      END DO
      !
      ! Nothing entered .. spaces or ÄÄÙ .. set FLAG to 1.0 and exit
      !
      FLAG = 1.0
      GO TO 1300

  100 NC     = NC + 1
      IF (NC .GT. 80) GO TO 1300
      IF (LINE(NC) .EQ. ' ') GO TO 100
      DO I = 1, 13
         J = I
         IF (LINE(NC) .EQ. DIGITS(I)) GO TO 300
      END DO
      GO TO 1200
  300 IF (J .LE. 10) GO TO 600
      IF (J .EQ. 13) GO TO 500
      JSIGN  = 1
      IF (J .EQ. 12) JSIGN = -1

C-----------------------------------------------------------------------
C***    IF SIGN IS REPEATED
C-----------------------------------------------------------------------

      IF (JKNEW .GT. 0) GO TO 1200
      JKNEW = 1
C-----------------------------------------------------------------------
C
C***    SIGN APPEARS AFTER THE DECIMAL POINT
C
C-----------------------------------------------------------------------
      IF (NDECML) 400,1200,900
  400 JSIGNR = JSIGN
      GO TO 900
  500 JKNEW  = 1
C-----------------------------------------------------------------------
C***    DECIMAL REPEATED
C-----------------------------------------------------------------------

      IF (NDECML .GE. 0) GO TO 1200
      NDECML = 0
      GO TO 900
  600 J = J - 1
      JKNEW = 1
      IF (NDECML) 700,800,900
  700 JVALUE = JVALUE*10 + J
      GO TO 900
  800 JTIME = JTIME + 1
      TFRAC = TFRAC + FLOAT(J)/(10.**JTIME)
  900 VALUE = FLOAT(JSIGNR*JVALUE)+FLOAT(JSIGNR)*TFRAC
 1000 NC    = NC + 1
      IF (NC .GT. 80) GO TO 1300
      IF (LINE(NC) .EQ. ' ') GO TO 1000
      DO I = 1, 13
         J = I
         IF (LINE(NC) .EQ. DIGITS(I)) GO TO 300
      END DO
 1200 FLAG = 1.0

 1300 DO I = 1, 80
         LINE(I) = ' '
      END DO

      END

C=======================================================================
C  TABEX, Function
C
C  Look up utility routine
C-----------------------------------------------------------------------
      FUNCTION TABEX(VAL,ARG,DUMMY,K)

      IMPLICIT NONE

      INTEGER K,J

      REAL VAL(K),ARG(K),DUMMY,TABEX

           DO 100  J = 2,K
           IF (DUMMY .GT. ARG(J)) GO TO 100
           GO TO 200
  100      CONTINUE
      J = K
  200 TABEX = (DUMMY-ARG(J-1))*(VAL(J)-VAL(J-1))/(ARG(J)-ARG(J-1))+VAL
     &     (J-1)

      END

C=======================================================================
C  CLEAR, Subroutine
C
C  Clears the screen using ANSI codes, sets color to white on blue
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2. Header revision and minor changes           P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : ESC
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : SESOIL SESIM SERES SEPLT SENSDM SENS SEFREQ SEWTH SEFERT SECLI
C           SEVAR SETIME IPVAR IPSOIL WTHMDI OPHARV OPDAY IPEXP SEIRR SEINIT
C           SEHARV SECROP INVAR INPUT
C
C  Calls  : None
C-----------------------------------------------------------------------

C                         DEFINITIONS
C
C  ESC    : ESC key codes
C=======================================================================

      SUBROUTINE CLEAR

      IMPLICIT  NONE

      CHARACTER ESC

      ESC = CHAR(27)

      WRITE  (*,100) ESC

100   FORMAT (1X,A1,'[2J',//////)

      END

C=======================================================================
C  HOME, Subroutine
C
C  Moves cursor to 0,0 ANSI and clears the screen
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                     P.W.W.      2- 9-93
C  2. Header revision and minor changes           P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : ESC
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : ERROR AUTPLT AUTHAR INPUT
C
C  Calls  : CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  ESC    : ESC key codes
C=======================================================================

      SUBROUTINE HOME

      IMPLICIT  NONE

      CHARACTER ESC

      ESC = CHAR(27)

      WRITE (*,100) ESC
      WRITE (*,200) ESC

100   FORMAT (1X,A1,'[2J')
200   FORMAT (1X,A1,'[0;0H')

      END

C=======================================================================
C  CURPOS, Subroutine
C
C  Moves cursor to specified row
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                     P.W.W.      2- 9-93
C  2. Header revision and minor changes           P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : ESC
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : INTRO
C
C  Calls  : NONE
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  ESC    : ESC key codes
C=======================================================================

      SUBROUTINE CURPOS (LINE)

      IMPLICIT  NONE

      CHARACTER ESC,LINE*2

      ESC = CHAR(27)

      WRITE (*,200) ESC,LINE

200   FORMAT (1X,A1,'[',A2,';0H')

      END

C=======================================================================
C  SIMEND, Subroutine
C
C  Clears the screen using ANSI codes, sets color to B/W
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                     P.W.W.      2-9-93
C  2. Header revision and minor changes           P.W.W.      2-9-93
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : ESC
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : ERROR AUTPLT AUTHAR CERES
C
C  Calls  : CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  ESC    : ESC key codes
C=======================================================================

      SUBROUTINE SIMEND

      IMPLICIT  NONE

      CHARACTER ESC

      ESC = CHAR(27)

      CALL CLEAR

      WRITE  (*,200) ESC
      WRITE  (*,100) ESC

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

100   FORMAT (1X,A1,'[2J')
200   FORMAT (1X,A1,'[37;40;0m')

      END
