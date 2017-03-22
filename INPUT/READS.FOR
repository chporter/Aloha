C=======================================================================
C  FIND, Subroutine, J.W.Jones, 01/03/91
C  Finds appropriate SECTION in a file of logiacl unit number LUNUM by
C  searching for a 6-character NAME at beginning of each line.
C-----------------------------------------------------------------------
C  INPUT : LUNUM  - logical unit number of the file to read
C          NAME  - 6-character variable name of section to find
C  OUTPUT: LNUM  - Line number of the file currently being read
C          FOUND - Indicator of completion of find routine
C                    0 - End-of-file encountered i.e. name not found
C                    1 - NAME was found
C  LOCAL :
C  IFILE : LUNUM
C  NOTES : Modified N.B. Pickering, 08/27/91
C=======================================================================

      SUBROUTINE FIND(LUNUM,NAME,LNUM,FOUND)

      IMPLICIT NONE
      INTEGER FOUND,I,LNUM,LUNUM
      CHARACTER SECTION*6,NAME*6,UPCASE*1
C
C     Initialization.
C
      FOUND = 0
      LNUM  = 1
      DO I = 1, LEN(NAME)
         NAME(I:I) = UPCASE(NAME(I:I))
      END DO
C
C     Loop to read through data file.
C
   10 IF (.TRUE.) THEN
         READ(LUNUM,'(A)',END=20) SECTION
         DO I = 1,LEN(SECTION)
            SECTION(I:I) = UPCASE(SECTION(I:I))
         END DO
C
C        String found, set FOUND to 1, and exit loop.
C
         IF (NAME .EQ. SECTION) then
            FOUND = 1
            GOTO 20
C
C           String not found, set FOUND to 0.
C
          ELSE
            FOUND = 0
         ENDIF

         LNUM = LNUM + 1
         GOTO 10
      ENDIF

   20 RETURN
      END

      SUBROUTINE IGNORE(LUN,LINEXP,ISECT,CHARTEST)
C
C----------------------------------------------------------------------------
C       PURPOSE: To read lines as an n-character variable and check it
C                for a blank line or for a comment line denoted by ! in col 1.
C       INPUTS:  LUN - Logical unit number of the file to be read
C                LINEXP - Starting line number at which this routine begins to
C                         read the file
C       OUTPUTS: LINEXP - Line number last read by the routine
C                ISECT - Indicator of completion of IGNORE routine
C                        0 - End of file encountered
C                        1 - Found a good line to read
C                        2 - End of Section in file encountered, denoted by *
C                            in column 1
C                CHARTEST - n-character variable containing the contents of
C                           the last line read by the IGNORE routine
C----------------------------------------------------------------------------
C
      CHARACTER BLANK*(80),CHARTEST*(*)
      INTEGER   LUN,LINEXP,ISECT
      DATA BLANK/'                                                    '/

      ISECT = 1
 30   READ(LUN,'(A)',ERR=70,END=70)CHARTEST
      LINEXP = LINEXP + 1
C     Check to see if all of this section has been read
      IF(CHARTEST(1:1) .EQ. '*' )THEN
C        End of section encountered
         ISECT = 2
         RETURN
      ENDIF
C
C     Check for blank lines and comments (denoted by ! in column 1)
      IF(CHARTEST(1:1).NE.'!' .AND. CHARTEST(1:1).NE.'@') THEN
         IF(CHARTEST(1:80).NE.BLANK)THEN
C           FOUND A GOOD LINE TO READ
            RETURN
         ENDIF
      ENDIF
      GO TO 30
C     To read the next line
 70   ISECT = 0
      RETURN
      END

C=======================================================================
C  HFIND, Subroutine  GPF 7/95
C  Finds appropriate HEADER in a file of logical unit number LUNUM
C  by searching for a 5-character NAME following the '@' at the
C  beginning of a header line
C-----------------------------------------------------------------------
C  INPUT  : LUNUM  logical unit of file to read
C           NAME   variable name of header section to find (5-char)
C  OUTPUT : LNUM   line number of file currently read
C           ISECT  return status of find routine
C                  0  - EOF, name not found
C                  1  - NAME found
C                  2  - End of section encountered, denoted by *
C=======================================================================
      SUBROUTINE HFIND(LUNUM,NAME,LNUM,ISECT)

      IMPLICIT NONE
      INTEGER ISECT,I,LNUM,LUNUM
      CHARACTER HEADER*5,NAME*(*),UPCASE*1,LINE*128
C
C     Initialization, save initial line
C
      ISECT = 1
      DO I = 1, LEN(NAME)
         NAME(I:I) = UPCASE(NAME(I:I))
      END DO

C     Loop to read through data file.

   10  IF (.TRUE.) THEN
         READ(LUNUM,'(A)',ERR=20,END=20) LINE
         LNUM = LNUM + 1

C     End of section

         IF (LINE(1:1) .EQ. '*') THEN
            ISECT = 2
            RETURN
         ENDIF

C     Header line

         IF (LINE(1:1) .EQ. '@') THEN
            HEADER='     '
            DO I=2,LEN(LINE)
               IF (LINE(I:I) .NE. ' ') THEN
                  LINE(I:I) = UPCASE(LINE(I:I))
               ENDIF
            ENDDO
            DO I=2,(LEN(LINE)-LEN(NAME)+1)
               HEADER(1:LEN(NAME)) = LINE(I:(I+LEN(NAME)-1))
               IF (HEADER(1:LEN(NAME)) .EQ. NAME) THEN
                 ISECT = 1
                 RETURN
               ENDIF
            ENDDO
         ENDIF
         GOTO 10
      ENDIF
   20 ISECT = 0
      RETURN
      END


