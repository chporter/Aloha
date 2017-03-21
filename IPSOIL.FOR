C=======================================================================
C  IPSOIL, Subroutine
C
C  Read soil parameters from SOIL.PAR
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                     P.W.W.      6-15-94
C  2. Header revision and minor changes           P.W.W.      2- 7-93
C  2. Added switch common block, restructured     P.W.W.      2- 7-93
C-----------------------------------------------------------------------
C  INPUT  :
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : CERES
C
C  Calls  : SCREATE
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  PP3    :
C=======================================================================

      SUBROUTINE IPSOIL (CODRES,DMINR,RCN,DSNC,RDCHO,RDCEL,RDLIG,PRCHO,
     &                   PRCEL,PRLIG)

      IMPLICIT     NONE

      CHARACTER*10 ACRO(12)
      CHARACTER*5  CODRES
      CHARACTER    BLANK*1,CHAR*180,FILECC*92,IDETO*1
      INTEGER      J,LUNSOL,ERR,LINC
      REAL         DMINR,RCN,DSNC,RDCHO,RDCEL,RDLIG,PRCHO,PRCEL,PRLIG

      PARAMETER (BLANK = ' ')
      PARAMETER (IDETO = ' ')

      DATA ACRO/'DMINR     ','RTCNR    ','DSNCV     ','RE001     ',
     &          'RE002     ','RE003    ','RE004     ','RE005     ',
     &          'RE006     ','RE007    ','RE008     ','RE009     '/
      !
      ! Default Soil Parameter values
      !
      DMINR = 8.3E-05
      RCN   = 40.0
      DSNC  = 20.0
      RDCHO = 0.2
      RDCEL = 0.05
      RDLIG = 0.0095
      PRCHO = 0.20
      PRCEL = 0.70
      PRLIG = 0.10
c     RETURN
      FILECC = 'SOILN980.SOL'
      LUNSOL = 18
      LINC   = 0

      OPEN (LUNSOL,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      !
      ! If there is no file, create it in the default directory
      !
      IF (ERR .NE. 0) THEN
         CALL SCREATE (FILECC,ACRO)
         RETURN
      ENDIF

C-----------------------------------------------------------------------
C     Read Soil Parameters from SOIL.PAR
C-----------------------------------------------------------------------

      DO WHILE (.NOT. EOF (LUNSOL))
         READ (LUNSOL,'(A180)',IOSTAT=ERR) CHAR
         DO J = 1, 12
            IF (CHAR(4:13) .EQ. ACRO(J)) THEN
               IF (J .EQ. 1) THEN
                    READ (CHAR(15:26),'(E12.0)',IOSTAT=ERR) DMINR
                ELSEIF (J .EQ. 2) THEN
                    READ (CHAR(15:24),'(F9.0)',IOSTAT=ERR) RCN
                ELSEIF (J .EQ. 3) THEN
                    READ (CHAR(15:24),'(F9.0)',IOSTAT=ERR) DSNC
                ELSEIF ((J .GE. 4) .AND. (J .LE. 12)) THEN
                    IF (ACRO(J) .EQ. CODRES) THEN
                       READ (CHAR(15:62),'(6F8.4)',IOSTAT=ERR)
     &                      RDCHO,RDCEL,RDLIG,PRCHO,PRCEL,PRLIG
                    ENDIF
               ENDIF
            ENDIF
         END DO
      END DO

      CLOSE (LUNSOL)
      !
      ! Update SOILPAR.OUT File  .. debugging purposes.  Change the value
      ! of IDETO (above) to 'Y' to create the file SOILPAR.out
      !
      IF (IDETO .EQ. 'Y') THEN
         OPEN (UNIT=81, FILE='SOILPAR.OUT', STATUS='UNKNOWN',
     &         ACCESS='APPEND')
         WRITE (81,550) CODRES
         WRITE (81,600) DMINR
         WRITE (81,625) RCN
         WRITE (81,650) DSNC
         WRITE (81,700) RDCHO,RDCEL,RDLIG
         WRITE (81,800) PRCHO,PRCEL,PRLIG
         CLOSE (81)
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  500 FORMAT (/,'*SOIL PARAMETERS UTILIZED',/)
  550 FORMAT ('RESCOD - ',A5)
  600 FORMAT ('DIMNR  - ',E12.5)
  625 FORMAT ('RCN    - ',F5.1)
  650 FORMAT ('DSNC   - ',F5.1)
  700 FORMAT ('RDCHO  - ',F8.4,'  RDCEL - ',F8.4,'  RDLIG - ',F8.4)
  800 FORMAT ('PRCHO  - ',F8.4,'  PRCEL - ',F8.4,'  PRLIG - ',F8.4,/)

      END

C=======================================================================
C  SCREATE, Subroutine
C
C  Make soil parameter file SOIL.PAR
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                     P.W.W.      6-15-94
C  2. Header revision and minor changes           P.W.W.      2- 7-93
C  2. Added switch common block, restructured     P.W.W.      2- 7-93
C-----------------------------------------------------------------------
C  INPUT  :
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : IPCROP
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  PP3    :
C=======================================================================

      SUBROUTINE SCREATE (FILECC,ACRO)

      IMPLICIT     NONE

      INCLUDE     'NTRC1.BLK'
      INCLUDE     'NTRC2.BLK'

      CHARACTER*10 ACRO(12)
      CHARACTER*56 T(22)
      CHARACTER    FILECC*92
      INTEGER      I,LUNSOL,ERR

      DATA T/'! Model parameter file which externalizes many of the   ',
     1       '! coefficients needed for simulating the decomposition  ',
     1       '! of soil organic matter (1 pool) and organic matter    ',
     1       '! added as residue or manure (3 pools). If SOILN980.PAR ',
     1       '! does not exist in the data directory, it is created   ',
     1       '! upon the first run of the model. Definitions follow:  ',
     1       '!                                                       ',
     1       '! DMINR: Potential decomposition rate of SOM pool.      ',
     1       '!        Default value is .8300E-04 per day.            ',
     1       '! RTCNR: C/N ratio of initial root residue.             ',
     1       '!        Default = 40.0.                                ',
     1       '! DSNCV: Depth to which soil C (SCDD) and total N (SNDD)',
     1       '!        values are integrated for output to CARBON.OUT.',
     1       '!        Default value is 20.0 cm.                      ',
     1       '! RE001: First 3 values are the potential decomposition ',
     1       '!        rates of the carbohydrate, cellulose, and      ',
     1       '!        lignin pools; next 3 values are the relative   ',
     1       '!        proportions of carbohydrate, cellulose, and    ',
     1       '!        lignin in the residue or manure dry matter.    ',
     1       '!        Defaults=.2000 .0500 .0095 .2000 .7000 .1000   ',
     1       '!        Up to 9 different residue or manure types can  ',
     1       '!        be defined.                                    '/
      LUNSOL = 10

      OPEN (LUNSOL,FILE = FILECC, STATUS = 'NEW',IOSTAT=ERR)

      WRITE (LUNSOL,100)
      WRITE (LUNSOL,105) (T(I),I=1,22)
      WRITE (LUNSOL,110)
      WRITE (LUNSOL,140) 'DS', ACRO( 1),  DMINR
      WRITE (LUNSOL,145) 'DS', ACRO( 2),  RCN
      WRITE (LUNSOL,145) 'DS', ACRO( 3),  DSNC
      WRITE (LUNSOL,150) 'DS', ACRO( 4),
     &                    RDCHO,RDCEL,RDLIG,PRCHO,PRCEL,PRLIG
      WRITE (LUNSOL,150) 'DS', ACRO( 5),
     &                    RDCHO,RDCEL,RDLIG,PRCHO,PRCEL,PRLIG
      WRITE (LUNSOL,150) 'DS', ACRO( 6),
     &                    RDCHO,RDCEL,RDLIG,PRCHO,PRCEL,PRLIG
      WRITE (LUNSOL,150) 'DS', ACRO( 7),
     &                    RDCHO,RDCEL,RDLIG,PRCHO,PRCEL,PRLIG
      WRITE (LUNSOL,150) 'DS', ACRO( 8),
     &                    RDCHO,RDCEL,RDLIG,PRCHO,PRCEL,PRLIG
      WRITE (LUNSOL,150) 'DS', ACRO( 9),
     &                    RDCHO,RDCEL,RDLIG,PRCHO,PRCEL,PRLIG
      WRITE (LUNSOL,150) 'DS', ACRO(10),
     &                    RDCHO,RDCEL,RDLIG,PRCHO,PRCEL,PRLIG
      WRITE (LUNSOL,150) 'DS', ACRO(11),
     &                    RDCHO,RDCEL,RDLIG,PRCHO,PRCEL,PRLIG
      WRITE (LUNSOL,150) 'DS', ACRO(12),
     &                    RDCHO,RDCEL,RDLIG,PRCHO,PRCEL,PRLIG
      CLOSE (LUNSOL)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  100 FORMAT ('*SOIL NITROGEN PARAMETER FILE',/)
  105 FORMAT (22(A56,/))
  110 FORMAT ('*CHARACTERISTICS',/,'@C VARIABLE      VALUE')
  140 FORMAT (A2,1X,A10,1X,E12.4)
  145 FORMAT (A2,1X,A10,1X,F7.1)
  147 FORMAT (A2,1X,A10,1X,1X,20(F4.1))
  150 FORMAT (A2,1X,A10,1X,6(2X,F6.4))

      END
