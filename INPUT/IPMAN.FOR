C=======================================================================
C  IPIRR, Subroutine
C
C  Determines irrigation application for a simulation
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/93 GH  Written
C  05/28/93 PWW Header revision and minor changes
C
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNIR,YRSIM,ISWWAT,NIRR,EFFIRX,DSOILX,THETCX
C           IEPTX,IOFFX,IAMEX,NAPW,TOTAPW,AIRAMX,IDLAPL,IRRCOD,AMT
C
C  LOCAL  : IDLAPL,ISECT,LINEXP,IFIND,LN,J,ERRNUM,AMT,ERRKEY,IRRCOD
C           CHARTEST
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : FIND ERROR IGNORE
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPIRR (LUNEXP,FILEX,LNIR,YRSIM,ISWWAT,
     &           NIRR,EFFIRX,DSOILX,THETCX,IEPTX,IOFFX,IAMEX,LNSIM,
     &           NAPW,TOTAPW,AIRAMX,IDLAPL,IRRCOD,AMT,IIRV,IIRRI)

      IMPLICIT NONE

      INTEGER      LNIR,NIRR,LUNEXP,IDLAPL(150),ISECT,LINEXP,LNSIM
      INTEGER      YRSIM,IFIND,LN,J,ERRNUM,NAPW,IIRV(150),IRRCD

      REAL         AMT(150),DSOILX,THETCX,IEPTX,EFFIRX,TOTAPW,AIRAMX

      CHARACTER*1  ISWWAT,IIRRI
      CHARACTER*5  IRRCOD(150),IOFFX,IAMEX
      CHARACTER*6  FINDCH,ERRKEY
      CHARACTER*12 FILEX
      CHARACTER*80 CHARTEST

      PARAMETER (ERRKEY='IPIRR ')

                 FINDCH='*IRRIG'
C
C     Set default values in case section or data are missing from file EXP
C
      EFFIRX = 1.00
      NIRR   = 0
      NAPW   = 0
      TOTAPW = 0.0
      THETCX = 0.0
      DSOILX = 0.0
      AIRAMX = 0.0
      IOFFX  = 'GS000'
      IAMEX  = 'IR001'

      DO J = 1, 150
         IDLAPL(J) = 0
         AMT(J)    = 0.0
         IRRCOD(J) = '     '
      END DO
      IF (LNIR .GT. 0) THEN
         IF (ISWWAT .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
            IIRRI  = 'R'
            ISWWAT = 'Y'
         ENDIF
         NIRR = 1
         CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,55,IOSTAT=ERRNUM) LN
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (LN .NE. LNIR) GO TO 50
            READ (CHARTEST,55,IOSTAT=ERRNUM) LN,EFFIRX,DSOILX,
     &                        THETCX,IEPTX,IOFFX,IAMEX,AIRAMX
            IF (AIRAMX .LT. 0.0) AIRAMX = 0.0
            IF (EFFIRX .LE. 0.0) EFFIRX = 1.0
            IF (DSOILX .LE. 0.0) DSOILX = 30.0
            IF (THETCX .LE. 0.0) THETCX = 75.0
            IF (IOFFX(1:3) .EQ. '-99' .OR. IOFFX(3:5) .EQ. '-99')
     &          IOFFX = 'GS000'
            IF (IAMEX(1:3) .EQ. '-99' .OR. IAMEX(3:5) .EQ. '-99')
     &          IAMEX = 'IR001'
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
          ELSE
            CALL ERROR (ERRKEY,2,FILEX,LINEXP)
         ENDIF
C
C        Read different IRR amounts for the selected IRR level
C
 70      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (LN .GT. LNIR) GO TO 120
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN,IDLAPL(NIRR),
     &                                IRRCOD(NIRR),AMT(NIRR),IIRV(NIRR)
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF ((IDLAPL(NIRR) .LT.  0) .OR.
     &         (IIRRI .EQ. 'R' .AND. MOD(IDLAPL(NIRR),1000) .GT. 366))
     &         THEN
               CALL ERROR (ERRKEY,10,FILEX,LINEXP)
            ENDIF
            IF (IIRRI .EQ. 'R' .AND. IDLAPL(NIRR) .LT. YRSIM) GO TO 70
            IF ((AMT(NIRR) .LT. -1.0) .OR. (AMT(NIRR) .GT. 99999.)) THEN
               CALL ERROR (ERRKEY,11,FILEX,LINEXP)
            ENDIF
            READ (IRRCOD(NIRR)(3:5),'(I3)',IOSTAT=ERRNUM) IRRCD
            IF (IRRCD .LT. 1 .OR. IRRCD .GT. 10 .OR.
     &          ERRNUM .NE. 0) THEN
                CALL ERROR (ERRKEY,12,FILEX,LINEXP)
            ENDIF
            IF ((IRRCOD(NIRR)(3:5)) .NE. '007' .AND.
     &          (IRRCOD(NIRR)(3:5)) .NE. '008' .AND.
     &          (IRRCOD(NIRR)(3:5)) .NE. '009') THEN
                NAPW   = NAPW + 1
                IF (AMT(NAPW) .GT. 0.0) THEN
                  TOTAPW = TOTAPW + AMT(NAPW)
                ENDIF
            ENDIF
            NIRR = NIRR + 1
            IF (NIRR .GE. 151) GO TO 120
          ELSE
            GO TO 120
         ENDIF
         GO TO 70
      ENDIF

 120  CONTINUE
      REWIND (LUNEXP)
      NIRR = MAX((NIRR - 1),0)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 55   FORMAT (I3,F5.0,3(1X,F5.0),2(1X,A5),1X,F5.0)
 60   FORMAT (I3,I5,1X,A5,1X,F5.0,4X,I2)
      END

C=======================================================================
C  IPRES, Subroutine
C
C  Determines residue application for a simulation
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNRES,RESDAY,RESCOD,RESIDUE,RINP,DEPRES,
C           RESN,RESP,RESK,NARES,RESAMT,ISWNIT,YRSIM,ISWPHO,ISWPOT
C
C  LOCAL  : ERRKEY,CHARTEST,ISECT,LINEXP,ERRNUM,J,IFIND,LN
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : FIND IGNORE ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPRES (LUNEXP,FILEX,LNRES,RESDAY,RESCOD,RESIDUE,
     &     RINP,DEPRES,RESN,RESP,RESK,NARES,RESAMT,ISWNIT,YRSIM,
     &     ISWPHO,ISWPOT,IRESI,ISWWAT,RMET,LNSIM)

      IMPLICIT     NONE

      CHARACTER*1  ISWNIT,ISWPHO,ISWPOT,IRESI,ISWWAT
      CHARACTER*5  RESCOD(10),RMET(10)
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*12 FILEX
      CHARACTER*80 CHARTEST

      INTEGER      LNRES,LUNEXP,ISECT,LINEXP,RESDAY(10),NRESAP
      INTEGER      ERRNUM,J,IFIND,LN,NARES,YRSIM,LNSIM

      REAL         RESN(10),RESP(10),RESK(10),RESIDUE(10),RINP(10),
     &             DEPRES(10),RESAMT

      PARAMETER   (ERRKEY ='IPRES ')

                   FINDCH ='*RESID'

      NRESAP = 0
      NARES  = 0
      RESAMT = 0.0

      DO J = 1, 10
         RESCOD(J)  = '     '
         RMET(J)    = '     '
         RESDAY(J)  = 0
         RESIDUE(J) = 0.0
         RINP(J)    = 100.0
         DEPRES(J)  = 0.0
         RESN(J)    = 0.0
         RESP(J)    = 0.0
         RESK(J)    = 0.0
      END DO

      IF (LNRES .GT. 0) THEN
         IF (ISWNIT .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
           ISWNIT = 'Y'
         ENDIF
         IF (ISWNIT .EQ. 'Y' .AND. LNSIM .EQ. 0) THEN
           IRESI  = 'R'
         ENDIF
         IF (ISWWAT .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
           ISWWAT = 'Y'
         ENDIF
         NRESAP = 1
         CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)

         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (LN .NE. LNRES) GO TO 50
C
C           Read different residue types and amounts
C
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN,RESDAY(NRESAP),
     &             RESCOD(NRESAP),RESIDUE(NRESAP),RESN(NRESAP),
     &             RESP  (NRESAP),RESK   (NRESAP),RINP(NRESAP),
     &             DEPRES(NRESAP),RMET(NRESAP)
            RESN(NRESAP)    = MAX (RESN(NRESAP),0.00)
            RESP(NRESAP)    = MAX (RESP(NRESAP),0.00)
            RESK(NRESAP)    = MAX (RESK(NRESAP),0.00)
C
C           RESIDUE(NRESAP) = MAX (RESIDUE(NRESAP),1000.0)
C
C           Set minimum lower than 1000 KG .. PWW
C
C-PW        RESIDUE(NRESAP) = MAX (RESIDUE(NRESAP),10.0)
            RESIDUE(NRESAP) = MAX (RESIDUE(NRESAP),0.0)
            DEPRES(NRESAP)  = MAX (DEPRES (NRESAP),15.0)
            IF ((RESDAY(NRESAP) .LT. 0) .OR.
     &          (IRESI .EQ. 'R' .AND. MOD(RESDAY(NRESAP),1000)
     &           .GT. 366)) THEN
               CALL ERROR (ERRKEY,11,FILEX,LINEXP)
            ENDIF
            IF (IRESI .EQ. 'R' .AND. RESDAY(NRESAP) .LT. YRSIM) GO TO 50
            IF (RESIDUE(NRESAP) .LT. 0.0 .OR. RESIDUE(NRESAP)
     &           .GT. 99999.) THEN
               CALL ERROR (ERRKEY,11,FILEX,LINEXP)
            ENDIF
            IF ((RESN(NRESAP) .LT. 0) .OR.
     &          (RESN(NRESAP) .GT. 99999.)) THEN
               CALL ERROR (ERRKEY,12,FILEX,LINEXP)
            ENDIF
            IF (ISWPHO .EQ. 'Y') THEN
               IF ((RESP(NRESAP) .LT.  0) .OR.
     &             (RESP(NRESAP) .GT. 99999.)) THEN
                  CALL ERROR (ERRKEY,13,FILEX,LINEXP)
               ENDIF
            ENDIF
            IF (ISWPOT .EQ. 'Y') THEN
               IF ((RESK(NRESAP) .LT.  0) .OR.
     &             (RESK(NRESAP) .GT. 99999.)) THEN
                  CALL ERROR (ERRKEY,13,FILEX,LINEXP)
               ENDIF
            ENDIF
            IF (RINP(NRESAP) .LE. 0.0) THEN
                RINP(NRESAP) = 100.0
            ENDIF
            RESAMT = RESAMT + RESIDUE(NRESAP)
            NARES  = NARES  + 1
            NRESAP = NRESAP + 1
            IF (NRESAP .GE. 11) GO TO 120
          ELSE
            GO TO 120
         ENDIF
         GO TO 50
      ENDIF

 120  REWIND (LUNEXP)
      NRESAP = MAX((NRESAP-1),0)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,I5,1X,A5,1X,F5.0,3(1X,F5.0),2(1X,F5.0),1X,A5)

      END

C=======================================================================
C  IPFERT, Subroutine
C
C  Subroutine to read in fertilizer applications during season
C  To read *FERTILIZER section in the V3.5 input files
C-----------------------------------------------------------------------
C  Revision history
C
C  05/08/91 JWW Written for DSSAT v3 format
C  05/28/93 PWW Header revision and minor changes
C
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNFER,YRSIM,ISWNIT
C
C  LOCAL  : ISECT,LINEXP,LN,ERRKEY,CHARTEST,ERRNUM,J,IFIND
C
C  OUTPUT : NFERT,FDAY,IFTYPE,FERCOD,DFERT,ANFER,APFER,AKFER
C           ACFER,AOFER,FOCOD,ANFER
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : FIND,IGNORE
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPFERT (LUNEXP,FILEX,LNFER,YRSIM,ISWNIT,ISWPHO,
     &     NFERT,FDAY,IFTYPE,FERCOD,DFERT,ANFER,APFER,AKFER,ACFER,
     &     AOFER,FOCOD,TOTNAP,IFERI,ISWWAT,LNSIM)

      IMPLICIT     NONE

      CHARACTER*1  ISWNIT,ISWPHO,IFERI,ISWWAT
      CHARACTER*5  FERCOD(25),FOCOD(25),IFTYPE(25)
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*12 FILEX
      CHARACTER*80 CHARTEST

      INTEGER      LUNEXP,LNFER,YRSIM,NFERT,FDAY(25),IFFTYP,IFFCOD
      INTEGER      ISECT,LINEXP,ERRNUM,J,IFIND,LN,LNSIM
      REAL         DFERT(25),ANFER(25),APFER(25),AKFER(25),ACFER(25),
     &             AOFER(25),TOTNAP

      PARAMETER   (ERRKEY ='IPFERT')

                   FINDCH ='*FERTI'

      NFERT  = 0
      TOTNAP = 0.0

      DO J = 1, 25
         DFERT(J) = 0.0
         ANFER(J) = 0.0
         APFER(J) = 0.0
         AKFER(J) = 0.0
         ACFER(J) = 0.0
         AOFER(J) = 0.0
      END DO

      IF (LNFER .GT. 0) THEN
         IF (ISWNIT .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
            ISWNIT = 'Y'
            IFERI  = 'R'
         ENDIF
         IF (ISWWAT .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
           ISWWAT = 'Y'
         ENDIF
         LINEXP = 0
         NFERT  = 1
         CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)

         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (LN .NE. LNFER) GO TO 50
C
C           Read different FERT amounts for the selected FERT level
C
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN,FDAY(NFERT),
     &           IFTYPE(NFERT),FERCOD(NFERT),DFERT(NFERT),ANFER(NFERT),
     &           APFER (NFERT),AKFER (NFERT),ACFER(NFERT),AOFER(NFERT),
     &           FOCOD (NFERT)

            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF ((FDAY(NFERT) .LT. 0) .OR.
     &         (IFERI .EQ. 'R' .AND. MOD(FDAY(NFERT),1000) .GT. 366))
     &         THEN
               CALL ERROR (ERRKEY,10,FILEX,LINEXP)
            ENDIF
            IF (IFERI .EQ. 'R' .AND. FDAY(NFERT) .LT. YRSIM) GO TO 50
            IF ((DFERT(NFERT) .LT. 0) .OR.
     &         (DFERT(NFERT) .GT. 99999.)) THEN
               CALL ERROR (ERRKEY,11,FILEX,LINEXP)
            ENDIF
            IF ((ANFER(NFERT) .LT. 0) .OR.
     &         (ANFER(NFERT) .GT. 99999.)) THEN
               CALL ERROR (ERRKEY,12,FILEX,LINEXP)
            ENDIF
            READ (IFTYPE(NFERT)(3:5),'(I3)',IOSTAT=ERRNUM) IFFTYP
            IF (IFFTYP .LT. 1 .OR. IFFTYP .GE. 60 .OR.
     &          ERRNUM .NE. 0) THEN
               CALL ERROR (ERRKEY,14,FILEX,LINEXP)
            ENDIF
            READ (FERCOD(NFERT)(3:5),'(I3)',IOSTAT=ERRNUM) IFFCOD
            IF (IFFCOD .LT. 1 .OR. IFFCOD .GT. 18 .OR.
     &          ERRNUM .NE. 0) THEN
               WRITE(FERCOD(NFERT)(3:5),'(A3)') '001'
            ENDIF
            IF (ISWPHO .EQ. 'Y') THEN
               IF ((APFER(NFERT) .LT. 0) .OR.
     &             (APFER(NFERT) .GT. 99999.)) THEN
                  CALL ERROR (ERRKEY,13,FILEX,LINEXP)
               ENDIF
            ENDIF
            TOTNAP = TOTNAP + ANFER(NFERT)
            NFERT  = NFERT  + 1
            IF (NFERT .GE. 26) GO TO 120
          ELSE
            GO TO 120
         ENDIF

         GO TO 50
      ENDIF

 120  REWIND (LUNEXP)
      NFERT = MAX((NFERT - 1),0)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,I5,1X,A5,1X,A5,6(1X,F5.0),1X,A5)

      END

C=======================================================================
C  IPHAR, Subroutine
C
C  Subroutine to read in harvest management in DSSAT 3.0
C  To read *HARVE section in the V3.0 input files
C-----------------------------------------------------------------------
C  Revision history
C
C  05/08/91 JWW Written for DSSAT v3 format
C  05/28/93 PWW Header revision and minor changes
C
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNHAR,YEAR
C
C  LOCAL  : LN,ERRKEY,CHARTEST,ERRNUM,J,IFIND
C
C  OUTPUT : HDATE,HSTG,HCOM,HSIZ,HPC,NHAR
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : FIND,IGNORE
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPHAR (LUNEXP,FILEX,LNHAR,HDATE,HSTG,HCOM,HSIZ,HPC,
     &                  NHAR,IHARI,YRSIM,CROP,HBPC)

      IMPLICIT     NONE

      CHARACTER*1  IHARI
      CHARACTER*2  CROP
      CHARACTER*5  HSTG(3),HCOM(3),HSIZ(3)
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*12 FILEX
      CHARACTER*80 CHARTEST

      INTEGER      LNHAR,LUNEXP,ISECT,LINEXP,HDATE(3),NHAR
      INTEGER      ERRNUM,J,IFIND,LN,YRSIM

      REAL         HPC(3),HBPC(3)

      PARAMETER   (ERRKEY='IPHAR ')

                   FINDCH='*HARVE'

      NHAR  = 0

      DO J = 1, 3
         HSTG(J)  = '     '
         HCOM(J)  = '     '
         HSIZ(J)  = '     '
         HPC(J)   = 100.0
         HDATE(J) = -99
      END DO

      IF (LNHAR .EQ. 0) GO TO 120
      NHAR = 1
      CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
      IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)

 50   CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)

      IF (ISECT .EQ. 1) THEN
         READ (CHARTEST,60,IOSTAT=ERRNUM) LN
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
         IF (LN .NE. LNHAR) GO TO 50
C
C        Read several lines of harvest details
C
         READ (CHARTEST,60,IOSTAT=ERRNUM) LN,HDATE(NHAR),HSTG(NHAR),
     &                  HCOM(NHAR),HSIZ(NHAR),HPC(NHAR),HBPC(NHAR)
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
         IF ((HDATE(NHAR) .LT.  0) .OR.
     &       (IHARI .EQ. 'R' .AND. MOD(HDATE(NHAR),1000) .GT. 366))
     &       THEN
             CALL ERROR (ERRKEY,10,FILEX,LINEXP)
         ENDIF
         IF (IHARI .EQ. 'R' .AND. HDATE(NHAR) .LT. YRSIM) GO TO 50
         IF (HPC(NHAR) .LT. 0.0) THEN
             HPC(NHAR) = 100.0
         ENDIF
         IF (HBPC(NHAR) .LT. 0.0) THEN
             HBPC(NHAR) = 0.0
         ENDIF
         NHAR = NHAR + 1
         IF (NHAR .GE. 4) GO TO 120
       ELSE
         GO TO 120
      ENDIF

      GO TO 50
 120  REWIND (LUNEXP)

      IF ((INDEX('CSPT',CROP)) .GT. 0) THEN
        IF (HDATE(1) .LT. 0) THEN
           CALL ERROR (ERRKEY,13,FILEX,LINEXP)
        ENDIF
        IF (IHARI .EQ. 'A') THEN
           CALL ERROR (ERRKEY,14,FILEX,LINEXP)
        ENDIF
      ENDIF

      NHAR = MAX (0,NHAR-1)
      IF (LNHAR .EQ. 0 .AND. IHARI .NE. 'M' .AND. IHARI .NE. 'A') THEN
         CALL ERROR (ERRKEY,1,FILEX,LINEXP)
      ENDIF
      IF (IHARI .EQ. 'G' .AND. HSTG(1) .EQ. '     ') THEN
         CALL ERROR (ERRKEY,3,FILEX,LINEXP)
      ENDIF
      IF (IHARI .EQ. 'R' .AND. HDATE(1) .EQ. 0) THEN
         CALL ERROR (ERRKEY,4,FILEX,LINEXP)
      ENDIF
      IF (IHARI .EQ. 'D' .AND. HDATE(1) .EQ. 0) THEN
         CALL ERROR (ERRKEY,5,FILEX,LINEXP)
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,I5,3(1X,A5),2(1X,F5.0))

      END

C=======================================================================
C  IPCUL, Subroutine
C
C  Reads parameters related cultivar selection from FILEX file
C  Reads new IBSNAT formats
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written, P. Wilkens                           P.W.W       4-12-93
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNCU
C
C  LOCAL  : LN,CHARTEST,ERRKEY,LINEXP,ISECT,IFIND,ERRNUM
C
C  OUTPUT : CROP,VARNO,VRNAME
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : FIND IGNORE ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPCUL (LUNEXP,FILEX,LNCU,CROP,VARNO)

      IMPLICIT     NONE

      INTEGER      LNCU,LUNEXP,ISECT,LINEXP
      INTEGER      IFIND,LN,ERRNUM

      CHARACTER*2  CROP
      CHARACTER*6  ERRKEY,VARNO,FINDCH
      CHARACTER*12 FILEX
      CHARACTER*16 CNAME
      CHARACTER*80 CHARTEST

      PARAMETER (ERRKEY='IPCUL ')

                 FINDCH='*CULTI'

      IF (LNCU .GT. 0) THEN
         CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,55,IOSTAT=ERRNUM) LN
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (LN .NE. LNCU) GO TO 50
            IF (LN .GT. LNCU) GO TO 120
            READ (CHARTEST,55,IOSTAT=ERRNUM) LNCU,CROP,VARNO,CNAME
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
          ELSE
            CALL ERROR(ERRKEY,2,FILEX,LINEXP)
         ENDIF
      ENDIF

 120  CONTINUE
      REWIND (LUNEXP)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 55   FORMAT (I3,A2,1X,A6,1X,A16)

      END
