      CHARACTER*160 FUNCTION COMMAS(STRING)
C     -------------------------------------
C     jw / 21-11-04  orig. draft commenced
C     jw / 18-12-08  re-written
C     jw / 18-03-12  re-re-written
C     jw / 12-04-12  last rev.

C     Strips leading/training blanks.
C     Substitutes a single comma for each embedded string of a comma
C     and one or more blanks.
C     Strips trailing zeros directly in front of each comma
C------------------------------------------------------------------------/

      CHARACTER (LEN=*)    :: STRING
      LOGICAL SkipSPC
      INTEGER   LENGTH

      STRING = TRIM(STRING)

      LENGTH = LEN(STRING)
      COMMAS = TRIM(REPEAT(' ',160))


C     FIRST, SKIP LEADING BLANKS

      DO iPOS=1,LENGTH
         IF(STRING(iPOS:iPOS).NE.' ') EXIT
      END DO

C     NEXT, COPY STRING to COMMAS, SKIPPING BLANKS DIRECTLY AFTER COMMAS

      SkipSPC = .FALSE.
      iCOM=1

      DO iPOS=iPOS,LENGTH
         IF(SkipSPC .AND. STRING(iPOS:iPOS).EQ.' ') CYCLE
         ! Copy one character
         COMMAS(iCOM:iCOM)=STRING(iPOS:iPOS)
         ! Just copied a character, so don't skip any more spaces
         SkipSPC=.FALSE.
         iCOM=iCOM+1
         ! Keep doing so until a comma has been copied
         IF(STRING(iPOS:iPOS).NE.',')THEN
            CYCLE
         ELSE
            ! Just copied a comma, so add a space after it in COMMAS
            ! but skip spaces immed. after it in STRING
            SkipSPC = .TRUE.
            iCOM=iCOM+1
            COMMAS(iCOM:iCOM)=' '
            CYCLE
         END IF
         SkipSPC = .FALSE.
      END DO

      LENGTH = LEN_TRIM(COMMAS)

C     FINALLY, STRIP CONTIGUOUS TRAILING ZEROS IN REAL NUMBERS
C     IMMEDIATELY BEFORE COMMAS

      DO iPOS=LENGTH,1, -1
         ! Is this a comma?
         IF(COMMAS(iPOS:iPOS).EQ.',') THEN
            !Yes, it's a comma, so check the characters before it for
            !the nearest decimal point and the nearest comma.  
            !If a decimal is closer then number immediately before 
            !Commas(iPOS:iPOS) has to be a real number, in which case, 
            !strip trailing zeros
            NearestDecimal=INDEX(Commas(1:iPOS-1),'.',BACK=.TRUE.)
            NearestComma=INDEX(Commas(1:iPOS-1),',',BACK=.TRUE.)
            IF(NearestDecimal.GT.NearestComma) THEN
               iZER=iPOS
               LENGTH = LEN_TRIM(COMMAS)
               DO WHILE (COMMAS(iZER-2:iZER-1).EQ.'00')
                  iZER=iZER-1
               END DO
               IF(iZER.NE.iPOS)
     +         COMMAS(iZER:LENGTH)=
     +         COMMAS(iPOS:LENGTH)//repeat(' ',Ipos-Izer)
            END IF
         END IF
      END DO

      RETURN
      END
