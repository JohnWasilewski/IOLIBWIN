      CHARACTER*80 FUNCTION LTRIM(STRING)
C     -----------------------------------
C     jw / 21-11-04  orig. draft commenced
C     jw / 26-12-08  last rev.

C     Strips leading blanks

      CHARACTER STRING*(*)
      INTEGER   INDENT,LENGTH,LEN

      WRITE(LTRIM,'(80X)')
      LENGTH  = LEN(STRING)

      DO 10 INDENT = 1,LENGTH
10       IF (STRING(INDENT:INDENT).NE.' ')
     +      GO TO 11
      RETURN

11    CONTINUE

	DO 12 I=1,LENGTH
12       LTRIM(I:I) = ' '

	LTRIM(1:LENGTH+1-INDENT) = STRING(INDENT:LENGTH)

      IF(INDENT.GT.1) THEN
         DO 22 I=LENGTH,1+LENGTH-INDENT
22         LTRIM(I:I) = ' '
         END IF

	RETURN
      END
