      CHARACTER*80 FUNCTION SKIPCHR(STRING,INDENT,NBGCHR)
C     ---------------------------------------------------
C     jw / 15-06-04  orig. draft commenced
C     jw / 21-11-04  last rev.

C     Scans STRING and returns it with all leading 'NBG' chars removed.
C     Returns the offset to the 1st 'good' character as INDENT
C     If all chars are NBG then returns SKIPCHR=' ' and INDENT=0.
C
      USE CONFIG
C     INCLUDE  'CFGTYP.INC'
C     INCLUDE  'asctyp.inc'

      CHARACTER STRING*(*), NBGCHR*(*)
      INTEGER   INDENT,OKCHARS,NBG,NNBGCHR,LENGTH,LEN,YES

      DATA      YES / 1 /

      LENGTH  = LEN(STRING)
      NNBGCHR = LEN(NBGCHR)

      POSNBG  = 0
      OKCHARS = 0
      DO 10 INDENT = 1,LENGTH
         DO 10 NBG=1,NNBGCHR
10          IF (INDEX(NBGCHR(1:NNBGCHR),STRING(INDENT:INDENT)).EQ.0)
     +      GO TO 11
      SKIPCHR=' '
      INDENT =0
      RETURN

11    CONTINUE

      DO 12 I=1,LENGTH
12       SKIPCHR(I:I) = ' '
      SKIPCHR = STRING(INDENT:LENGTH)

      RETURN
      END
