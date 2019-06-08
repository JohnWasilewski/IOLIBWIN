      INTEGER FUNCTION LENSTR(STRING)
C     -------------------------------
C     Returns length of string ignoring trailing blanks
C
C     Clive Page/ 22-02-95 draft
C     jw / 31-10-01 last rev

      CHARACTER*(*) STRING

      DO 15, LENSTR = LEN(STRING),1, -1
15       IF(STRING(LENSTR:LENSTR) .NE. ' ')  RETURN

      END
