      CHARACTER*1 FUNCTION FINDCHR(STRING,INSET,OKCHARS,LTRIM)
C     ---------------------------------------------------------
C     jw / 01-05-04  orig. draft commenced
C     jw / 03-12-08  last rev.

C     Scans STRING and returns the first character found that is
C     also among the characters in OKCHARS.
C
C     Returns the char position where found as INSET.
C     If not found then returns INSET=0 and FINDCHR=' '.
C
C     If LTRIM=1 then returns STRING with leading chars
C     all removed up to and including the A1INPUT found.

      INCLUDE  'CfgTyp.inc'
      INCLUDE  'asctyp.inc'

      CHARACTER STRING*(*), OKCHARS*(*)
      INTEGER   INSET,NOKCHR,LTRIM,YES

      DATA      YES / 1 /

      LSTR  = LEN(STRING)
      NOKCHR  = LEN(OKCHARS)

      FINDCHR = ' '
      INSET = LSTR+1
      DO 10 ICHAR=NOKCHR,1,-1
         INDEN  = INDEX(STRING(1:LSTR),OKCHARS(ICHAR:ICHAR))
10       IF (INDEN.GT.0) INSET = MIN(INSET,INDEN)

      IF(INSET.GT.LSTR) INSET=0
      IF(INSET.GT.0) FINDCHR=STRING(INSET:INSET)

      IF(LTRIM.EQ.YES) THEN
         INSTR=INSET+1
C        IF(STRING(INSTR:INSTR).EQ.',') INSTR=INSTR+1
C        IF(STRING(INSTR:INSTR).EQ.' ') INSTR=INSTR+1
         STRING = STRING(INSTR:LSTR)
         END IF 

      RETURN
      END
