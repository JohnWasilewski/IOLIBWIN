      FUNCTION R2CHAR(RVALU,NCHAR,FORM)
C     ---------------------------------
C     jw / 01-06-04  draft
C     jw / 29-07-14  last amended

C     Returns left-justified string equivalent of real, RVALU.
C     Also returns the string length (digits+sign(if any)).

      CHARACTER*16 R2CHAR
      CHARACTER*6  FORM

      WRITE(R2CHAR,'('//TRIM(AdjustL(FORM))//')') RVALU

      R2CHAR=AdjustL(R2CHAR)

C     STRIP TRAILING ZEROS

      DO iPOS=LEN_TRIM(R2CHAR) ,1, -1
         IF(R2CHAR(iPos:iPos).EQ.'0') THEN
            R2CHAR(iPos:iPos)=' '
         ELSE
            EXIT
         END IF
      END DO

      NCHAR=LEN_TRIM(R2CHAR)

      RETURN
      END
