      CHARACTER*16 FUNCTION I2CHAR(IVALU,nChars)
C     ------------------------------------------
C     jw / 01-06-04  draft
C     jw / 20-05-12  last amended

C     Returns left-justified string equivalent of integer, IVALU.
C     Also returns the string length (digits+sign(if any)).

      INTEGER, INTENT(IN)            :: IVALU
      OPTIONAL                          nChars

C     I2CHAR=REPEAT(' ',16)
      WRITE(I2CHAR,'(I8,8X)') IVALU
      I2CHAR=TRIM(ADJUSTL(I2CHAR))

      IF (PRESENT(nChars)) THEN
         nChars=LEN_TRIM(I2CHAR)
      END IF

      RETURN
      END