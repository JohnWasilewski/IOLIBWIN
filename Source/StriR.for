
      FUNCTION StriR(REAL)
C     ---------------------
C     jw / 30-01-12  draft
C     jw / 30-01-12  last revised

C     Returns the real argument as a 16-char left-justified string 

      REAL*8 REAL
      CHARACTER StriR*15

      WRITE(StriR,'(16X)')
      WRITE(StriR,'(F16.6)') REAL
      StriR=ADJUSTR(StriR)

      RETURN
      END
