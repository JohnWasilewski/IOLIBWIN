
      FUNCTION StriI(INTGR)
C     ---------------------
C     jw / 30-05-09  draft
C     jw / 30-05-09  last revised

C     Returns the integer argument as a 12-char left-justified string 

      INTEGER*4 INTGR
      CHARACTER StriI*12

      WRITE(StriI,'(12X)')
      WRITE(StriI,'(I12)') INTGR
      StriI=ADJUSTR(StriI)

      RETURN
      END
