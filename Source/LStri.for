
      FUNCTION LSTRI(INTGR)
C     ---------------------
C     jw / 30-05-09  draft
C     jw / 30-05-09  last revised

C     Returns the integer argument as a 12-char left-justified string 

      INTEGER*4 INTGR
      CHARACTER LSTRI*12

      WRITE(LSTRI,'(12X)')
      WRITE(LSTRI,'(I12)') INTGR
      LSTRI=ADJUSTL(LSTRI)

      RETURN
      END
