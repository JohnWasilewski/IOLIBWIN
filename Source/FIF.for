      FUNCTION FIF(TEST,TRUEVALU,FALSVALU)
C     ------------------------------------
C     jw / 04-06-09  written
C     jw / 08-05-11  last revised

C     Returns TRUEVALU or FALSVALU, depending on TEST

      LOGICAL TEST

      IF(TEST) THEN
         FIF=TRUEVALU
      ELSE
         FIF=FALSVALU
      END IF

      RETURN

      END
