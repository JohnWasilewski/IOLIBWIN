      FUNCTION IIF(TEST,TRUEVALU,FALSVALU)
C     ------------------------------------
C     jw / 08-05-11  written
C     jw / 08-05-11  last revised

C     Returns TRUEVALU or FALSVALU, depending on TEST

      LOGICAL TEST
      INTEGER TRUEVALU, FALSVALU

      IF(TEST) THEN
         IIF=TRUEVALU
      ELSE
         IIF=FALSVALU
      END IF

      RETURN

      END
