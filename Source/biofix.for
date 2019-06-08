      CHARACTER*9 FUNCTION TODAY()
C     --------------------------------

C     Gnu GFortran :
C     --------------
      CHARACTER*30 DATE
      CHARACTER*9  DDMMYY
      CALL FDATE(DATE)
      DDMMYY = ' '//DATE(4:6)//DATE(1:3)//DATE(7:8)
      TODAY(1:9)=DDMMYY

      RETURN
      END





      SUBROUTINE CloseALL
C     -------------------

      USE CONFIG
      USE LUnits
      USE Files
      USE RTF

      IF(F%I%OPEN) THEN
          CLOSE (F%I%LU)
          F%I%EXISTS = .FALSE.
          LUISET     = .FALSE.
          F%I%OPEN   = .FALSE.
          F%I%LU     =  0
      END IF

      IF(F%O%OPEN) THEN
C     write(6,'(''Closing file '',A,'' on Logical unit '',I2)')
C    +      TRIM(F%Name)//'.rtf',F%O%LU
C         IF(TRIM(F%O%Ext).EQ.'rtf'.OR.TRIM(F%O%Ext).EQ.'RTF')
C    +    WRITE(LUO,'(A)') LF$//'}'
          FLUSH (F%O%LU)
          CLOSE (F%O%LU)
          F%O%EXISTS = .FALSE.
          LUOSET     = .FALSE.
          F%O%OPEN   = .FALSE.
          F%O%LU     =  0
      END IF

      F%Named    = .FALSE.

      IF(LU1SET) CLOSE (LU1)
      IF(LU2SET) CLOSE (LU2)
      IF(LU3SET) CLOSE (LU3)
      IF(LU4SET) CLOSE (LU4)

      RETURN

      END


