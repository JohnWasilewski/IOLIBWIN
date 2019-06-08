      SUBROUTINE LF(LU,NLF)
C     ---------------------
C     Send NLF line-feeds / carriage-returns to LU
C     jw / 20.04.84
C     jw / 17.07.12  last rev.

      USE LUNITS
      USE FILES
      USE TITLES
      USE RTF

      CHARACTER*6 Lff$

      IF (NLF.LE.0) RETURN

      Lff$ = ''

      IF(LUOset) THEN
          IF(LU.EQ.LUO.AND.(F%O%Ext.EQ.'rtf'.OR.F%O%Ext.EQ.'RTF')) THEN
              Lff$ = LF$
          END IF
      END IF

      DO I=1,NLF
          WRITE (LU,'(A)') Lff$
      END DO

      IF(LU.NE.LUS) ILINE=ILINE+NLF
      RETURN
      END
