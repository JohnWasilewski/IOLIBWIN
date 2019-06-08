      SUBROUTINE BANNER(PROG, VERS, DESC, CPRT)
C     -----------------------------------------
C     jw / 15-04-86
C     jw / 27-11-01 last rev.

C     Display a banner title screen

      USE TITLES
      USE LUnits

C     CALL CLS
C     Commented out the prev line because better to let calling
C     program do a CALL CLS first, or not do one, as required.

C     CALL UNDLIN(LUS,'_')

      WRITE(LUS,'(''Program '',A,$)') TRIM(Prog8)
      WRITE(LUS,'('' vers.'',A)') TRIM(Vers4)
      WRITE(LUS,'(A)') TRIM(Desc24)
      WRITE(LUS,'(''Copyright '',A)') TRIM(Cprt17)
      WRITE(LUS,'(A)') TRIM(Auth16)

      CALL UNDLIN(LUS,'_')

      CALL LF(LUS,2)

      RETURN
      END
