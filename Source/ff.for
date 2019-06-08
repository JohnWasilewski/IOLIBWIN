      SUBROUTINE FF(LU)
C     -----------------
C     Output a formfeed character on LU
C     [New page on a printer, clears the screen]
C     jw / 20.04.84
C     jw / 20.10.00  amended for GNU Fortran77
C     jw / 17.07.12  last rev.

      USE CONFIG
      USE RTF
      USE TITLES
      USE LUnits

      IF (LU.EQ.0) THEN
          WRITE(LUS,'(A/)') ('',i=1,100)
      ELSE
          WRITE(LU,'(A)') FF$
          IPAGE=IPAGE+1
          ILINE=1          
      END IF

      RETURN
      END
