      SUBROUTINE INDENT(LU,nTAB)
C     --------------------------
C     Indent LU
C     jw / 13.08.04
C     jw / 16.08.04  last rev.

      INTEGER   nTAB

      DO iTAB=1,nTAB
C        WRITE (LU,'('' '',$)')
C        WRITE (LU,'('' '',$)')
C        WRITE (LU,'(A)', ADVANCE = 'NO') ' '
         WRITE (LU,'(A,$)') ' '
      END DO

      RETURN
      END
