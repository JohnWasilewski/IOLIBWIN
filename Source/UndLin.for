      SUBROUTINE UNDLIN(LU,UCHAR)
C     ---------------------------
C     jw / 14-04-86
C     jw / 09-02-09  last rev

      USE Config
      USE LUnits

      CHARACTER UCHAR

      IF(LUOset) THEN
         LENLIN = PRLINW
      ELSE
         LENLIN = SCLINW-1
      END IF

      DO I=1,LENLIN
         WRITE(LU,'(A,$)') UCHAR
      END DO

C     WRITE(LU,'(A)') LF$
      CALL LF(LU,1)
    
      RETURN
      END
