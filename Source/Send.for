      SUBROUTINE SEND(LU,LF1,ITEXT,NCHAR,LF2)
C     -------------------------------------------
C     Send to LU:
C       - LF1 line-feeds,
C       - NCHAR chars. of ITEXT,
C       - then LF2 line-feeds
C
C     Note that the output channel IS selectable
C     by the calling program
C
C     jw/20.04.84
C     jw/23.11.03 last rev.
C
      CHARACTER ITEXT(NCHAR)

      IF(LF1.GT.0) CALL LF(LU,LF1)
      DO 100 I=1,NCHAR
100      WRITE (LU,'(A,$)') ITEXT(I)

      IF(LF2.GT.0) CALL LF(LU,LF2)

      RETURN
      END
