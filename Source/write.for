      SUBROUTINE WRITE(LF1,ITEXT,NCHAR,LF2)
C     -------------------------------------
C     Send to LUO:
C       - LF1 line-feeds,
C       - NCHAR chars. of ITEXT,
C       - then LF2 line-feeds
C
C     Note that the output channel is NOT selectable
C     by the calling program
C
C     Note:
C     Calling program must give a value to LUO.
        
C     jw/20.4.84
C     jw/19.10.00 last rev.
C
      USE LUnits
      CHARACTER ITEXT(NCHAR)
C       
      CALL SEND (LUO,LF1,ITEXT,NCHAR,LF2)
C
      RETURN
      END
