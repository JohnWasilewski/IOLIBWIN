      SUBROUTINE DISP(LF1,ITEXT,NCHAR,LF2)
C     ------------------------------------
C     Display a message on the screen
C
C     Note:
C     Calling program must INCLUDE 'LUcom.inc'
C     Calling program must give a value to LUS.
        
C     jw/20.4.84
C     jw/10.05.01 last rev.
C       
      CHARACTER ITEXT(NCHAR)

      INCLUDE  'LUcom.inc'
C       
      CALL SEND(LUS,LF1,ITEXT,NCHAR,LF2)
C      
      RETURN
      END
