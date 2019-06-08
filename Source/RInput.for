      FUNCTION RINPUT(LF,IPRMT,MAXDOT)
C     --------------------------------
C     Send a prompt to the screen and read a real number
C     variable from the current input channel, LUI;
C     The input variable value is returned by the function.
C
C     Return a special value, -1.7E+37, to inform calling
C     program if EOF has been encountered on LUI.
C
C     Calling program must INCLUDE  'LUCOM.INC' ;
C     Calling program must give values to LUS, LUK and LUI ;
C     Calling program should test returned value for special
C     -1.7E+37 EOF signal.
C
C     jw/10.02.84   
C     jw/23.11.03 last rev.

      USE CONFIG
      USE Files
      USE LUnits
 
      CHARACTER IPRMT*(*)

      LPRMT  = LEN(IPRMT)
      MXDOT = MIN(MAXDOT,SCLINW-LPRMT-8)
50    CALL PROMPT(LF,IPRMT,LPRMT,MXDOT)
     
      RINPUT = 0
      LU=LUI
      IF(LU.LT.0) LU=LUK
      READ(LU,60,ERR=70,END=80) RINPUT
60    FORMAT(F8.0)       
C     IF(LUE.EQ.LUESET)WRITE(LUE,61) RINPUT
C61   FORMAT(F8.3)
      RETURN

70    CALL ERROR(30,'ERROR.. real number value expected',*50)

C     Special value signalling (EOF) condition
80    RINPUT=-1.7E+37
      RETURN
C 
      END
