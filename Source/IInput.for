      FUNCTION IINPUT(NLF,IPRMT,MAXDOT,NDIGIT)
C     ----------------------------------------
C     Send a prompt to the screen and read an integer
C     variable from the current input channel, LUI;
C     The input variable value is returned by the function.
C
C     NDIGIT used only to limit fieldwidth on prompt screen.
C
C     Return a special value, -32767, to inform calling
C     program if EOF has been encountered on LUI.
C
C     Calling program must INCLUDE  'LUCOM.INC' ;
C     Calling program must give values to LUS, LUK and LUI ;
C     Calling program should test returned value for special
C     -32767 EOF signal.
C
C     jw/15.04.86
C     jw/12.11.08 last rev

      USE CONFIG
      USE Files
      USE LUnits

      CHARACTER IPRMT*(*)
      CHARACTER IINSTR*12, I2CHAR*16
      INTEGER   LPRMT

      LPRMT  = LEN(IPRMT)
      MXDOT = MIN(MAXDOT,SCLINW-LPRMT-NDIGIT)
50    CALL PROMPT(NLF,IPRMT,LPRMT,MXDOT)

      IINPUT = 0
      LU=LUI
      IF(LU.LT.0) LU=LUK
      READ(LU,*,ERR=70,END=80) IINPUT	 


C     IF(LUE.EQ.LUESET) WRITE(LUE,60)	 IINPUT
      IINSTR=I2CHAR(IINPUT,NCHAR)
      IF(LUE.EQ.LUESET) CALL SEND(LUE,0,IINSTR,NCHAR,0)
      IF(LUISET) CALL SEND(LUS,0,IINSTR,NCHAR,0)
      
60    FORMAT(I8)

      GO TO 81
C
70    CONTINUE
      CALL ERROR(30,'Integer value expected',*50)

C     Special value signalling (EOF) condition
80    IINPUT=-32767
81    RETURN
C
      END
