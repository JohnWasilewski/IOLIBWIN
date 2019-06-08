      CHARACTER*(*) FUNCTION AINPUT(LF,IPRMT,SAYLEN,MAXDOT,NCHAR)
C     -----------------------------------------------------------
C     Send a prompt to the screen and read a
C     string from the current input channel, LUI;
C     NCHAR characters of input string are returned.
C
C     Return a special value, 'EOF', to inform calling
C     program if EOF has been encountered on LUI.
C
C     LF     = No of linefeeds before the prompt
C     IPRMT  = Prompt string
C     LPRMT  = Length of the above-stated prompt string
C     SAYLEN = Whether the prompt should say max no. chars. reqd.
C
C     Calling program must INCLUDE 'LUCOM.INC' ;
C     Calling program must give values to LUS, LUK and LUI;
C
C NB: Calling program must declare a character variable of
C     up to 80-char to receive the returned string.
C
C     Calling program should test returned value for
C     special 'EOF' signal.
C
C     Logical variable .OPENED. shows whether subroutine OPENER
C     has been executed yet.  If it has not, then AINPUT is being
C     called to obtain filenames so input must be forced from
C     the keyboard and echo file output must be suppressed.
C
C     jw/01.03.89   
C     jw/20.04.11 last rev
C

      USE CONFIG
      USE Files
      USE LUnits
      INCLUDE  'asctyp.inc'

      CHARACTER BUFFER*128, IPRMT*(*), EOF*3
      CHARACTER JPRMT*128, KPRMT*128, ILEN*2
      CHARACTER IBUF1
      INTEGER   I
      LOGICAL   SAYLEN, OPENED

      INCLUDE  'asccom.inc'
C
      DATA  EOF /'EOF'/

      LPRMT  = LEN(IPRMT)

      IF(NCHAR.LT.1)  NCHAR=1
C     IF(NCHAR.GT.128) NCHAR=128
      NCHAR1 = NCHAR+1
C
      WRITE(BUFFER,'(128X)')
      WRITE(AINPUT,'(128X)')
      
      WRITE (JPRMT,'(A,$)') IPRMT(1:LPRMT)
      KPRMT=JPRMT(1:LPRMT)
      LPRMT3 = LPRMT

      IF (SAYLEN) THEN
         WRITE (ILEN,26) NCHAR
26       FORMAT(I2)
         KPRMT=KPRMT(1:LPRMT3)//' [max '//ILEN//' char]'
         LPRMT3 = LPRMT3 + 14
         END IF
      MXDOT=MIN(MAXDOT,SCLINW-LPRMT3-NCHAR)
      IF(LUISET) CALL PROMPT(LF,KPRMT(1:LPRMT3),LPRMT3,MXDOT)

C     Use LUI to read the data unless .OPENED. has not yet been
C     set, which might mean we are still reading the LUI filename,
C     before opening the files, in which case, read from the keyboard.

      LU=LUK
      IF(OPENED) LU=LUI
      READ(LU,60,END=80) BUFFER
      IF(LU.NE.LUK) write(lus,'(A)') BUFFER(1:NCHAR)

C     Most editors terminate all records with  <CR LF>  but some
C     assume all records terminate only with  <CR>.  If the
C     file on LUI was prepared by an editor and has <CR LF> then
C     each READ might terminate on <CR> and the <LF> char would
C     be taken incorrectly to be the first char of the next record.
C     To overcome this difficulty, test the first char read into
C     BUFFER(41) and do not pass it back to calling program if it is
C     a <LF> character (Hex 0A).
       
      IPOIN=1
      IBUF1=BUFFER(1:1)
      IF(IBUF1.EQ.LF$) IPOIN=2
      AINPUT(1:NCHAR)=BUFFER(IPOIN:NCHAR)

      IF(OPENED) THEN
        IF(LUE.EQ.LUESET) WRITE(LUE,61) AINPUT(1:NCHAR)
C       IF(LUI.EQ.LUISET) WRITE(LUS,60) AINPUT(1:NCHAR)
        END IF
60    FORMAT(A)
61    FORMAT(/A,$)

      GO TO 90

80    AINPUT(I:3) = EOF

90    RETURN

      END
