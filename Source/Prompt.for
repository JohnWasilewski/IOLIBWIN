      SUBROUTINE PROMPT(LF,ITEXT,NCHAR,MAXDOT)
C     ----------------------------------------
C     Send LF line-feeds/carr-return to the screen,
C     followed by a prompt; the prompt consists of a
C     concatenation of NCHARs of text in ITEXT, and up
C     to a maximum of MAXDOT '.' DOTs
C
C     Prompt length, including the dots, not to exceed:
C       (a)  (SCLINW-SCRINR)  chars.
C       (b)  (NCHAR+MAXDOT) chars.
C
C     Leave the cursor on the same line as the prompt.
C
C     Calling program must INCLUDE 'LUCOM.INC'
C     Calling program must give values to LUS and LUK.
C
C     jw / 20.04.84
C     jw / 21.05.01 last rev.
C

      INCLUDE    'CfgTyp.inc'
      INTEGER     I, LF, SCRINR, LENPRM, MAXDOT, MINDOT
      CHARACTER   ITEXT(NCHAR), IPRMT(80), DOT

      INCLUDE    'CfgCom.inc'

      DATA SCRINR / 0 /

      DATA DOT    / '.' /

      MINDOT = MIN(2,MAXDOT)
      LENTXT = MAX(NCHAR,0)
      LENPRM = SCLINW-SCRINR

C     LENTXT = MIN(LENTXT,LENPRM-MINDOT)
      IF(LENTXT.GT.LENPRM-MINDOT) LENTXT = LENPRM-MINDOT

C     LENPRM = MIN(LENPRM,LENTXT+MAXDOT)
      IF(LENPRM.GT.LENTXT+MAXDOT) LENPRM = LENTXT+MAXDOT

      DO 10 I=1,LENPRM
10       IPRMT(I) = ' '

      DO 20 I=1,LENTXT
20       IPRMT(I) = ITEXT(I)

      DO 30 I=LENTXT+1,LENPRM-1
30       IPRMT(I) = DOT

      CALL DISP(LF,IPRMT,LENPRM,0)

      RETURN
      END
