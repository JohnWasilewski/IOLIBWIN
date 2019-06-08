      SUBROUTINE ERROR(IERR,ERRMSG,*)
C     -------------------------------
C     Display an error message and print it on the output unit then,
C     depeGOODBYEnding on how bad the error was and where the input is coming
C     from, either return (to resume or re-enter, depending on the
C     error and/or on user's response), or exit to the system

C     jw/14-04-86
C     jw/97-11-04 last rev.

C      IERR   CONDITION           ACTION
C     ------- -----------------   -------------------------------------
C      0 - 9  Reminder/info only  ERRMSG 'Note' to LUC only
C                                 RETURN whence called
C
C     10 - 19 Polite Warning      ERRMSG 'Warning' to LUO/LUC
C                                      Resume -> RETURN whence called
C                                      Quit   -> Abort
C
C     20 - 29 Warn(w/rtry option) If console input
C                                    ERRMSG 'Error' to LUC
C                                    Prompt LUC for action...
C                                      Ignore -> RETURN whence called
C                                      Retry  -> RETURN 1 for re-entry
C                                      Quit   -> Abort
C                                 If file input
C                                    ERRMSG 'Error' to LUO/LUC
C                                    Abort
C
C     30 - 39 Bad(w/retry option) If console input
C                                    ERRMSG 'Error' to LUC
C                                    Prompt LUC for action...
C                                      Retry  -> RETURN 1 for re-entry
C                                      Quit   -> Abort
C                                 If file input
C                                    ERRMSG 'Error' to LUO/LUC
C                                    Abort
C
C     40 - 49 Worse (MUST retry)  If console input
C                    ----            ERRMSG 'Error' to LUC
C                                    Retry    -> RETURN 1 for re-entry
C                                 If file input
C                                    ERRMSG 'Error' to LUO/LUC
C                                    Abort
C
C     50 - Worst(Immed.FatalExit) ERRMSG 'Fatal error' to LUO/LUC
C                                 Abort

      INCLUDE       'CfgTyp.inc'
      CHARACTER*(*) ERRMSG
      CHARACTER*1   IRQ, AINPUT
      INTEGER       LUETMP, IERR, LENMSG

      INCLUDE      'CfgCom.inc'
      INCLUDE      'LUcom.inc'

      LENMSG = LEN(ERRMSG)

C     Temporarily disable any echo file setting to prevent prompted
C     actions keyed in in this routine from being recorded.
C     (Restore the LUE setting before all RETURNs)
      LUETMP = LUE
      LUE    = -1

C     0...9
C     Polite warning only
C     - - - - - - - - - -
      IF(IERR.GE.10) GOTO 10
      WRITE (LUS,501) IERR, ' '//ERRMSG
501   FORMAT (' Note: [',I3,']: '/A,$)
      LUE=LUETMP
      RETURN


C     10...19
C     Warning with confirmation request
C     - - - - - - - - - - - - - - - - -
10    IF(IERR.GE.20) GOTO 20
      WRITE (LUS,502) IERR, ' '//ERRMSG
502   FORMAT (' Check: [',I3,']: '/A,$)
12    IRQ = AINPUT(1,' (R)esume, (Q)uit ? >',.FALSE.,0,1)
      IF (IRQ.EQ.'R' .OR. IRQ.EQ.'r') THEN
         LUE=LUETMP
         RETURN
      ELSE IF (IRQ.EQ.'Q' .OR. IRQ.EQ.'q') THEN
         IF(LUO.EQ.LUOSET) WRITE (LUO,502) IERR, ' '//ERRMSG
         CALL CloseALL
      ELSE
         GOTO 12
      END IF


C     20...29
C     Warning (OK to continue but with retry option)
C     - - - - - - - - - - - - - - - - - - - - - - - -
20    IF(IERR.GE.30) GOTO 30
      WRITE (LUS,502) IERR, ' '//ERRMSG

      IF (LUI.EQ.LUISET) THEN
C        IF (LUO.EQ.LUOSET)
C    +      CALL DISP(0,' Unclear how to continue. Input stopped.',40,1)
C        IRQ = AINPUT(3,' [Press RETURN to quit]',.FALSE.,0,1)
C        CALL CloseALL
         CALL DISP(1,' Continuing..',13,1)
         RETURN
      ELSE
26       IRQ = AINPUT(1,' (A)ccept, (R)e-enter, (Q)uit ? >',.FALSE.,0,1)
         IF (IRQ.EQ.'A' .OR. IRQ.EQ.'a') THEN
            IF(LUO.EQ.LUOSET) WRITE (LUO,502) IERR, ' '//ERRMSG
            LUE=LUETMP
            RETURN
         ELSE IF (IRQ.EQ.'R' .OR. IRQ.EQ.'r') THEN
            LUE=LUETMP
            RETURN 1
         ELSE IF (IRQ.EQ.'Q' .OR. IRQ.EQ.'q') THEN
            IF (LUO.EQ.LUOSET) CALL WRITE(1,' Abandoned.',11,1)
            CALL CloseALL
         ELSE
            GOTO 26
         END IF
      END IF

C     30...39
C     Bad (can't continue but allowed to retry)
C     - - - - - - - - - - - - - - - - - - - - -
30    IF(IERR.GE.40) GOTO 40
      WRITE (LUS,503) IERR, ' '//ERRMSG
503   FORMAT (' Error [',I3,']: '/A,$)

      IF (LUI.EQ.LUISET) THEN
         IF (LUO.EQ.LUOSET) WRITE (LUO,503) IERR, ' '//ERRMSG
         CALL CloseALL
      ELSE
36       IRQ = AINPUT(1,' (R)etry, (Q)uit ? >',.FALSE.,0,1)
         IF (IRQ.EQ.'R' .OR. IRQ.EQ.'r') THEN
            LUE=LUETMP
            RETURN 1
         ELSE IF (IRQ.EQ.'Q' .OR. IRQ.EQ.'q') THEN
            IF (LUO.EQ.LUOSET) WRITE (LUO,503) IERR, ' '//ERRMSG
            CALL CloseALL
         ELSE
            GOTO 36
         END IF
      END IF


C     40...49
C     Worse (insist on retry if possible - no other option)
C     - - - - - - - - - - - - - - - - - - - - - - - - - - -
40    IF(IERR.GE.50) GOTO 50
      WRITE (LUS,503) IERR, ' '//ERRMSG
      IF (LUI.EQ.LUISET) THEN
         IF (LUO.EQ.LUOSET) WRITE (LUO,503) IERR, ' '//ERRMSG
         CALL CloseALL
      ELSE
         CALL LF(LUS,1)
         LUE=LUETMP
         RETURN 1
      END IF


C     50...
C     Worst (work out the answer to life, the universe and everything):
C     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
50    CONTINUE
      WRITE (LUS,504) IERR, ' '//ERRMSG
      IF (LUO.EQ.LUOSET) WRITE (LUO,504) IERR,' '//ERRMSG
504   FORMAT (' Fatal error [',I3,']: '/A)
      IRQ = AINPUT(3,'[Press RETURN to quit]',.FALSE.,0,1)
      CALL CloseALL


      END
