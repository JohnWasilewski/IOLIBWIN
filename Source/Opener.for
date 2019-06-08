      SUBROUTINE OPENER (IN, ECHO, OUT)
C     ---------------------------------
C
C     Prompts for filenames and optionally opens INPUT file,
C     ECHO file, and OUTPUT file (on any disk drive).
C
C     Each file is only opened if it is flagged with a '1' in
C     the subroutine argument list.
C
C     Input assumed via keyboard if filename not given
C     by the user.  Output optionally to screen or to
C     printer if filename not given by the user.
C
C     Echo file option can be invoked only if input is
C     obtained from the keyboard.
C
C     Echo file option requires supporting coding in the calling
C     program which must write ECHO copies on LUE of all input
C     data read from LUI.
C
C     Main program must include an EXTERNAL LUINIT statement
C     Calling program must INCLUDE  'LUcom.inc'
C
C     jw/20.04.84
C     jw/29.05.84/rev : save-file facility added.
C     jw/06.06.84/rev : printer option added.
C     jw/06.10.87/rev : save-file facility deleted.
C     jw/03.03.12/rev : last rev.

      USE DISLIN

      INTEGER   IN, ECHO, OUT
      LOGICAL   INCHK, ECHCHK, OUTCHK, OPENED
      CHARACTER FINAME*128, BLANK*128

      INCLUDE  'TitTyp.inc'

      INCLUDE  'LUcom.inc'
      INCLUDE  'TitCom.inc'

      OPENED = .FALSE.
      FINAME =  BLANK
      INCHK  = .FALSE.
      ECHCHK = .FALSE.
      OUTCHK = .FALSE.

      WRITE(BLANK,'(128X)')
      FINAME = BLANK

      IF (IN  .EQ.1) INCHK  = .TRUE.
      IF (ECHO.EQ.1) ECHCHK = .TRUE.
      IF (OUT .EQ.1) OUTCHK = .TRUE.

C     Warn user to be ready to open files

C     Calling PROGRAM may require an input file..
      IF (INCHK) CALL OPENI(FINAME,ECHCHK)
C     Input file needed

C     If LUI returned equal to LUISET then
C     the input file has successfully opened.
C     If it fails to be opened then OPENI returns
C     the FINAME for OPENE to try to open it instead.
C     (Note that OPENI may have switched ECHCHK either on
C     or off to override the original setting if necessary)

C     Calling program may have echo file facilities
C     and may require an echo file..
      IF (ECHCHK) CALL OPENE(FINAME)
C     If LUE returned equal to LUESET then
C     the echo file has successfully opened.
C     If FINAME supplied by OPENI then user won't
C     be asked for it again.

C     Calling program may require an output file..
      LUO=LUS
      IF (OUTCHK) CALL OPENO(FINAME)
C     Output file required

C     If LUO returned equal to LUOSET then
C     the output file has successfully opened.

      OPENED = .TRUE.

      RETURN
      END
