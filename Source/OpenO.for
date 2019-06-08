      SUBROUTINE OPENO (FiNAME)
C     -------------------------
C     jw / 20-04-84
C     jw / 20/04/11 / DISLIN Windows file requester added
C     jw / 08/05/11 / Last rev.


      USE DISLIN
C     USE CONFIG
C     USE LUnits

      CHARACTER FiNAME*128, Y1*1, Y2*1
      LOGICAL   SAYLEN, NOTLEN, EXISTS, INUSE

      INCLUDE  'LUcom.inc'
      INCLUDE  'CfgTyp.inc'
      INCLUDE  'CfgCom.inc'

      DATA      Y1, Y2 /'Y', 'y'/
      DATA      SAYLEN /.TRUE./,
     +          NOTLEN /.FALSE./

C   It is necessary, temporarily during this subroutine, to over-ride
C   any LUI settings so as to force all LUI input to come only from LUK.
C   This is because IF LUI has been set already as input from a file
C   rather than from the screen, THEN the PROGRAM will try to READ the
C   output filename, and the user's decision about whether to engage the
C   printer, from the input file instead of the user's keyboard input.
C   Therefore, store the current LUI value in LUITMP until just before
C   returning to the calling PROGRAM.

      LUITMP = LUI
      LUI = LUKSET

C     WRITE(FiNAME,'(128X)')
      LenFiName=LEN_TRIM(FiNAME)

C     Make the filename extension '.out'
      IF(LenFiName.NE.0) THEN
         IDOT=LenFiName
         DO WHILE (IDOT.GT.1 .AND. FiNAME(IDOT:IDOT).NE.'.')
            IDOT=IDOT-1
         END DO !(IDOT.NE.0)
         IF(IDOT.GT.1) LenFiName=IDOT-1
         FiNAME=FiNAME(1:LenFiName)//'.out'
      END IF !(Len(FiNAME).NE.0)

C     FiNAME = AINPUT(2,' OUTPUT file name',SAYLEN,80,12)
      CALL dwgfil('OUTPUT file name', FiNAME, '*.ou*')
      FiNAME=TRIM(FiNAME)

      IF ( LEN(FiNAME).EQ.0 ) GOTO 42

C     Else user wants output to file
      LUO=-1

C     Check whether the output file already exists
      INQUIRE(FILE=FiNAME,EXIST=EXISTS,OPENED=INUSE, IOSTAT=IOCODE)
C     If it is there, and already open, don't touch it
      IF(INUSE)GO TO 40
C     IF it is there and not being used, offer a choice
      IF(EXISTS) THEN
C        GET = AINPUT(0,' OK to overwrite? (Y/N)',NOTLEN,0,1)
C        IF( (GET .EQ. Y1) .OR. (GET .EQ. Y2) ) GO TO 32
         CALL DWGBUT ('File [ '//Trim(FiNAME)//' ] already exists.|'//
     +                'OK to overwrite?', IVAL)
         IF(IVAL.EQ.1) GO TO 32
         GO TO 42
      END IF

C     If it doesn't exist, or it's OK to overwrite it, try to open it
32    OPEN(UNIT=LUOSET,
     +     FILE=FiNAME,
     +     STATUS='UNKNOWN',
     +     ERR=40,
     +     IOSTAT=IOCODE )

      CALL DISP(0,' Opened.',8,1)
      LUO = LUOSET
      GO TO 60

C     User has failed to open an output file or doesn't want one

40    WRITE(LUS,41) IOCODE
41    FORMAT(/' Error code [',I4,']. File not accessible',$)
42    CALL DISP(0,' No output file opened.',23,0)

      LUO=LUS

      CALL DISP(0,' Output will go to the screen.',30,2)
      LUO = LUOSET
      CALL OPENSW(LUO)
      PRLINW = SCLINW

60    LUI = LUITMP

      RETURN
      END
