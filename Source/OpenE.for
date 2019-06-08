      SUBROUTINE OPENE(FiNAME)
C     ------------------------
C     jw / 20-04-84
C     jw / 20/04/11 / DISLIN Windows file requester added
C     jw / 20/04/11 / Last rev.


      USE DISLIN

      CHARACTER AINPUT*128, FiNAME*128, YES*1, Y1*1, Y2*2
      LOGICAL   SAYLEN, NOTLEN, EXISTS, INUSE

      INCLUDE  'LUcom.inc' 

      
      DATA      SAYLEN /.TRUE./,
     +          NOTLEN /.FALSE./
     +          EXISTS /.FALSE./
     +          INUSE  /.FALSE./
     
      DATA      Y1/'Y'/, Y2/'y'/

C     Check to see if a filename has already been provided
C     by attempting to open a non-existent INPUT file
      IF(Len_Trim(FiNAME).NE.0) GO TO 20

C     No NAME available yet, so ask for one
C     FiNAME = AINPUT(2,' ECHO filename',SAYLEN,80,12)
      CALL dwgfil('ECHO file name', FiNAME, '*.ins')
      FiNAME=TRIM(FiNAME)

      IF ( LEN(FiNAME).EQ.0 ) GOTO 62
C     Check whether the requested echo file already exists
      INQUIRE(FILE=FiNAME,EXIST=EXISTS,OPENED=INUSE, IOSTAT=IOCODE)
C     If it is there, and already open, don't touch it
      IF(INUSE)GO TO 60
C     Is it is simply there, but not being used, offer a choice
      IF(EXISTS) THEN
         YES = AINPUT(0,' OK to overwrite? (Y/N)',NOTLEN,0,1)
         IF( (YES .EQ. Y1) .OR. (YES .EQ. Y2) ) GO TO 20
         GO TO 62
      END IF
        
C     User has requested or needs an echo file, where an
C     echo copy will be written by the user's program of all input
20    OPEN(UNIT=LUESET,
     +     FILE=FiNAME,
     +     STATUS='UNKNOWN',
     +     ERR=60,
     +     IOSTAT=IOCODE )

      FiNAME=TRIM(FiNAME)

      WRITE(LUS,48)
48    FORMAT (' A formatted copy of all keyboard input will'
     +        ' be saved as a NEW input file.')
      WRITE(LUS,'('' Input file opened:''/'' '',A)') FiNAME

      LUE = LUESET
      RETURN

C     User has failed to open an echo file or doesn't want one
60    WRITE(LUS,61) IOCODE
61    FORMAT(' Error code [',I4,']. Named echo file not accessible.',$)
62    CALL DISP(1,' Warning: echo file not opened.',31,0)

      LUE = -1

      RETURN
      END
