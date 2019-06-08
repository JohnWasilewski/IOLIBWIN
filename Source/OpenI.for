      SUBROUTINE OPENI(FiNAME,ECHCHK)
C     -------------------------------
C     jw / 20.04.84
C     jw / 20/04/11 / DISLIN Windows file requester added
C     jw / 20/04/11 / Last rev.

      USE DISLIN

      INCLUDE  'asctyp.inc'
      CHARACTER FiNAME*128
      LOGICAL   ECHCHK, SAYLEN

      INCLUDE  'asccom.inc'
      INCLUDE  'LUcom.inc'

      DATA      SAYLEN /.TRUE./

      WRITE(FiNAME,'(128X)')

C     FiNAME = AINPUT(0,' INPUT file name',SAYLEN,80,12)
      CALL dwgfil('INPUT file name', FiNAME, '*.*')
      FiNAME=TRIM(FiNAME)

      IF ( LEN(FiNAME).NE.0 ) THEN
C        User has entered an input file name
C        So attempt to open it
         LUI=LUK
         OPEN(UNIT=LUISET,
     +        FILE=FiNAME,
     +        STATUS='OLD',
     +        ERR=50,
     +        IOSTAT=IOCODE )

C        No ERR, so say so and set file-opened return value of LUI
C        CALL DISP(0,FiNAME//' opened.',LEN(FiNAME)+8,0)
  	      WRITE(LUS,'('' Input file opened:''/'' '',A)') FiNAME
         LUI=LUISET
         GO TO 60

50       CONTINUE
C        Input filename entered but file not found and/or not opened
C        Return a request to open the same filename as an echo file
C        Set return value of LUI to LUK because input file-not-opened
         WRITE(LUS,54) IOCODE
54       FORMAT(' Error code [',I4,'].',$)
         LUI=LUK
55       CONTINUE

      ELSE
C        User has not entered an input filename
         LUI=LUK

60    END IF

C     Has an input file just been opened?
      IF(LUI.EQ.LUISET) THEN
C        Yes. Note that echo file option is invalid with file input
C        so disable it so user won't be asked for an echo filename
         ECHCHK = .FALSE.

      ELSE
C        No. Revert to keyboard input. Announce this.
C        If an input filename was given but not opened then send
C        this FiNAME back for use as an echo file
         CALL DISP(0,' Named input file not found.',28,1)
         CALL DISP(0,' Prompted keyboard input required.',34,1)

C        Next statement disabled because would only be sensible if it
C        was guaranteed that all applications had full echo file support
C        IF(LEN_TRIM(FiNAME).NE.0) ECHCHK=.TRUE.

      END IF

      RETURN
      END
