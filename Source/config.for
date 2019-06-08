      SUBROUTINE CONFIG
C     ------------------
C     Read all system-dependent variables from the Filename.CFG
C     text-file that MUST reside in the program directory.
C     PRTMIN is the minimum text-length that must be available
C     after deduction of PRMLHS & PRMRHS from PRLINW
C
C     jw  19.04.01
C     jw  09.08.07 last rev.


      INCLUDE  'ASCTYP.INC'
      INCLUDE  'CFGTYP.INC'
      INCLUDE  'TITTYP.INC'
      CHARACTER CFGLIN*80,TODAY*9,T0DAY*9,WNAME*25,ERRMSG*32,CFGERR*34
      CHARACTER R2CHAR*16,APRTMN*3, SEQ*3, AINPUT, IRQ*1
      INTEGER   SCLLDF,SPGWDF,PPGWDF,PRLLDF,PRMLDF,PRMRDF,
     +          SLPPDF,SPGHDF,PPGHDF,PLPPDF,PRMTDF,PRMBDF
      LOGICAL   EXISTS

      INCLUDE  'ASCCOM.INC'
      INCLUDE  'LUCOM.INC'
      INCLUDE  'CFGCOM.INC'
      INCLUDE  'TITCOM.INC'

      DATA
     +  SPGWDF  /160 /,
     +  SPGHDF  /150 /,
     +  SCLLDF  / 80 /,
     +  SLPPDF  / 50 /,
     +  PPGWDF  /210 /,
     +  PPGHDF  /297 /,
     +  PRLLDF  / 80 /,
     +  PLPPDF  / 70 /,
     +  PRMLDF  /  2 /,
     +  PRMRDF  /  0 /,
     +  PRMTDF  /  6 /,
     +  PRMBDF  /  6 /

C     Default values
      ERRSFI    = ' '
      TMPDIR    = ' '
      WRKDIR    = ' '
      SCLINW = SCLLDF
      PPGWID = PPGWDF
      SPGWID = SPGWDF
      PRLINW = PRLLDF
      PRMLHS = PRMLDF
      PRMRHS = PRMRDF
      SCRLPP = SLPPDF
      PPGHGT = PPGHDF
      SPGHGT = SPGHDF
      PRLIPP = PLPPDF
      PRMTOP = PRMTDF
      PRMBTM = PRMBDF

      T0DAY  = TODAY()

C	OPEN(UNIT= LUS, FILE= 'USER', TITLE= 'LUS')
C	OPEN(UNIT= LUC, FILE= 'USER', TITLE= 'LUC')

      CFGLEN = LEN(PROG)
      CFGFIL(1:CFGLEN+4) = PROG(1:CFGLEN)//'.cfg'
      CFGLEN = CFGLEN+4
      CFGERR = ' Bad line in file '//CFGFIL(1:CFGLEN)
      CALL DISP(0,' Checking '//CFGFIL(1:CFGLEN)//' file..',CFGLEN+17,0)

      IF (.NOT.ASCset) THEN
         NUL$   = CHAR(0)
         BEL$   = CHAR(7)
         BS$    = CHAR(8)
         TAB$   = CHAR(9)
         LF$    = CHAR(10)
         CR$    = CHAR(13)
         EOF$   = CHAR(26)
         SPC$   = CHAR(20)
         ESC$   = CHAR(27)
         Box1TL = CHAR(218)
         Box1TR = CHAR(191)
         Box1BL = CHAR(192)
         Box1BR = CHAR(217)
         Box1H  = CHAR(196)
         Box1V  = CHAR(179)
         Tee1T  = CHAR(194)
         Tee1B  = CHAR(193)
         Tee1L  = CHAR(195)
         Tee1R  = CHAR(180)
      END IF

      INQUIRE(FILE=CFGFIL(1:CFGLEN),
     +     EXIST =EXISTS,
     +     SEQUENTIAL=SEQ)
      IF(.NOT.EXISTS .OR. SEQ.EQ.'NO') GO TO 700

      LU1=LU1SET
      OPEN(UNIT=LU1,
     +     FILE=CFGFIL(1:CFGLEN),
     +     STATUS='OLD',
     +     ACCESS='SEQUENTIAL',
     +     ERR=700,
     +     IOSTAT=IOCODE)

C     Check config file for program version/config version match
C     LPRG=LEN(PROG)
C     WRITE(WNAME,'(A)') PROG(1:LPRG)//' ver.'//VERS
C     LNAM=LEN(WNAME)
C     DO 10 I=1,0
C        READ(UNIT=LU1, FMT=101, ERR=700, END=12) CFGLIN
C10       IF (CFGLIN(1:LNAM).EQ.WNAME(1:LNAM)) GO TO 100

C     Check config file for program version/config version match
10    READ(LU1,101, ERR=700, END=12) CFGLIN
         IF (CFGLIN(1:LEN(CFGLIN))
     +       .EQ.
     +       PROG(1:LEN(PROG))//' ver.'//VERS)
     +   GO TO 100
         GO TO 10

C     Cfg file is an incorrect version
12    CALL DISP(0,'. it needs updating.',20,1)
      CALL ERROR(21,'OK to update it?',*769) 
      CLOSE (UNIT=LU1, STATUS='DELETE', ERR=1000,IOSTAT=IOCODE)
      GO TO 701   


C     Read next record
100   READ(UNIT=LU1, FMT=101, ERR=700, END=999) CFGLIN
101   FORMAT (A80)

C     Does it appear to contain data?

      IF ((CFGLIN(1:1).EQ.';')
     +     .OR.
     +    (CFGLIN(1:1).EQ.'['))
     +                           GOTO 100

C     Work out what config data it provides

      IF (CFGLIN(1:15).NE.'FileErrorsFile=') GOTO 110
         ERRSFI=CFGLIN(16:75)
         GOTO 100

110   IF (CFGLIN(1:18).NE.'TemporaryFilesDir=') GOTO 120
         TMPDIR=CFGLIN(19:78)
         GOTO 100

120   IF (CFGLIN(1:8).NE.'WorkDir=') GOTO 130
         WRKDIR=CFGLIN(9:68)
         GOTO 100

130   IF (CFGLIN(1:8).NE.'WorkDir=') GOTO 140
         WRKDIR=CFGLIN(9:68)
         GOTO 100

140   IF (CFGLIN(1:19).NE.'ScreenLinesPerPage=') GOTO 150
        READ(CFGLIN(20:), '(BN,I2)', IOSTAT=KODE) SCRLPP
        IF(KODE.NE.0 .OR. SCRLPP.LT.10 .OR. SCRLPP.GT.80) THEN
          WRITE(LUS,'(A)') CFGERR(1:18+CFGLEN)
          WRITE(LUS,'(A)') CFGLIN(1:18)
          SCRLPP=SLPPDF
          WRITE(ERRMSG,144) SCRLPP
144       FORMAT(' using ScreenLinesPerPage=',I2)
          CALL ERROR(10,ERRMSG(1:23),*100)
          END IF
        GO TO 100

150   IF (CFGLIN(1:19).NE.'ScreenCharsPerLine=') GOTO 160
        READ(CFGLIN(20:), '(BN,I2)', IOSTAT=KODE) SCLINW
        IF(KODE.NE.0 .OR. SCLINW.LT.32 .OR. SCLINW.GT.98) THEN
          WRITE(LUS,'(A)') CFGERR(1:18+CFGLEN)
          WRITE(LUS,'(A)') CFGLIN(1:18)
          SCLINW=SCLLDF
          WRITE(ERRMSG,154) SCLINW
154       FORMAT(' using ScreenCharsPerLine=',I2)
          CALL ERROR(10,ERRMSG(1:23),*100)
          END IF
        GO TO 100

160   IF (CFGLIN(1:16).NE.'PrintLeftMargin=') GOTO 170
        READ(CFGLIN(17:), '(BN,I2)', IOSTAT=KODE) PRMLHS
        IF(KODE.NE.0 .OR. PRMLHS.LT.0 .OR. PRMLHS.GT.40) THEN
          WRITE(LUS,'(A)') CFGERR(1:18+CFGLEN)
          WRITE(LUS,'(A)') CFGLIN(1:15)
          PRMLHS=PRMLDF
          WRITE(ERRMSG,164) PRMLHS
164       FORMAT(' using PrintLeftMargin=',I1)
          CALL ERROR(10,ERRMSG(1:25),*100)
          END IF
        GO TO 100

170   IF (CFGLIN(1:18).NE.'PrintCharsPerLine=') GOTO 180
        READ(CFGLIN(19:), '(BN,I3)', IOSTAT=KODE) PRLINW
        IF(KODE.NE.0 .OR. PRLINW.LT.32 .OR. PRLINW.GT.132) THEN
          WRITE(LUS,'(A)') CFGERR(1:18+CFGLEN)
          WRITE(LUS,'(A)') CFGLIN(1:17)
          PRLINW=PRLLDF
          WRITE(ERRMSG,174) PRLINW
174       FORMAT(' using PrintCharsPerLine=',I2)
          CALL ERROR(10,ERRMSG(1:26),*100)
          END IF
        GO TO 100

180   IF (CFGLIN(1:17).NE.'PrintRightMargin=') GOTO 190
        READ(CFGLIN(18:), '(BN,I2)', IOSTAT=KODE) PRMRHS
        IF(KODE.NE.0 .OR. PRMRHS.LT.0 .OR. PRMRHS.GT.32) THEN
          WRITE(LUS,'(A)') CFGERR(1:18+CFGLEN)
          WRITE(LUS,'(A)') CFGLIN(1:16)
          PRMRHS=PRMRDF
          WRITE(ERRMSG,184) PRMRHS
184       FORMAT(' using PrintRightMargin=',I2)
          CALL ERROR(10,ERRMSG(1:27),*100)
          END IF
        GO TO 100

190   IF (CFGLIN(1:15).NE.'PrintTopMargin=') GOTO 200
        READ(CFGLIN(16:), '(BN,I2)', IOSTAT=KODE) PRMTOP
        IF(KODE.NE.0 .OR. PRMTOP.LT.0 .OR. PRMTOP.GT.40) THEN
          WRITE(LUS,'(A)') CFGERR(1:18+CFGLEN)
          WRITE(LUS,'(A)') CFGLIN(1:14)
          PRMTOP=PRMTDF
          WRITE(ERRMSG,194) PRMTOP
194       FORMAT(' using PrintTopMargin=',I1)
          CALL ERROR(10,ERRMSG(1:24),*100)
          END IF
        GO TO 100

200   IF (CFGLIN(1:18).NE.'PrintLinesPerPage=') GOTO 210
        READ(CFGLIN(19:), '(BN,I3)', IOSTAT=KODE) PRLIPP
        IF(KODE.NE.0 .OR. PRLIPP.LT.32 .OR. PRLIPP.GT.160) THEN
          WRITE(LUS,'(A)') CFGERR(1:18+CFGLEN)
          WRITE(LUS,'(A)') CFGLIN(1:17)
          PRLIPP=PLPPDF
          WRITE(ERRMSG,204) PRLIPP
204       FORMAT(' using PrintLinesPerPage=',I3)
          CALL ERROR(10,ERRMSG(1:28),*100)
          END IF
        GO TO 100

210   IF (CFGLIN(1:15).NE.'PrintBtmMargin=') GOTO 220
        READ(CFGLIN(16:), '(BN,I2)', IOSTAT=KODE) PRMBTM
        IF(KODE.NE.0 .OR. PRMBTM.LT.0 .OR. PRMBTM.GT.32) THEN
          WRITE(LUS,'(A)') CFGERR(1:18+CFGLEN)
          WRITE(LUS,'(A)') CFGLIN(1:14)
          PRMBTM=PRMBDF
          WRITE(ERRMSG,214) PRMBTM
214       FORMAT(' using PrintBtmMargin=',I2)
          CALL ERROR(10,ERRMSG(1:25),*100)
          END IF
        GO TO 100

220   IF (CFGLIN(1:15).NE.'PrintPageWidth=') GOTO 230
        READ(CFGLIN(16:), '(BN,I3)', IOSTAT=KODE) PPGWID
        IF(KODE.NE.0 .OR. PPGWID.LT.150 .OR. PPGWID.GT.512) THEN
          WRITE(LUS,'(A)') CFGERR(1:18+CFGLEN)
          WRITE(LUS,'(A,I3)') CFGLIN(1:14),PPGWID
          PPGWID=PPGWDF
          WRITE(ERRMSG,224) PPGWID
224       FORMAT(' using PrintPageWidth=',I3)
          CALL ERROR(10,ERRMSG(1:26),*100)
          END IF
        GO TO 100

230   IF (CFGLIN(1:16).NE.'PrintPageHeight=') GOTO 240
        READ(CFGLIN(17:), '(BN,I3)', IOSTAT=KODE) PPGHGT
        IF(KODE.NE.0 .OR. PPGHGT.LT.150 .OR. PPGHGT.GT.1024) THEN
          WRITE(LUS,'(A)') CFGERR(1:18+CFGLEN)
          WRITE(LUS,'(A)') CFGLIN(1:15)
          PPGHGT=PPGHDF
          WRITE(ERRMSG,234) PPGHGT
234       FORMAT(' using PrintPageHeight=',I3)
          CALL ERROR(10,ERRMSG(1:26),*100)
          END IF
        GO TO 100

240   IF (CFGLIN(1:12).NE.'ScreenWidth=') GOTO 250
        READ(CFGLIN(13:), '(BN,I3)', IOSTAT=KODE) SPGWID
        IF(KODE.NE.0 .OR. SPGWID.LT.75 .OR. SPGWID.GT.2048) THEN
          WRITE(LUS,'(A)') CFGERR(1:18+CFGLEN)
          WRITE(LUS,'(A)') CFGLIN(1:11)
          SPGWID = SPGWDF
          WRITE(ERRMSG,244) SPGWID
244       FORMAT(' using ScreenWidth=',I3)
          CALL ERROR(10,ERRMSG(1:26),*100)
          END IF
        GO TO 100

250   IF (CFGLIN(1:13).NE.'ScreenHeight=') GOTO 400
        READ(CFGLIN(14:), '(BN,I3)', IOSTAT=KODE) SPGHGT
        IF(KODE.NE.0 .OR. SPGHGT.LT.75 .OR. SPGHGT.GT.2048) THEN
          WRITE(LUS,'(A)') CFGERR(1:18+CFGLEN)
          WRITE(LUS,'(A)') CFGLIN(1:12)
          SPGHGT = SPGHDF
          WRITE(ERRMSG,254) SPGHGT
254       FORMAT(' using ScreenHeight=',I3)
          CALL ERROR(10,ERRMSG(1:26),*100)
          END IF
        GO TO 100

400     IF ( CFGLIN(1:15).EQ.'ScreenInWindow=' ) THEN
           IF
     +     ( (CFGLIN(16:18).EQ.'Yes') .OR.
     +       (CFGLIN(16:18).EQ.'YES') .OR.
     +       (CFGLIN(16:18).EQ.'yes') )
     +       THEN
C Following lines for Amiga only
C              LUC output to Console window instead of to normal LUC
C              WNAME(1:8)='Program '
C              WNAME(9:16)=PROG(1:8)
C              WNAME(17:22)=' ver.'
C              WNAME(22:25)=VERS(1:4)
C              OPEN(UNIT=LUC,
C    +            FILE='USER',
C    +            BLOCKSIZE=0,
C    +            TITLE=WNAME(1:25),
C    +            IOFOCUS=.TRUE.)
               GO TO 100

           ELSE IF
     +     ( (CFGLIN(16:17).EQ.'No') .OR.
     +       (CFGLIN(16:17).EQ.'NO') .OR.
     +       (CFGLIN(16:17).EQ.'no') )
     +       THEN
               GO TO 100

           ELSE
               GO TO 600

           END IF
        END IF

C     If GO TO 100 not reached by now then there must be some duff data

600   CALL DISP(1,' '//CFGLIN(1:60)  ,61,1)
      CALL ERROR(10,'Line not recognised in config file:',*100)

      GOTO 100

C     Error when trying to open the config data file 

700   CALL DISP(1,' Can''t find/open/read file '//
     +             CFGFIL(1:CFGLEN)//'.',CFGLEN+28,1)
      CALL ERROR 
     +  (21,'Create one for you?',*769)
C     Create a config file
701   LU1=LU1SET

      CALL DISP(1,' Creating a new config file..',29,0)
      OPEN(UNIT=LU1,
     +     FILE=CFGFIL(1:CFGLEN),
     +     STATUS='NEW',
     +     ACCESS='SEQUENTIAL',
     +     ERR=770,
     +     IOSTAT=IOCODE)
      WRITE(LUS,'(''.opened.''/'' Writing config data..'',$)')
     
      WRITE(LU1,'(A/ 
     + '';Config initialisation file.''/ 
     + '';J M Wasilewski, '',A,''.''/ 
     + '';''/ 
     + '';This editable text-file MUST stay in the program directory.''/
     + '';Variable-names MUST NOT be altered in any way.''/ 
     + '';This file is case-sensitive.''/ 
     + '';Alter only the relevant values.''/ 
     + '';Do not insert spaces around equals signs.''/ 
     + '';''/ 
     + '';SEE BELOW FOR PRINT SET-UP''/ 
     + '';''/ 
     + '';Comments are allowed anywhere, with [;] at start '',
     + ''of the line.''/ 
     + '';All [;] lines may be safely deleted if desired, but do not''/ 
     + '';leave any blank lines.''/ 
     + '';''/ 
     + '';''/ 
     + '';LINES WITHOUT A LEADING [;] ARE PROGRAM INSTRUCTIONS''/ 
     + '';----------------------------------------------------''/ 
     + '';''/ 
     + '';''/ 
     + ''[File_locations]''/ 
     + '';(This section not used)''/ 
     + '';FileErrorsFile=''/ 
     + '';TemporaryFilesDir=''/ 
     + '';WorkDir=''/ 
     + '';''/ 
     + '';''/ 
     + '';''/ 
     + ''[Screen]''/ 
     + ''ScreenCharsPerLine=80''/
     + ''ScreenLinesPerPage=61''/ 
     + ''ScreenWidth=178''/ 
     + ''ScreenHeight=203''/ 
     + '';''/ 
     + '';''/ 
     + '';''/ 
     + ''[Window]''/ 
     + ''ScreenInWindow=No''/ 
     + '';''/ 
     + '';''/ 
     + '';''
     + )') PROG(1:CFGLEN-4)//' ver.'//VERS, T0DAY

      WRITE(LU1,'(
     + ''[File/printer output]''/ 
     + '';''/ 
     + '';Printing guide''/ 
     + '';--------------''/ 
     + '';Program output is a plain text file with page headings but''/ 
     + '';no page breaks.  Windows software (eg Notepad, M$Word)''/ 
     + '';must be used to view and/or print the output file.''/ 
     + '';To synchronise page headings with tops of actual pages''/ 
     + '';when printed, and line lengths with printer page widths,''/ 
     + '';the program needs to know how many lines there are on a''/ 
     + '';printer-output page and how wide the printer page is.''/ 
     + '';These depend on three particular settings in the software''/ 
     + '';used to view/print the output file (Notepad, Word, etc).''/ 
     + '';''/ 
     + '';PROCEDURE''/ 
     + '';''/ 
     + '';(1) In M$Word, Notepad, etc, set:''/ 
     + '';    (a) --> Page        = A4,''/
     + '';    (b) --> Typeface    = Courier-New 10-point (remember to'',
     +                           '' select this font),''/ 
     + '';    (c) --> Orientation = Portrait.''/ 
     + '';''/ 
     + '';(2) Now supply T,B,L,R margins EITHER in your text '',
     +      ''handler software,''/ 
     + '';    OR in the program output format set by this '',
     +      ''config file,''/
     + '';    but not in both.''/ 
     + '';    You need Program settings in this config file of:''/ 
     + '';    (d) --> PrintLeftMargin + PrintCharsPerLine + '',
     +              ''PrintRightMargin = 99 char''/ 
     + '';    (e) --> PrintTopMargin + PrintLinesPerPage + '',
     +              ''PrintBtmMargin = 74 lines''/ 
     + '';'' )')
      
      WRITE(LU1,'(
     + '';    For margins set by Word/Notepad,''/ 
     + '';     - Leave this config file as it is (default program '',
     +         ''margins all nil);''/ 
     + '';     - When viewing/printing program output files with '',
     +         ''M$Word, Notepad etc,''/ 
     + '';       use the file menu and Page Setup to set all '',
     +         ''margins to 2cm left ''/ 
     + '';       + right and 2½cm top + bottom.''/ 
     + '';''/ 
     + '';    For margins set by the program output format,''/ 
     + '';     - Edit this config file by removing the leading [;] '',
     +         ''from each of''/ 
     + '';       the second set of Printsetting lines below and '',
     +         ''either inserting a''/ 
     + '';       [;] at the start of each line of the first set, '',
     +         ''(or deleting them);''/ 
     + '';     - When viewing/printing program output files with '',
     +         ''M$Word, Notepad etc,''/ 
     + '';       use the file menu and Page Setup to set all '',
     +         ''margins to zero all round.''/ 
     + '';''/
     + '';A4 LESS 2cm L+R MARGINS & 2½cm T+B MARGINS (default)''/
     + ''PrintPageWidth=170''/
     + ''PrintPageHeight=245''/ 
     + ''PrintCharsPerLine=80''/ 
     + ''PrintLeftMargin=0''/ 
     + ''PrintRightMargin=0''/ 
     + ''PrintLinesPerPage=61''/ 
     + ''PrintTopMargin=0''/ 
     + ''PrintBtmMargin=0''/ 
     + '';''/
     + '';A4 WITH NO MARGINS''/
     + '';PrintPageWidth=210''/
     + '';PrintPageHeight=297''/
     + '';PrintCharsPerLine=89''/ 
     + '';PrintLeftMargin=6''/ 
     + '';PrintRightMargin=4''/ 
     + '';PrintLinesPerPage=68''/ 
     + '';PrintTopMargin=3''/ 
     + '';PrintBtmMargin=3''/ 
     + '';''/ 
     + '';NB''/
     + '';Margins ADD TO PrintCharsPerLine & PrintLinesPerPage but''/
     + '';are INCLUDED IN ScreenCharsPerLine & ScreenLinesPerPage.''/
     + '';''/ 
     + '';''
     + )')
     
      WRITE(LU1,'(
     + '';Useful info for customising your own print settings:''/ 
     + '';''/ 
     + '';In M$Word, Notepad, etc, with Courier-New 10-point font,''/ 
     + '';  -  A 2cm left and right margin eats 19 char,''/ 
     + '';  -  A 2½cm top and bottom margin eats 13 lines.''/ 
     + '';''/ 
     + '';A4 WITH NO MARGINS''/ 
     + '';Font          Size  Paper-width   Paper-height''/ 
     + '';Courier-New   10pt    99 char       74 char''/ 
     + '';Courier-New   11pt    90 char       67 char''/ 
     + '';Courier-New   12pt    82 char       61 char''/ 
     + '';''/ 
     + '';A4 WITH 2cm L+R MARGINS & 2½cm T+B MARGINS''/ 
     + '';Font          Size  Print-width   Print-height''/ 
     + '';Courier-New   10pt    80 char       61 char''/ 
     + '';Courier-New   11pt    73 char       58 char''/ 
     + '';Courier-New   12pt    52 char       61 char''/ 
     + '';''/ 
     + '';''/ 
     + '';''/ 
     + ''[System]''/ 
     + '';''/ 
     + '';''/ 
     + '';''
     + )')

      CLOSE (UNIT=LU1, ERR=1000,IOSTAT=IOCODE)

      WRITE(LUS,'(''.done.''//'' Config file now ready: '',A)')
     +                                       CFGFIL(1:CFGLEN)

      WRITE(LUS,
     +   '( '' Please use a text editor to examine this config '',
     +           ''file. ''//
     +
     +      '' The program can be re-run straight away using '',
     +      ''pre-set default''/
     +
     +      '' settings but the config file contains a few '',
     +      ''settings that you''/ 
     +
     +      '' might want to adjust before the program uses them.  '',
     +      ''You can use ''/
     +
     +      '' (eg) Notepad to edit the config file.  If you '',
     +      ''change anything ''/
     +
     +      '' with M$Word etc., then you MUST use SAVE-AS, '',
     +      ''not SAVE, and ''/
     +
     +      '' change the filetype to <Text file> before saving,''/
     +
     +      '' being careful to '',
     +      ''keep the same filename of `Config.ini`.''//
     +
     +      '' Warning: Micro$oft Notepad sometimes adds `.txt`'',
     +      ''to the filename.''/ 
     +
     +      '' If the program keeps asking for an updated Config '',
     +      ''file, please check that''/
     +
     +      '' the config file has not acquired the incorrect name '',
     +      ''of [Config.ini.txt]. ''//)' )

      WRITE(LUS,'('' After reading this,'',$)')

769   CALL QUIT
770   CALL ERROR 
     +  (99,'Unable to create file '//CFGFIL(1:CFGLEN),*999)

999   PRLINW=PRLINW-PRMLHS-PRMRHS
      IF(PRLINW.GE.PRTMIN) THEN
        CALL DISP(0,'. seems OK',10,1)
      ELSE
        WRITE(LUS,9990) PRTMIN
9990    FORMAT('.'/' PrintCharsPerLine too small ',
     + 'or PrintMargins too big'/
     + ' - please alter them to leave room for',I3,' print characters.')
        CALL ERROR(10,'(Continuing may cause problems)',*9991)
      END IF


9991  CLOSE (UNIT=LU1,
     +       ERR=1000,IOSTAT=IOCODE)

1000  CONTINUE

      RETURN

      END
