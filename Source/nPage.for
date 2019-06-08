      SUBROUTINE NPAGE(NEED, NEWpg)
C     -----------------------------
C     jw / 14-04-86 draft
C     jw / 18-07-12 last rev

C     If fewer lines remain on the current page than NEED, then..
C
C     Form-feed the output except for very first page of output.
C     Print a 3-line banner panel when CALLed to form-feed page 1.
C     Print 1-line continuation headers when CALLed for subsequent pages.
C     Return NEWpg=.TRUE. .
C
C     PRLINW = Print line length
C     PRMLHS = Print margin left
C     PRMRHS = Print margin right
C     PRLIPP = Print page height
C     PRMTOP = Print margin top
C     PRMBTM = Print margin bottom

      USE CONFIG
      USE TITLES
      USE Files
      USE RTF
      USE LUnits

      CHARACTER TODAY*9, NX*16, I2CHAR*16

      LOGICAL   NEWpg
      INTEGER   NEED
C     Note - calling routine must include an INTEGER value amongst the
C     variables if an expression passes the parameter to NEED.

      NEWpg=.FALSE.

C     Immediate return if more lines remain on the page than NEED,
C     unless this is also the first call to this subroutine (signified
C     by PAGE.EQ.0), in which case execute this subroutine anyway and
C     print a 3-line page heading at the start of printout.

      IF(.NOT.F%O%OPEN) RETURN
      IF( (IPAGE.NE.0).AND.(ILINE+NEED.LT.PRLIPP) ) RETURN



C     Form feed..

      NEWpg=.TRUE.

      IF(IPAGE.EQ.0) THEN
          IPAGE=1
          IF(F%O%EXT.EQ.'rtf' .OR. F%O%EXT.EQ.'RTF') THEN
              REWIND LUO
              WRITE(F%O%LU,'(A)') TRIM(RTFiniSTART$)
              WRITE(F%O%LU,'(A)') RTFiniFONTS$
              WRITE(F%O%LU,'(A)') RTFiniColour$
              WRITE(F%O%LU,'(A)') TRIM(RTFiniPAGE$)
              WRITE(F%O%LU,'(A)') RTFiniPARA$
          END IF
      ELSE
          CALL FF(LUO)
      END IF

C     CALL LF(LUO,PRMTOP)


C     Page 1 only..
C     -------------
      IF(IPAGE.EQ.1) THEN

C       For page 1, work out min reqd. column widths..
        LP=LEN_TRIM(PROG8)
        LV=LEN_TRIM(VERS4)
        LD=LEN_TRIM(DESC24)
        LC=LEN_TRIM(CPRT17)
        LE=LEN_TRIM(ENG)
        COLW1=MAX(LP+5+LV, LD, LC)
        COLW3=MAX(15, 16, 7+LE)
        COLW1=MAX(COLW1,COLW3)

        LT1=LEN_TRIM(TITLE1)
        LT2=LEN_TRIM(TITLE2)
        LT3=LEN_TRIM(TITLE3)

        COLW2=MAX(LT1,LT2,LT3)
        COLW3=COLW1

        IF(LUOset) WRITE(LUS,'('' '',A)') TITLE1,TITLE2,TITLE3

C       now work out avail free width to add to cols 1 and 2..
        COLFRE=(PRLINW-COLW1-COLW2-COLW3)

        IF(COLFRE.LT.4) THEN
C         WRITE(NX,'(I)') 4-COLFRE
C         NX=I2CHAR(4-COLFRE,NCHAR)
          NX=I2CHAR(COLW1+COLW2+COLW3+4+PRMLHS+PRMRHS,iDum)

          CALL DISP(1,'PrintCharsPerLine needs '//TRIM(NX)//
     +                ' characters ',39,0)
          CALL DISP(0,'and it is too short.',20,1)
          CALL DISP(0,'Increase it. reduce page margins, or '//
     +                'shorten the longest title line.',68,1)
          CALL CloseALL
        END IF

C       and finally decide actual column widths to use..
        COLW1=COLW1+COLFRE-COLFRE/2
C       COLW2=COLW2
        COLW3=COLW3+COLFRE/2

        MORXX=MAX(0,LEN_TRIM(ENG)-9)
        KOL3 = 7+9+MORXX

      END IF


C     1st line, all pages..
C     ---------------------
C     Top line across page,
        CALL UNDLIN(LUO,'-')

C     Line-1 column-1
        WRITE(LUO,'(A,$)')
     +  PROG8(1:LP)//' ver.'//VERS4(1:LV)//REPEAT(' ',COLW1-LP-5-LV)

C     Line-1 column-2
        WRITE(LUO,'(A,$)')
     +  Bold$//TITLE1(1:LT1)//unBold$//REPEAT(' ',COLW2-LT1)

C     Line-1 column-3
        WRITE(LUO,'(A,I3.3,A)')
     +  REPEAT(' ',COLW3-KOL3)//'Page :'//REPEAT(' ',6+MORXX)//
     +  '.',iPage,LF$
        ILINE=ILINE+1

      IF(IPAGE.GT.1) GOTO 200


C     2nd line, page 1 only..
C     -----------------------
C     Line-2 column-1
        WRITE(LUO,'(A,$)')DESC24(1:LD)//REPEAT(' ',(COLW1-LD))

C     Line-2 column-2
        WRITE(LUO,'(A,$)')
     +  Bold$//TITLE2(1:LT2)//unBold$//REPEAT(' ',COLW2-LT2)

C     Line-2 column-3
        WRITE(LUO,'(A)')
     +  REPEAT(' ',COLW3-KOL3)//'Date :'//REPEAT(' ',1+MORXX)//
     +  UDat//LF$
        ILINE=ILINE+1


C     3rd line, page 1 only..
C     -----------------------
C     Line-3 column-1
        WRITE(LUO,'(A,$)')CPRT17(1:LC)//REPEAT(' ',(COLW1-LC))

C     Line-3 column-2
        WRITE(LUO,'(A,$)')
     +  Bold$//TITLE3(1:LT3)//unBold$//REPEAT(' ',COLW2-LT3)

C     Line-3 column-3
        WRITE(LUO,'(A)')
     +  REPEAT(' ',COLW3-KOL3)//'Engr :'//
     +  REPEAT(' ',MAX(1,10+MORXX-LE))//ENG(1:LE)//LF$
        ILINE=ILINE+1


C     Move lowest-entered title line into TITLE1 for page header
C     line on continuation pages, page 2 onwards..
      IF( TITLE2(1:24).NE.'                        ' ) THEN
          TITLE1(1:24)=TITLE2(1:24)
          LT1=LT2
          END IF
      IF( TITLE3(1:24).NE.'                        ' ) THEN
          TITLE1(1:24)=TITLE3(1:24)
          LT1=LT3
          END IF

C     Lower underline to page header, all pages..
200   CALL UNDLIN(LUO,'-')
      RETURN
      END

