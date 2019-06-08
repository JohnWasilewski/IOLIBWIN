      MODULE RTF
C     ----------
C     Window declarations
C     jw 11-07-12 draft
C     jw 29-08-12 last amended

C     RTF measurements & units
C     ------------------------
C
C     POINT   Printers' units : 72.27 points/inch: 1mm = 2.845 points
C             Postscript uints: 72 points/inch   : 1mm = 2.835 points

              REAL, PARAMETER    :: PTperMM = 2.835
              REAL, PARAMETER    :: MMperPT = 0.353
              REAL, PARAMETER    :: PTperTW = 0.05
              INTEGER, PARAMETER :: TWperPT = 20

C             RTF uses points for font sizes but beware:
C             Font sizes are expressed in half-points,
C             so '\fs20 ' means a 10-point font size.


C     TWIP    20 TWIPs per 'point'.
C             Postscript points of 72 points/inch,  1 Twip = 1/56.693 mm
C             Printers' points of 72.27 points/inch 1 Twip = 1/56.906 mm
C
C             Twips are the 'pixels' of RTF.
C             They are used for most RTF measurements.

              REAL, PARAMETER :: TWperMM = 56.7


C     EMU     12700 EMUs ('English metric units') per point.
C             For Postscript points of (1/72)"   1 EMU = 1/36000 mm
C             For printers' points of (1/72.27)" 1 EMU = 1/36135 mm
C
C             EMUs are line thickness units.

              INTEGER, PARAMETER :: EMperMM = 36000
              INTEGER, PARAMETER :: EMperTW = 635

C     ---------------------
C     RICH TEXT FORMAT DATA
C     ---------------------
                                             !

C     WARNING: Subroutine RTFini evaluates the next two variables
C     by computation.  Which is correct?  That or this?
      REAL, PARAMETER :: TWperCHAR = 120.626 ! Courier New, 10-point
      REAL, PARAMETER :: TWperLINE = 226.000 ! Courier New, 10-point

      REAL, PARAMETER :: MMperCHAR =  2.1273 ! Courier New, 10-point
      REAL, PARAMETER :: MMperLINE =  4.0000 ! Courier New, 10-point

      REAL, PARAMETER :: LnWdperMM =  36000  ! LineWidth units per mm
      REAL, PARAMETER :: LnWdperTW =  635    ! LineWidth units per TW


C     REAL*4  MMperLINE, TWperLINE

C     RTF DOCUMENT INITIALISATION
C     ---------------------------
      CHARACTER*112 RTFiniSTART$
C     Use CALL RTFini to build RTFiniSTART$.
C     It should look something like this when built:
C     CHARACTER*112, PARAMETER:: RTFiniSTART$ =
C    +          '{\rtf1 {\*\generator'//
C    +          ' Program '//TRIM(PROG8)//
C    +          ' vers.'TRIM(VERS4)//': '//PDAT9//
C    +          ' Copyright '//TRIM(CPRT17)//' '//TRIM(AUTH16)//'}'//
C    +          '\ansi \deff0 '


      CHARACTER*68, PARAMETER:: RTFiniFONTS$ =
     + '{\fonttbl'//
     +   '{\f0 Courier New;}'//
     +   '{\f1 Lucida Console;}'//
     +   '{\f2 Arial Narrow;}}'



      CHARACTER*297, PARAMETER:: RTFiniColour$ =
     + '{\colortbl;'//                     !11
     +   '{\red0\green0\blue0;}'//         !21  01 = black
     +   '{\red255\green0\blue0;}'//       !23  02 = bright red
     +   '{\red0\green255\blue0;}'//       !23  03 = bright green
     +   '{\red0\green0\blue255;}'//       !23  04 = bright blue
     +   '{\red255\green255\blue0;}'//     !25  05 = bright green
     +   '{\red255\green0\blue255;}'//     !25  06 =
     +   '{\red0\green255\blue255;}'//     !25  07 =
     +   '{\red255\green255\blue255;}'//   !27  08 = white
     +   '{\red133\green0\blue0;}'//       !23  09 = dark red
     +   '{\red0\green133\blue0;}'//       !23  10 = dark green
     +   '{\red0\green0\blue133;}'//       !23  11 = dark blue
     +   '{\red255\green0\blue255}}'       !25  12 =



      CHARACTER*66  RTFiniPAGE$
C     Use CALL RTFini to build RTFiniPAGE$.
C     It should look something like this when built:
C     CHARACTER*69, PARAMETER:: RTFiniPAGE$ =
C    + '\paperw11907'//
C    + '\paperh16841'//
C    + '\margl1070'//
C    + '\margr825'//
C    + '\margt1100'//
C    + '\margb1100'

      CHARACTER*52, PARAMETER:: RTFiniPARA$ =
     + '\f0 \fs20 \i0 \ul0 \b0 \ql \strike0 \striked0 '//
     + '\pard '

C     RTF CHAR FORMAT & APPEARANCE
C     ----------------------------
      CHARACTER*7,  PARAMETER:: Bold$       = '\f1 \b '   !Bold
      CHARACTER*8,  PARAMETER:: unBold$     = '\b0 \f0 '  !EndBold

      CHARACTER*3,  PARAMETER:: Italic$     = '\i '       !Italic
      CHARACTER*4,  PARAMETER:: unItalic$   = '\i0 '      !EndItalic

      CHARACTER*5,  PARAMETER:: SubScrip$   = '\dn6 '     !SubScript
      CHARACTER*5,  PARAMETER:: unSubScrip$ = '\dn0 '     !EndSubScript

      CHARACTER*7,  PARAMETER:: ArNarrB$    = '\f2 \b ' !Arial Narrow B
      CHARACTER*8,  PARAMETER:: unArNarrB$  = '\b0 \f0 '!end Ar Narrow B

      CHARACTER*4,  PARAMETER:: UndLine$    = '\ul '
      CHARACTER*5,  PARAMETER:: unUndLine$  = '\ul0 '

      CHARACTER*6,  PARAMETER:: Small$      = '\fs15 '
      CHARACTER*6,  PARAMETER:: unSmall$    = '\fs20 '

      CHARACTER*5,  PARAMETER:: WHITE$      = '\cf8 '   !White
      CHARACTER*5,  PARAMETER:: RED$        = '\cf2 '   !Bright RED
      CHARACTER*5,  PARAMETER:: GREEN$      = '\cf3 '   !Bright GREEN
      CHARACTER*5,  PARAMETER:: BLUE$       = '\cf4 '   !Bright BLUE
      CHARACTER*5,  PARAMETER:: DarkRED$    = '\cf9 '   !Dark RED
      CHARACTER*6,  PARAMETER:: DarkGREEN$  = '\cf10 '  !Dark GREEN
      CHARACTER*6,  PARAMETER:: DarkBLUE$   = '\cf11 '  !Dark BLUE
      CHARACTER*5,  PARAMETER:: BLACK$      = '\cf1 '   !BLACK


C     RTF LINE, PARA & PAGE CONTROL
C     -----------------------------
      CHARACTER*6,  PARAMETER:: LF$ = '\line '   !Linefeed
      CHARACTER*5,  PARAMETER:: CR$ = '\par '    !Return, keeping format
      CHARACTER*52, PARAMETER:: CN$ = '\pard '// !New para, format reset
     +                          '\f0 \fs20 \i0 \ul0 \b0 \ql '//
     +                          '\strike0 \striked0 '
      CHARACTER*5,  PARAMETER:: FF$ = '\page'   !Formfeed

C     RTF ALIGNMENT CONTROL
C     ---------------------
      CHARACTER*4, PARAMETER::LL$     = '\ql '   !Line left aligned
      CHARACTER*4, PARAMETER::LR$     = '\qr '   !Line right aligned
      CHARACTER*4, PARAMETER::LJ$     = '\qj '   !Line L+R justified
      CHARACTER*4, PARAMETER::CJ$     = '\qc '   !Centre the text
      CHARACTER*4, PARAMETER::TabS$   = '\tx '   !Tab set [TWIPS needed]
      CHARACTER*11,PARAMETER::TabDecS$='\tqdec \tx '!Dec.tab set [TWIPS]
      CHARACTER*9, PARAMETER::TabRS$ = '\tqr \tx '!Right tab set [TWIPS]


C     RTF DRAWINGS
C     ------------
C     Initialise the drawing
      CHARACTER*32, PARAMETER:: DrgInit =
     +             '{\shpbxignore\shpbyignore\shpwr1'

C     Make a line grey
      CHARACTER*33, PARAMETER:: LineGrey =
     +             '{\sp{\sn lineColor}{\sv 8421504}}'

C     Make a line light grey
      CHARACTER*34, PARAMETER:: LineLtGrey =
     +             '{\sp{\sn lineColor}{\sv 14855845}}'

C     Make a line very light grey
      CHARACTER*34, PARAMETER:: LineVLtGrey =
     +             '{\sp{\sn lineColor}{\sv 12961221}}'

C     Make a line colour red
      CHARACTER*29, PARAMETER:: LineRed =
     +             '{\sp{\sn lineColor}{\sv 255}}'

C     Make a line colour Light blue
      CHARACTER*34, PARAMETER:: LineLtBlu =
     +             '{\sp{\sn lineColor}{\sv 15773696}}'

C     Make a line colour White
      CHARACTER*34, PARAMETER:: LineWhite =
     +             '{\sp{\sn lineColor}{\sv 16777215}}'

C     Make a line colour Black
C     Just OMIT the "LineColour" entry

C     Announce a line thickness
      CHARACTER*24, PARAMETER:: LineThk =
     +             '{\sp{\sn lineWidth}{\sv '
                   ![Thickness value must follow directly (eg //'9525')]
                   ![//ShapeOK must then follow this]

C     Make the next shape a straight line
      CHARACTER*51, PARAMETER:: StrLine =
     +             '{\shpwr1\shp{\*\shpinst'//
     +             '{\sp{\sn shapeType}{\sv 20}}'

C     Make the next shape a rectangle
C     (a rectangle occupying the entire page width prevents word-wrap)
      CHARACTER*50, PARAMETER:: Box =
     +             '{\shpwr1\shp{\*\shpinst'//
     +             '{\sp{\sn shapeType}{\sv 1}}'

C     Make the next shape a white-lined rectangle
C     (a rectangle occupying the entire page width prevents word-wrap)
      CHARACTER*77, PARAMETER:: BoxNoLine =
     +             '{\shpwr1\shp{\*\shpinst'//
     +             '{\sp{\sn shapeType}{\sv 1}}'//
     +             '{\sp{\sn fLine}{\sv FALSE}}'

C     Make the next shape an ellipse
      CHARACTER*50, PARAMETER:: Ellipse =
     +             '{\shpwr1\shp{\*\shpinst'//
     +             '{\sp{\sn shapeType}{\sv 3}}'

C     Make the next shape a 'doughnut'
      CHARACTER*51, PARAMETER:: Donut =
     +             '{\shpwr1\shp{\*\shpinst'//
     +             '{\sp{\sn shapeType}{\sv 23}}'

C     Make the next shape an arrow
      CHARACTER*51, PARAMETER:: Arrow =
     +             '{\shpwr1\shp{\*\shpinst'//
     +             '{\sp{\sn shapeType}{\sv 13}}'

C     Make the next shape a text box
      CHARACTER*290,PARAMETER:: TxtBox =
     +             '{\shp{\*\shpinst '//
     +             '{\sp{\sn ShapeType}{\sv 202}}'//
     +             '{\sp{\sn fFitShapeToText}{\sv 1}}'//
     +             '{\sp{\sn anchorText}{\sv 4}}'//
     +             '{\sp{\sn fFilled}{\sv 0}}'//
     +             '{\sp{\sn fLine}{\sv 0}}'//
     +             '{\sp{\sn dxTextLeft}{\sv 4}}'//
     +             '{\sp{\sn dyTextTop}{\sv 4}}}'//
     +             '{\sp{\sn dxTextRight}{\sv 4}'//
     +             '{\sp{\sn dyTextBottom}{\sv 4}}'//
     +             '\shpfblwtxt1 \shptxt ' ![Text must follow directly]

C     Place the shape
      CHARACTER*8,  PARAMETER:: ShapeL = '\shpleft'
      CHARACTER*7,  PARAMETER:: ShapeT = '\shptop'

      CHARACTER*9,  PARAMETER:: ShapeR = '\shpright'
      CHARACTER*10, PARAMETER:: ShapeB = '\shpbottom'

C     Activate the shape
      CHARACTER*2,  PARAMETER:: ShapeOK = '}}'

C     Complete the drawing
      CHARACTER*1,  PARAMETER:: DrgOK = '}'

      END MODULE RTF
