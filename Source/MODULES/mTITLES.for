      MODULE TITLES
C     -------------
C     jw 18-02-12 draft
C     jw 17-07-12 last amended

      LOGICAL  MadeTIT, ShowsTIT, GoodTIT

      INTEGER  idTitPan,
     +            idTitPan1,
     +            idTitPan2,
     +            idTitPan3,
     +         idTL1,
     +         idTL2,
     +         idTL3,
     +         idENG

      CHARACTER
     +         PROG8*8,VERS4*4,PDAT9*9,DESC24*24,DESC48*48,DESC70*70,
     +         UDat*9, AUTH16*16,FRM32*32,TEL1*20,TEL2*20,EML24*24,
     +         CPRT17*17,VERSTR*70

      CHARACTER
     +         TITLE1*24,TITLE2*24,TITLE3*24,DESC*24,ENG*16

      INTEGER  IPAGE,ILINE

      INTEGER  LP,LV,LD,LA,LE,
     +         COLW1,COLW2,COLW3,COLFRE,
     +         LT1,LT2,LT3,MORXX,KOL3

      END MODULE TITLES