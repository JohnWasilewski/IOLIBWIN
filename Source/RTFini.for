      SUBROUTINE RTFini
C     -----------------
      USE RTF
      USE Config
      USE Titles

      CHARACTER*16 I2CHAR
      INTEGER PPGWIDTW, PPGHGTTW,
     +        PRMLHSTW, PRMRHSTW, PRMTOPTW, PRMBTMTW                                                                                                                                                                                                                                                                                                                                                                                                                                                                         REAL*4 MMperLINE, TWperLINE

      PPGWIDTW = INT(TWperMM*PPGWID)
      PPGHGTTW = INT(TWperMM*PPGHGT)
      PRMLHSTW = INT(TWperMM*PRMLHS)
      PRMRHSTW = INT(TWperMM*PRMRHS)
      PRMTOPTW = INT(TWperMM*PRMTOP)
      PRMBTMTW = INT(TWperMM*PRMBTM)

      RTFiniSTART$ =
     + '{\rtf1 '//
     + '{\*\generator'//
     + ' Program '//TRIM(PROG8)//
     + ' vers.'//TRIM(VERS4)//': '//PDAT9//
     + ' Copyright '//TRIM(CPRT17)//' '//TRIM(AUTH16)//'}'//
     + '\ansi \deff0 '

      RTFiniPAGE$ =
     + '\paperw'//TRIM(I2CHAR(PPGWIDTW,iDUM))//
     + '\paperh'//TRIM(I2CHAR(PPGHGTTW,iDUM))//
     + '\margl' //TRIM(I2CHAR(PRMLHSTW,iDUM))//
     + '\margr' //TRIM(I2CHAR(PRMRHSTW,iDUM))//
     + '\margt' //TRIM(I2CHAR(PRMTOPTW,iDUM))//
     + '\margb' //TRIM(I2CHAR(PRMBTMTW,iDUM))

C     Warning: MODULE RTF assigns PARAMETER values to these 2 variables.
C     Which is correct?  That or this?
C     MMperLINE = Float((PPGHGT-PRMTOP-PRMBTM)/PRLIPP)
C     TWperLINE = MMperLINE * TWperMM

      RETURN
      END