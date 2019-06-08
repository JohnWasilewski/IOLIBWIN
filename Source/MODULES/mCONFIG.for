      MODULE CONFIG
C     -------------
C     jw 16-07-12 draft
C     jw 16-07-12 last amended

      INTEGER,  PARAMETER ::  
     +          SPGWID =   85, !screen page width
     +          SCLINW =   85, !screen characters per line
     +          SCRLPP = 9999, !screen lines per page
     +          SPGHGT = 9999  !screen page height

      INTEGER,  PARAMETER ::  
     +          PPGWID =  210, !outfile page width, mm
     +          PPGHGT =  297, !outfile page height, mm
     +          PRLINW =   83, !outfile characters per line
     +          PRLIPP =   65, !outfile lines per page
     +          PRMLHS =   19, !outfile L margin, mm
     +          PRMRHS =   15, !outfile R margin, mm
     +          PRMTOP =   16, !outfile T margin, mm
     +          PRMBTM =   16  !outfile B margin, mm

      END MODULE CONFIG