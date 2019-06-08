      CHARACTER*80 FUNCTION PADSTR(STRING,LSTRG,PADCHR,IFROM)
C     -------------------------------------------------------
C     jw / 26-12-08  written
C     jw / 26-12-08  last rev.

C     Pads STRING(1:LSTRG) with PADCHRs, starting from the IFROMth char.
C

      CHARACTER STRING*80, PADCHR*1
      INTEGER   LSTRG, IFROM

	DO 100 I=IFROM,LSTRG
100      PADSTR(I:I) = PADCHR

      RETURN
      END
