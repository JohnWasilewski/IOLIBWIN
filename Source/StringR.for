      FUNCTION STRINGR(RValu,F)
C     -------------------------

C     Returns real RValu as a string
C     F is the format specification, such as '(F8.3)'

      Character STRINGR*20, F*80
      REAL RValu
      WRITE(STRINGR,F) RValu
      RETURN
      END