      FUNCTION aIF(TEST,TRUEVAL,FALSVAL)
C     ----------------------------------
C     jw / 07-04-12  written
C     jw / 07-04-12  last revised

C     Returns the string before the '$' in either TRUEVAL or FALSVAL,
C     depending on TEST

      CHARACTER AIF*60, TrueVAL*60, FalsVAL*60
      LOGICAL TEST

C     IF(TEST) THEN 
C        idx=INDEX(TrueVAL,'$')
C        IF(idx.EQ.0) THEN
C           aIF=''
C        ELSE
C           aIF=''//TrueVAL(1:idx-1)
C        END IF !(idx.EQ.0)
C     ELSE
C        idx=INDEX(FalsVAL,'$')
C        IF(idx.EQ.0) THEN
C           aIF=''
C        ELSE
C           aIF=''//FalsVAL(1:idx-1)
C        END IF !(idx.EQ.0)
C     END IF !(TEST)
      
      aIF = ''
      
      IF(TEST) THEN
         idx=INDEX(TrueVAL,'$')
         IF(idx.EQ.0) RETURN
         aIF=''//TrueVAL(1:idx-1)
      ELSE
         idx=INDEX(FalsVAL,'$')
         IF(idx.EQ.0) RETURN
         aIF=''//FalsVAL(1:idx-1)
      END IF !(TEST)
      

      RETURN
      END
