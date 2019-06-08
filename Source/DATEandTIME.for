      CHARACTER*9 FUNCTION DateToday()
C     --------------------------------
      CHARACTER*8  DATE8
C     INTEGER*2    IMON
      CHARACTER*3, PARAMETER :: MONTH(1:12) = 
     +    (/'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     +      'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /)

      CALL Date_and_Time(DATE=Date8)
      READ(Date8(5:6),'(I2)') IMON
      DateToday(1:9) = DATE8(7:8)//'/'//MONTH(IMON)//'/'//DATE8(3:4)

      RETURN
      END



      CHARACTER*5 FUNCTION TimeNow()
C     ------------------------------
      CHARACTER*10  Time10

      CALL Date_and_Time(TIME=Time10)
      TimeNow(1:5) = Time10(1:2)//':'//Time10(3:4)

      RETURN
      END