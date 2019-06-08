      MODULE FILES
C     ------------
C     jw 22-02-12 draft
C     jw 22-02-12 last amended
C     --------------------
C     FILES
C     --------------------
C     INPUT FILE
C     OUPUT FILE
C     Filename
C     Flags
C     Fortran unit, file menu ID, Menu-select IDs R, W, Wnew

      DOUBLE PRECISION ChkSUM ! Sum of all numeric data for use in a
                              ! simple test for any changed data.
                              ! See the CHANGED()function.

      TYPE ::  Fdat
         CHARACTER   :: EXT*6               !Filename EXTension
         LOGICAL     :: Exists              !Whether the file exists
         LOGICAL     :: OPEN                !Whether the file is OPEN
         INTEGER     :: LU
      END TYPE

      TYPE ::  FILE
         LOGICAL     :: NAMed               !Whether filename known
         CHARACTER   :: PATH*256            !PATHname
         CHARACTER   :: NAME*96            !FileNAMe
         LOGICAL     :: BAKexists           !Whether the BAK file exists
         LOGICAL     :: BAKdeletable        !Whether it is deletable
         LOGICAL     :: InpDataPosted       !Input data sent to output
         TYPE (Fdat) :: I, O
      END TYPE

      TYPE (FILE) :: F

      END MODULE FILES


