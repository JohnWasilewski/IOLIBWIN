      MODULE LUnits
C     -------------
C     Logical Units ('LUs') for I/O ;
C     jw / 23-04-84
C     jw / 28-10-00 converted to block data (was a subroutine)
C     jw / 16-07-12 converted to a module
C     jw / 17-07-12 last rev
C
C     Fortran Units
C     -------------
      INTEGER, PARAMETER :: LUI = 105  !Input     - File
      INTEGER, PARAMETER :: LUO = 106  !Output    - File
      INTEGER, PARAMETER :: LUK = 102  !Keyboard  - Keyboard input
      INTEGER, PARAMETER :: LUS = 103  !Screen    - Screen output

      LOGICAL LUISET, LUOSET, LUSSET, LUKSET,LU1SET,LU2SET,LU3SET,LU4SET
      INTEGER LUD

      CONTAINS

          SUBROUTINE LUinit
C         -----------------
          LUIset = .FALSE.
          LUOset = .FALSE.
          LU1set = .FALSE.
          LU2set = .FALSE.
          LU3set = .FALSE.
          LU4set = .FALSE.
          END SUBROUTINE

      END MODULE LUnits

