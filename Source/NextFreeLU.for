      INTEGER FUNCTION NextFreeLU()
!     -----------------------------
!     Returns next free FORTRAN unit as an integer between 1 and 99,
!     which is not currently associated with an I/O device.
!
!     Never returns units 5 or 6, which are assumed to be special.
!
!     Returns NextFreeLU = 0 if no free FORTRAN unit could be found
!     among any of the 99 units other than units 5 and 6.
!
!     Written by John Burkardt on 02 March 1999 as subroutine get_unit
!
      implicit none

      integer i
      integer ios
      logical lopen

      NextFreeLU = 0

      do i = 100, 199

        if ( i /= 5 .and. i /= 6 ) then
            inquire ( unit = i, opened = lopen, iostat = ios )

            if ( ios == 0 ) then
                if ( .not. lopen ) then
                   NextFreeLU = i
                   return
                end if

            end if
        end if
      end do

      return
      end