subroutine genprm ( iarray, n )

!*****************************************************************************80
!
!! GENPRM generates and applies a random permutation to an array.
!
!  Discussion:
!
!    To see the permutation explicitly, let the input array be
!    1, 2, ..., N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) IARRAY(N), an array to be permuted.
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) iarray(n)
  integer ( kind = 4 ) ignuin
  integer ( kind = 4 ) itmp
  integer ( kind = 4 ) iwhich

  do i = 1, n
    iwhich = ignuin ( i, n )
    itmp = iarray(iwhich)
    iarray(iwhich) = iarray(i)
    iarray(i) = itmp
  end do

  return
end
