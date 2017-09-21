subroutine stats ( x, n, av, var, xmin, xmax )

!*****************************************************************************80
!
!! STATS computes statistics for a given array.
!
!  Discussion:
!
!    This procedure computes the average and variance of an array.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) X(N), the array to be analyzed.
!
!    Input, integer ( kind = 4 ) N, the dimension of the array.
!
!    Output, real ( kind = 4 ) AV, the average value.
!
!    Output, real ( kind = 4 ) VAR, the variance.
!
!    Output, real ( kind = 4 ) XMIN, XMAX, the minimum and maximum entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 4 ) av
  integer ( kind = 4 ) i
  real ( kind = 4 ) total
  real ( kind = 4 ) var
  real ( kind = 4 ) x(n)
  real ( kind = 4 ) xmax
  real ( kind = 4 ) xmin

  xmin = x(1)
  xmax = x(1)
  total = 0.0E+00
  do i = 1, n
    total = total + x(i)
    xmin = min ( xmin, x(i) )
    xmax = max ( xmax, x(i) )
  end do

  av = total / real ( n )

  total = 0.0E+00
  do i = 1, n
    total = total + ( x(i) - av ) ** 2
  end do
  var = total / real ( n - 1 )

  return
end
