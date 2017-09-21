function r8vec_covar ( n, x, y )

!*****************************************************************************80
!
!! R8VEC_COVAR computes the covariance of two vectors.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 April 2013
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(N), Y(N), the two vectors.
!
!    Input, integer ( kind = 4 ) N, the dimension of the two vectors.
!
!    Output, real ( kind = 8 ) R4VEC_COVAR, the covariance of the vectors.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8vec_covar
  real ( kind = 8 ) value
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_average
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) y_average

  x_average = sum ( x(1:n) ) / real ( n, kind = 8 )
  y_average = sum ( y(1:n) ) / real ( n, kind = 8 )

  value = 0.0D+00
  do i = 1, n
    value = value + ( x(i) - x_average ) * ( y(i) - y_average )
  end do

  r8vec_covar = value / real ( n - 1, kind = 8 )

  return
end
