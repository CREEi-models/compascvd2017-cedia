function r4_exp ( x )

!*****************************************************************************80
!
!! R4_EXP computes the exponential of an R8, avoiding overflow and underflow.
!
!  Discussion:
!
!    For arguments of very large magnitude, the evaluation of the
!    exponential function can cause computational problems.  Some languages
!    and compilers may return an infinite value or a "Not-a-Number".
!    An alternative, when dealing with a wide range of inputs, is simply
!    to truncate the calculation for arguments whose magnitude is too large.
!    Whether this is the right or convenient approach depends on the problem
!    you are dealing with, and whether or not you really need accurate
!    results for large magnitude inputs, or you just want your code to
!    stop crashing.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 4 ) X, the argument of the exponential function.
!
!    Output, real ( kind = 4 ) R4_EXP, the value of exp ( X ).
!
  implicit none

  real ( kind = 4 ) r4_exp
  real ( kind = 4 ), parameter :: r4_huge = 1.0E+30
  real ( kind = 4 ), parameter :: r4_log_max = +69.0776E+00
  real ( kind = 4 ), parameter :: r4_log_min = -69.0776E+00
  real ( kind = 4 ) value
  real ( kind = 4 ) x

  if ( x <= r4_log_min ) then
    value = 0.0E+00
  else if ( x < r4_log_max ) then
    value = exp ( x )
  else
    value = r4_huge
  end if

  r4_exp = value

  return
end
