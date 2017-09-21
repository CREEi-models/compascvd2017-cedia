function genunf ( low, high )

!*****************************************************************************80
!
!! GENUNF generates a uniform random deviate.
!
!  Discussion:
!
!    This procedure generates a real deviate uniformly distributed between
!    LOW and HIGH.
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
!    Input, real ( kind = 4 ) LOW, HIGH, the lower and upper bounds.
!
!    Output, real ( kind = 4 ) GENUNF, a random deviate from the distribution.
!
  implicit none

  real ( kind = 4 ) genunf
  real ( kind = 4 ) high
  real ( kind = 4 ) low
  real ( kind = 4 ) random_number_f

  genunf = low + ( high - low ) * random_number_f ( )

  return
end
