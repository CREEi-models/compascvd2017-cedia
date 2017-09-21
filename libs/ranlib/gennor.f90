function gennor ( av, sd )

!*****************************************************************************80
!
!! GENNOR generates a normal random deviate.
!
!  Discussion:
!
!    This procedure generates a single random deviate from a normal distribution
!    with mean AV, and standard deviation SD.
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
!  Reference:
!
!    Joachim Ahrens, Ulrich Dieter,
!    Extensions of Forsythe's Method for Random
!    Sampling from the Normal Distribution,
!    Mathematics of Computation,
!    Volume 27, Number 124, October 1973, page 927-937.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) AV, the mean.
!
!    Input, real ( kind = 4 ) SD, the standard deviation.
!
!    Output, real ( kind = 4 ) GENNOR, a random deviate from the distribution.
!
  implicit none

  real ( kind = 4 ) av
  real ( kind = 4 ) gennor
  real ( kind = 4 ) sd
  real ( kind = 4 ) snorm

  gennor = sd * snorm ( ) + av

  return
end
