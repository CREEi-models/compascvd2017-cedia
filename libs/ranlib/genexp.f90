function genexp ( av )

!*****************************************************************************80
!
!! GENEXP generates an exponential random deviate.
!
!  Discussion:
!
!    This procedure generates a single random deviate from an exponential
!    distribution with mean AV.
!
!    See also the function R4_EXPONENTIAL_SAMPLE.
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
!    Computer Methods for Sampling From the
!    Exponential and Normal Distributions,
!    Communications of the ACM,
!    Volume 15, Number 10, October 1972, pages 873-882.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) AV, the mean of the exponential distribution
!    from which a random deviate is to be generated.
!
!    Output, real ( kind = 4 ) GENEXP, a random deviate from the distribution.
!
  implicit none

  real ( kind = 4 ) av
  real ( kind = 4 ) genexp
  real ( kind = 4 ) sexpo

  genexp = sexpo ( ) * av

  return
end
