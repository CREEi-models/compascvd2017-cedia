function r8_exponential_sample ( lambda )

!*****************************************************************************80
!
!! R8_EXPONENTIAL_SAMPLE samples the exponential PDF.
!
!  Discussion:
!
!    Note that the parameter LAMBDA is a multiplier.  In some formulations,
!    it is used as a divisor instead.
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
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) LAMBDA, the parameter of the PDF.
!
!    Output, real ( kind = 8 ) R8_EXPONENTIAL_SAMPLE, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) lambda
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_exponential_sample
  real ( kind = 8 ) r8_uni_01

  r = r8_uni_01 ( )

  r8_exponential_sample = - log ( r ) * lambda

  return
end
