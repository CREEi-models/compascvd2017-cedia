function r4_exponential_sample ( lambda )

!*****************************************************************************80
!
!! R4_EXPONENTIAL_SAMPLE samples the exponential PDF.
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
!    18 April 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 4 ) LAMBDA, the parameter of the PDF.
!
!    Output, real ( kind = 4 ) R4_EXPONENTIAL_SAMPLE, a sample of the PDF.
!
  implicit none

  real ( kind = 4 ) lambda
  real ( kind = 4 ) r
  real ( kind = 4 ) r4_exponential_sample
  real ( kind = 4 ) random_number_f

  r = random_number_f ( )

  r4_exponential_sample = - log ( r ) * lambda

  return
end
