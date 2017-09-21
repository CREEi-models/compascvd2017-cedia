subroutine trstat ( pdf, parin, av, var )

!*****************************************************************************80
!
!! TRSTAT returns the mean and variance for distributions.
!
!  Discussion:
!
!    This procedure returns the mean and variance for a number of statistical
!    distributions as a function of their parameters.
!
!    The input vector PARIN is used to pass in the parameters necessary
!    to specify the distribution.  The number of these parameters varies
!    per distribution, and it is necessary to specify an ordering for the
!    parameters used to a given distribution.  The ordering chosen here
!    is as follows:
!
!    bet
!      PARIN(1) is A
!      PARIN(2) is B
!    bin
!      PARIN(1) is Number of trials
!      PARIN(2) is Prob Event at Each Trial
!    chi
!      PARIN(1) = df
!    exp
!      PARIN(1) = mu
!    f
!      PARIN(1) is df numerator
!      PARIN(2) is df denominator
!    gam
!      PARIN(1) is A
!      PARIN(2) is R
!    nbn
!      PARIN(1) is N
!      PARIN(2) is P
!    nch
!      PARIN(1) is df
!      PARIN(2) is noncentrality parameter
!    nf
!      PARIN(1) is df numerator
!      PARIN(2) is df denominator
!      PARIN(3) is noncentrality parameter
!    nor
!      PARIN(1) is mean
!      PARIN(2) is standard deviation
!    poi
!      PARIN(1) is Mean
!    unf
!      PARIN(1) is LOW bound
!      PARIN(2) is HIGH bound
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character * ( 4 ) PDF, indicates the distribution:
!    'bet'  beta distribution
!    'bin'  binomial
!    'chi'  chisquare
!    'exp'  exponential
!    'f'    F (variance ratio)
!    'gam'  gamma
!    'nbn'  negative binomial
!    'nch'  noncentral chisquare
!    'nf'   noncentral f
!    'nor'  normal
!    'poi'  Poisson
!    'unf'  uniform
!
!    Input, real ( kind = 4 ) PARIN(*), the parameters of the distribution.
!
!    Output, real ( kind = 4 ) AV, the mean of the specified distribution.
!
!    Output, real ( kind = 4 ) VAR, the variance of the specified distribuion.
!
  implicit none

  real ( kind = 4 ) a
  real ( kind = 4 ) av
  real ( kind = 4 ) b
  integer ( kind = 4 ) n
  real ( kind = 4 ) p
  real ( kind = 4 ) parin(*)
  character * ( 4 ) pdf
  real ( kind = 4 ) r
  real ( kind = 4 ) var
  real ( kind = 4 ) width

  if ( pdf == 'bet' ) then

    av = parin(1) / ( parin(1) + parin(2) )
    var = ( av * parin(2) ) / ( ( parin(1) + parin(2) ) * &
      ( parin(1) + parin(2) + 1.0E+00 ) )

  else if ( pdf == 'bin' ) then

    n = int ( parin(1) )
    p = parin(2)
    av = real ( n ) * p
    var = real ( n ) * p * ( 1.0E+00 - p )

  else if ( pdf == 'chi' ) then

    av = parin(1)
    var = 2.0E+00 * parin(1)

  else if ( pdf == 'exp' ) then

    av = parin(1)
    var = av ** 2

  else if ( pdf == 'f' ) then

    if ( parin(2) <= 2.0001E+00 ) then
      av = -1.0E+00
    else
      av = parin(2) / ( parin(2) - 2.0E+00 )
    end if

    if ( parin(2) <= 4.0001E+00 ) then
      var = -1.0E+00
    else
      var = ( 2.0E+00 * parin(2) ** 2 * ( parin(1) + parin(2) - 2.0E+00 ) ) / &
        ( parin(1) * ( parin(2) - 2.0E+00 ) ** 2 * ( parin(2) - 4.0E+00 ) )
    end if

  else if ( pdf == 'gam' ) then

    a = parin(1)
    r = parin(2)
    av = r / a
    var = r / a ** 2

  else if ( pdf == 'nbn' ) then

    n = int ( parin(1) )
    p = parin(2)
    av = n * ( 1.0E+00 - p ) / p
    var = n * ( 1.0E+00 - p ) / p ** 2

  else if ( pdf == 'nch' ) then

    a = parin(1) + parin(2)
    b = parin(2) / a
    av = a
    var = 2.0E+00 * a * ( 1.0E+00 + b )

  else if ( pdf == 'nf' ) then

    if ( parin(2) <= 2.0001E+00 ) then
      av = -1.0E+00
    else
      av = ( parin(2) * ( parin(1) + parin(3) ) ) &
        / ( ( parin(2) - 2.0E+00 ) * parin(1) )
    end if

    if ( parin(2) <= 4.0001E+00 ) then
      var = -1.0E+00
    else
      a = ( parin(1) + parin(3) ) ** 2 &
        + ( parin(1) + 2.0E+00 * parin(3) ) * ( parin(2) - 2.0E+00 )
      b = ( parin(2) - 2.0E+00 ) ** 2 * ( parin(2) - 4.0E+00 )
      var = 2.0E+00 * ( parin(2) / parin(1) ) ** 2 * ( a / b )
    end if

  else if ( pdf == 'nor' ) then

    av = parin(1)
    var = parin(2) ** 2

  else if ( pdf == 'poi' ) then

    av = parin(1)
    var = parin(1)

  else if ( pdf == 'unf' ) then

    width = parin(2) - parin(1)
    av = parin(1) + width / 2.0E+00
    var = width ** 2 / 12.0E+00

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TRSTAT - Fatal error!'
    write ( *, '(a)' ) '  Illegal input value for PDF.'
    stop 1

  end if

  return
end
