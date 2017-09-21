function ignnbn ( n, p )

!*****************************************************************************80
!
!! IGNNBN generates a negative binomial random deviate.
!
!  Discussion:
!
!    This procedure generates a single random deviate from a negative binomial
!    distribution.
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
!    Luc Devroye,
!    Non-Uniform Random Variate Generation,
!    Springer, 1986,
!    ISBN: 0387963057,
!    LC: QA274.D48.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the required number of events.
!    0 <= N.
!
!    Input, real ( kind = 4 ) P, the probability of an event during a
!    Bernoulli trial.  0.0 < P < 1.0.
!
!    Output, integer ( kind = 4 ) IGNNBN, a random deviate from
!    the distribution.
!
  implicit none

  real ( kind = 4 ) a
  real ( kind = 4 ) gengam
  integer ( kind = 4 ) ignnbn
  integer ( kind = 4 ) ignpoi
  integer ( kind = 4 ) n
  real ( kind = 4 ) p
  real ( kind = 4 ) r
  real ( kind = 4 ) y

  if ( n < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'IGNNBN - Fatal error!'
    write ( *, '(a)' ) '  N < 0.'
    stop 1
  end if

  if ( p <= 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'IGNNBN - Fatal error!'
    write ( *, '(a)' ) '  P <= 0.0'
    stop 1
  end if

  if ( 1.0E+00 <= p ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'IGNNBN - Fatal error!'
    write ( *, '(a)' ) '  1.0 <= P'
    stop 1
  end if
!
!  Generate Y, a random gamma (n,(1-p)/p) variable.
!
  r = real ( n )
  a = p / ( 1.0E+00 - p )
  y = gengam ( a, r )
!
!  Generate a random Poisson ( y ) variable.
!
  ignnbn = ignpoi ( y )

  return
end
