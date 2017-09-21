function genbet ( aa, bb )

!*****************************************************************************80
!
!! GENBET generates a beta random deviate.
!
!  Discussion:
!
!    This procedure returns a single random deviate from the beta distribution
!    with parameters A and B.  The density is
!
!      x^(a-1) * (1-x)^(b-1) / Beta(a,b) for 0 < x < 1
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
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Russell Cheng,
!    Generating Beta Variates with Nonintegral Shape Parameters,
!    Communications of the ACM,
!    Volume 21, Number 4, April 1978, pages 317-322.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) AA, the first parameter of the beta distribution.
!    0.0 < AA.
!
!    Input, real ( kind = 4 ) BB, the second parameter of the beta distribution.
!    0.0 < BB.
!
!    Output, real ( kind = 4 ) GENBET, a beta random variate.
!
  implicit none

  real ( kind = 4 ) a
  real ( kind = 4 ) aa
  real ( kind = 4 ) alpha
  real ( kind = 4 ) b
  real ( kind = 4 ) bb
  real ( kind = 4 ) beta
  real ( kind = 4 ) delta
  real ( kind = 4 ) gamma
  real ( kind = 4 ) genbet
  real ( kind = 4 ) k1
  real ( kind = 4 ) k2
  real ( kind = 4 ), parameter :: log4 = 1.3862943611198906188E+00
  real ( kind = 4 ), parameter :: log5 = 1.6094379124341003746E+00
  real ( kind = 4 ) r
  real ( kind = 4 ) r4_exp
  real ( kind = 4 ) random_number_f
  real ( kind = 4 ) s
  real ( kind = 4 ) t
  real ( kind = 4 ) u1
  real ( kind = 4 ) u2
  real ( kind = 4 ) v
  real ( kind = 4 ) w
  real ( kind = 4 ) y
  real ( kind = 4 ) z

  if ( aa <= 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENBET - Fatal error!'
    write ( *, '(a)' ) '  AA <= 0.0'
    stop 1
  end if

  if ( bb <= 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENBET - Fatal error!'
    write ( *, '(a)' ) '  BB <= 0.0'
    stop 1
  end if
!
!  Algorithm BB
!
  if ( 1.0E+00 < aa .and. 1.0E+00 < bb ) then

    a = min ( aa, bb )
    b = max ( aa, bb )
    alpha = a + b
    beta = sqrt ( ( alpha - 2.0E+00 ) / ( 2.0E+00 * a * b - alpha ) )
    gamma = a + 1.0E+00 / beta

    do

      u1 = random_number_f ( )
      u2 = random_number_f ( )
      v = beta * log ( u1 / ( 1.0E+00 - u1 ) )
!
!  exp ( v ) replaced by r4_exp ( v )
!
      w = a * r4_exp ( v )

      z = u1 ** 2 * u2
      r = gamma * v - log4
      s = a + r - w

      if ( 5.0E+00 * z <= s + 1.0E+00 + log5 ) then
        exit
      end if

      t = log ( z )
      if ( t <= s ) then
        exit
      end if

      if ( t <= ( r + alpha * log ( alpha / ( b + w ) ) ) ) then
        exit
      end if

    end do
!
!  Algorithm BC
!
  else

    a = max ( aa, bb )
    b = min ( aa, bb )
    alpha = a + b
    beta = 1.0E+00 / b
    delta = 1.0E+00 + a - b
    k1 = delta * ( 1.0E+00 / 72.0E+00 + b / 24.0E+00 ) &
      / ( a / b - 7.0E+00 / 9.0E+00 )
    k2 = 0.25E+00 + ( 0.5E+00 + 0.25E+00 / delta ) * b

    do

      u1 = random_number_f ( )
      u2 = random_number_f ( )

      if ( u1 < 0.5E+00 ) then

        y = u1 * u2
        z = u1 * y

        if ( k1 <= 0.25E+00 * u2 + z - y ) then
          cycle
        end if

      else

        z = u1 ** 2 * u2

        if ( z <= 0.25E+00 ) then

          v = beta * log ( u1 / ( 1.0E+00 - u1 ) )
          w = a * exp ( v )

          if ( aa == a ) then
            genbet = w / ( b + w )
          else
            genbet = b / ( b + w )
          end if

          return

        end if

        if ( k2 < z ) then
          cycle
        end if

      end if

      v = beta * log ( u1 / ( 1.0E+00 - u1 ) )
      w = a * exp ( v )

      if ( log ( z ) <= alpha * ( log ( alpha / ( b + w ) ) + v ) - log4 ) then
        exit
      end if

    end do

  end if

  if ( aa == a ) then
    genbet = w / ( b + w )
  else
    genbet = b / ( b + w )
  end if

  return
end
