function sgamma ( a )

!*****************************************************************************80
!
!! SGAMMA samples the standard Gamma distribution.
!
!  Discussion:
!
!    This procedure corresponds to algorithm GD in the reference.
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
!  Reference:
!
!    Joachim Ahrens, Ulrich Dieter,
!    Generating Gamma Variates by a Modified Rejection Technique,
!    Communications of the ACM,
!    Volume 25, Number 1, January 1982, pages 47-54.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) A, the parameter of the standard gamma
!    distribution.  0.0 < A < 1.0.
!
!    Output, real ( kind = 4 ) SGAMMA, a random deviate from the distribution.
!
  implicit none

  real ( kind = 4 ) a
  real ( kind = 4 ), parameter :: a1 =  0.3333333E+00
  real ( kind = 4 ), parameter :: a2 = -0.2500030E+00
  real ( kind = 4 ), parameter :: a3 =  0.2000062E+00
  real ( kind = 4 ), parameter :: a4 = -0.1662921E+00
  real ( kind = 4 ), parameter :: a5 =  0.1423657E+00
  real ( kind = 4 ), parameter :: a6 = -0.1367177E+00
  real ( kind = 4 ), parameter :: a7 =  0.1233795E+00
  real ( kind = 4 ) b
  real ( kind = 4 ) c
  real ( kind = 4 ) d
  real ( kind = 4 ) e
  real ( kind = 4 ), parameter :: e1 = 1.0E+00
  real ( kind = 4 ), parameter :: e2 = 0.4999897E+00
  real ( kind = 4 ), parameter :: e3 = 0.1668290E+00
  real ( kind = 4 ), parameter :: e4 = 0.0407753E+00
  real ( kind = 4 ), parameter :: e5 = 0.0102930E+00
  real ( kind = 4 ) p
  real ( kind = 4 ) q
  real ( kind = 4 ) q0
  real ( kind = 4 ), parameter :: q1 =  0.04166669E+00
  real ( kind = 4 ), parameter :: q2 =  0.02083148E+00
  real ( kind = 4 ), parameter :: q3 =  0.00801191E+00
  real ( kind = 4 ), parameter :: q4 =  0.00144121E+00
  real ( kind = 4 ), parameter :: q5 = -0.00007388E+00
  real ( kind = 4 ), parameter :: q6 =  0.00024511E+00
  real ( kind = 4 ), parameter :: q7 =  0.00024240E+00
  real ( kind = 4 ) r
  real ( kind = 4 ) random_number_f
  real ( kind = 4 ) s
  real ( kind = 4 ) s2
  real ( kind = 4 ) sexpo
  real ( kind = 4 ) si
  real ( kind = 4 ) sgamma
  real ( kind = 4 ) snorm
  real ( kind = 4 ), parameter :: sqrt32 = 5.656854E+00
  real ( kind = 4 ) t
  real ( kind = 4 ) u
  real ( kind = 4 ) v
  real ( kind = 4 ) w
  real ( kind = 4 ) x

  if ( 1.0E+00 <= a ) then

    s2 = a - 0.5E+00
    s = sqrt ( s2 )
    d = sqrt32 - 12.0E+00 * s
!
!  Immediate acceptance.
!
    t = snorm ( )
    x = s + 0.5E+00 * t
    sgamma = x * x

    if ( 0.0E+00 <= t ) then
      return
    end if
!
!  Squeeze acceptance.
!
    u = random_number_f ( )
    if ( d * u <= t * t * t ) then
      return
    end if

    r = 1.0E+00 / a
    q0 = (((((( q7 &
      * r + q6 ) &
      * r + q5 ) &
      * r + q4 ) &
      * r + q3 ) &
      * r + q2 ) &
      * r + q1 ) &
      * r
!
!  Approximation depending on size of parameter A.
!
    if ( 13.022E+00 < a ) then
      b = 1.77E+00
      si = 0.75E+00
      c = 0.1515E+00 / s
    else if ( 3.686E+00 < a ) then
      b = 1.654E+00 + 0.0076E+00 * s2
      si = 1.68E+00 / s + 0.275E+00
      c = 0.062E+00 / s + 0.024E+00
    else
      b = 0.463E+00 + s + 0.178E+00 * s2
      si = 1.235E+00
      c = 0.195E+00 / s - 0.079E+00 + 0.16E+00 * s
    end if
!
!  Quotient test.
!
    if ( 0.0E+00 < x ) then

      v = 0.5E+00 * t / s

      if ( 0.25E+00 < abs ( v ) ) then
        q = q0 - s * t + 0.25E+00 * t * t + 2.0E+00 * s2 * log ( 1.0E+00 + v )
      else
        q = q0 + 0.5E+00 * t * t * (((((( a7 &
          * v + a6 ) &
          * v + a5 ) &
          * v + a4 ) &
          * v + a3 ) &
          * v + a2 ) &
          * v + a1 ) &
          * v
      end if

      if ( log ( 1.0E+00 - u ) <= q ) then
        return
      end if

    end if

    do

      e = sexpo ( )
      u = 2.0E+00 * random_number_f ( ) - 1.0E+00

      if ( 0.0E+00 <= u ) then
        t = b + abs ( si * e )
      else
        t = b - abs ( si * e )
      end if
!
!  Possible rejection.
!
      if ( t < -0.7187449E+00 ) then
        cycle
      end if
!
!  Calculate V and quotient Q.
!
      v = 0.5E+00 * t / s

      if ( 0.25E+00 < abs ( v ) ) then
        q = q0 - s * t + 0.25E+00 * t * t + 2.0E+00 * s2 * log ( 1.0E+00 + v )
      else
        q = q0 + 0.5E+00 * t * t * (((((( a7 &
          * v + a6 ) &
          * v + a5 ) &
          * v + a4 ) &
          * v + a3 ) &
          * v + a2 ) &
          * v + a1 ) &
          *  v
      end if
!
!  Hat acceptance.
!
      if ( q <= 0.0E+00 ) then
        cycle
      end if

      if ( 0.5E+00 < q ) then
        w = exp ( q ) - 1.0E+00
      else
        w = (((( e5 * q + e4 ) * q + e3 ) * q + e2 ) * q + e1 ) * q
      end if
!
!  May have to sample again.
!
      if ( c * abs ( u ) <= w * exp ( e - 0.5E+00 * t * t ) ) then
        exit
      end if

    end do

    x = s + 0.5E+00 * t
    sgamma = x * x

    return
!
!  Method for A < 1.
!
  else

    b = 1.0E+00 + 0.3678794E+00 * a

    do

      p = b * random_number_f ( )

      if ( p < 1.0E+00 ) then

        sgamma = exp ( log ( p ) / a )

        if ( sgamma <= sexpo ( ) ) then
          return
        end if

        cycle

      end if

      sgamma = - log ( ( b - p ) / a )

      if ( ( 1.0E+00 - a ) * log ( sgamma ) <= sexpo ( ) ) then
        exit
      end if

    end do

  end if

  return
end
