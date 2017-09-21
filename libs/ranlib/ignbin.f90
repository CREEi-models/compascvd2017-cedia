function ignbin ( n, pp )

!*****************************************************************************80
!
!! IGNBIN generates a binomial random deviate.
!
!  Discussion:
!
!    This procedure generates a single random deviate from a binomial
!    distribution whose number of trials is N and whose
!    probability of an event in each trial is P.
!
!    The previous version of this program relied on the assumption that
!    local memory would be preserved between calls.  It set up data
!    one time to be preserved for use over multiple calls.  In the
!    interests of portability, this assumption has been removed, and
!    the "setup" data is recomputed on every call.
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
!    Voratas Kachitvichyanukul, Bruce Schmeiser,
!    Binomial Random Variate Generation,
!    Communications of the ACM,
!    Volume 31, Number 2, February 1988, pages 216-222.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of binomial trials, from which a
!    random deviate will be generated.
!    0 < N.
!
!    Input, real ( kind = 4 ) PP, the probability of an event in each trial of
!    the binomial distribution from which a random deviate is to be generated.
!    0.0 < PP < 1.0.
!
!    Output, integer ( kind = 4 ) IGNBIN, a random deviate from the
!    distribution.
!
  implicit none

  real ( kind = 4 ) al
  real ( kind = 4 ) alv
  real ( kind = 4 ) amaxp
  real ( kind = 4 ) c
  real ( kind = 4 ) f
  real ( kind = 4 ) f1
  real ( kind = 4 ) f2
  real ( kind = 4 ) ffm
  real ( kind = 4 ) fm
  real ( kind = 4 ) g
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ignbin
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) ix1
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mp
  real ( kind = 4 ) pp
  integer ( kind = 4 ) n
  real ( kind = 4 ) p
  real ( kind = 4 ) p1
  real ( kind = 4 ) p2
  real ( kind = 4 ) p3
  real ( kind = 4 ) p4
  real ( kind = 4 ) q
  real ( kind = 4 ) qn
  real ( kind = 4 ) r
  real ( kind = 4 ) random_number_f
  real ( kind = 4 ) t
  real ( kind = 4 ) u
  real ( kind = 4 ) v
  real ( kind = 4 ) w
  real ( kind = 4 ) w2
  real ( kind = 4 ) x
  real ( kind = 4 ) x1
  real ( kind = 4 ) x2
  real ( kind = 4 ) xl
  real ( kind = 4 ) xll
  real ( kind = 4 ) xlr
  real ( kind = 4 ) xm
  real ( kind = 4 ) xnp
  real ( kind = 4 ) xnpq
  real ( kind = 4 ) xr
  real ( kind = 4 ) ynorm
  real ( kind = 4 ) z
  real ( kind = 4 ) z2

  if ( pp <= 0.0E+00 .or. 1.0E+00 <= pp ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'IGNBIN - Fatal error!'
    write ( *, '(a)' ) '  PP is out of range.'
    stop 1
  end if

  p = min ( pp, 1.0E+00 - pp )
  q = 1.0E+00 - p
  xnp = real ( n, kind = 4 ) * p

  if ( xnp < 30.0E+00 ) then

    qn = q ** n
    r = p / q
    g = r * real ( n + 1, kind = 4 )

    do

      ix = 0
      f = qn
      u = random_number_f ( )

      do

        if ( u < f ) then
          if ( 0.5E+00 < pp ) then
            ix = n - ix
          end if
          ignbin = ix
          return
        end if

        if ( 110 < ix ) then
          exit
        end if

        u = u - f
        ix = ix + 1
        f = f * ( g / real ( ix, kind = 4 ) - r )

      end do

    end do

  end if

  ffm = xnp + p
  m = ffm
  fm = m
  xnpq = xnp * q
  p1 = int ( 2.195E+00 * sqrt ( xnpq ) - 4.6E+00 * q ) + 0.5E+00
  xm = fm + 0.5E+00
  xl = xm - p1
  xr = xm + p1
  c = 0.134E+00 + 20.5E+00 / ( 15.3E+00 + fm )
  al = ( ffm - xl ) / ( ffm - xl * p )
  xll = al * ( 1.0E+00 + 0.5E+00 * al )
  al = ( xr - ffm ) / ( xr * q )
  xlr = al * ( 1.0E+00 + 0.5E+00 * al )
  p2 = p1 * ( 1.0E+00 + c + c )
  p3 = p2 + c / xll
  p4 = p3 + c / xlr
!
!  Generate a variate.
!
  do

    u = random_number_f ( ) * p4
    v = random_number_f ( )
!
!  Triangle
!
    if ( u < p1 ) then
      ix = xm - p1 * v + u
      if ( 0.5E+00 < pp ) then
        ix = n - ix
      end if
      ignbin = ix
      return
    end if
!
!  Parallelogram
!
    if ( u <= p2 ) then

      x = xl + ( u - p1 ) / c
      v = v * c + 1.0E+00 - abs ( xm - x ) / p1

      if ( v <= 0.0E+00 .or. 1.0E+00 < v ) then
        cycle
      end if

      ix = x

    else if ( u <= p3 ) then

      ix = xl + log ( v ) / xll
      if ( ix < 0 ) then
        cycle
      end if
      v = v * ( u - p2 ) * xll

    else

      ix = xr - log ( v ) / xlr
      if ( n < ix ) then
        cycle
      end if
      v = v * ( u - p3 ) * xlr

    end if

    k = abs ( ix - m )

    if ( k <= 20 .or. xnpq / 2.0 - 1.0 <= k ) then

      f = 1.0E+00
      r = p / q
      g = ( n + 1 ) * r

      if ( m < ix ) then
        mp = m + 1
        do i = m + 1, ix
          f = f * ( g / i - r )
        end do
      else if ( ix < m ) then
        ix1 = ix + 1
        do i = ix + 1, m
          f = f / ( g / real ( i, kind = 4 ) - r )
        end do
      end if

      if ( v <= f ) then
        if ( 0.5E+00 < pp ) then
          ix = n - ix
        end if
        ignbin = ix
        return
      end if

    else

      amaxp = ( k / xnpq ) * ( ( k * ( k / 3.0E+00 &
        + 0.625E+00 ) + 0.1666666666666E+00 ) / xnpq + 0.5E+00 )
      ynorm = - real ( k * k, kind = 4 ) / ( 2.0E+00 * xnpq )
      alv = log ( v )

      if ( alv < ynorm - amaxp ) then
        if ( 0.5E+00 < pp ) then
          ix = n - ix
        end if
        ignbin = ix
        return
      end if

      if ( ynorm + amaxp < alv ) then
        cycle
      end if

      x1 = real ( ix + 1, kind = 4 )
      f1 = fm + 1.0E+00
      z = real ( n + 1, kind = 4 ) - fm
      w = real ( n - ix + 1, kind = 4 )
      z2 = z * z
      x2 = x1 * x1
      f2 = f1 * f1
      w2 = w * w

      t = xm * log ( f1 / x1 ) + ( n - m + 0.5E+00 ) * log ( z / w ) &
        + real ( ix - m, kind = 4 ) * log ( w * p / ( x1 * q ) ) &
        + ( 13860.0E+00 - ( 462.0E+00 - ( 132.0E+00 - ( 99.0E+00 - 140.0E+00 &
        / f2 ) / f2 ) / f2 ) / f2 ) / f1 / 166320.0E+00 &
        + ( 13860.0E+00 - ( 462.0E+00 - ( 132.0E+00 - ( 99.0E+00 - 140.0E+00 &
        / z2 ) / z2 ) / z2 ) / z2 ) / z / 166320.0E+00 &
        + ( 13860.0E+00 - ( 462.0E+00 - ( 132.0E+00 - ( 99.0E+00 - 140.0E+00 &
        / x2 ) / x2 ) / x2 ) / x2 ) / x1 / 166320.0E+00 &
        + ( 13860.0E+00 - ( 462.0E+00 - ( 132.0E+00 - ( 99.0E+00 - 140.0E+00 &
        / w2 ) / w2 ) / w2 ) / w2 ) / w / 166320.0E+00

      if ( alv <= t ) then
        if ( 0.5E+00 < pp ) then
          ix = n - ix
        end if
        ignbin = ix
        return
      end if

    end if

  end do

  return
end
