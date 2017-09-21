function ignpoi ( mu )

!*****************************************************************************80
!
!! IGNPOI generates a Poisson random deviate.
!
!  Discussion:
!
!    This procedure generates a single random deviate from a Poisson
!    distribution with given mean.
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
!    Computer Generation of Poisson Deviates
!    From Modified Normal Distributions,
!    ACM Transactions on Mathematical Software,
!    Volume 8, Number 2, June 1982, pages 163-179.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) MU, the mean of the Poisson distribution
!    from which a random deviate is to be generated.
!
!    Output, integer ( kind = 4 ) IGNPOI, a random deviate from
!    the distribution.
!
  implicit none

  real ( kind = 4 ), parameter :: a0 = -0.5E+00
  real ( kind = 4 ), parameter :: a1 =  0.3333333E+00
  real ( kind = 4 ), parameter :: a2 = -0.2500068E+00
  real ( kind = 4 ), parameter :: a3 =  0.2000118E+00
  real ( kind = 4 ), parameter :: a4 = -0.1661269E+00
  real ( kind = 4 ), parameter :: a5 =  0.1421878E+00
  real ( kind = 4 ), parameter :: a6 = -0.1384794E+00
  real ( kind = 4 ), parameter :: a7 =  0.1250060E+00
  real ( kind = 4 ) b1
  real ( kind = 4 ) b2
  real ( kind = 4 ) c
  real ( kind = 4 ) c0
  real ( kind = 4 ) c1
  real ( kind = 4 ) c2
  real ( kind = 4 ) c3
  real ( kind = 4 ) d
  real ( kind = 4 ) del
  real ( kind = 4 ) difmuk
  real ( kind = 4 ) e
  real ( kind = 4 ) fact(10)
  real ( kind = 4 ) fk
  real ( kind = 4 ) fx
  real ( kind = 4 ) fy
  real ( kind = 4 ) g
  integer ( kind = 4 ) ignpoi
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kflag
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  real ( kind = 4 ) mu
  real ( kind = 4 ) muold
  real ( kind = 4 ) muprev
  real ( kind = 4 ) omega
  real ( kind = 4 ) p
  real ( kind = 4 ) p0
  real ( kind = 4 ) px
  real ( kind = 4 ) py
  real ( kind = 4 ) q
  real ( kind = 4 ) random_number_f
  real ( kind = 4 ) s
  real ( kind = 4 ) sexpo
  real ( kind = 4 ) snorm
  real ( kind = 4 ) t
  real ( kind = 4 ) u
  real ( kind = 4 ) v
  real ( kind = 4 ) x
  real ( kind = 4 ) xx

  save fact

  data fact / 1.0E+00, 1.0E+00, 2.0E+00, 6.0E+00, 24.0E+00, &
    120.0E+00, 720.0E+00, 5040.0E+00, 40320.0E+00, 362880.0E+00 /
!
!  Start new table and calculate P0.
!
  if ( mu < 10.0E+00 ) then

    m = max ( 1, int ( mu ) )
    p = exp ( - mu )
    q = p
    p0 = p
!
!  Uniform sample for inversion method.
!
    do

      u = random_number_f ( )
      ignpoi = 0

      if ( u <= p0 ) then
        return
      end if
!
!  Creation of new Poisson probabilities.
!
      do k = 1, 35
        p = p * mu / real ( k )
        q = q + p
        if ( u <= q ) then
          ignpoi = k
          return
        end if
      end do

    end do

  else

    s = sqrt ( mu )
    d = 6.0E+00 * mu * mu
    l = int ( mu - 1.1484E+00 )
!
!  Normal sample.
!
    g = mu + s * snorm ( )

    if ( 0.0E+00 <= g ) then

      ignpoi = int ( g )
!
!  Immediate acceptance if large enough.
!
      if ( l <= ignpoi ) then
        return
      end if
!
!  Squeeze acceptance.
!
      fk = real ( ignpoi )
      difmuk = mu - fk
      u = random_number_f ( )

      if ( difmuk * difmuk * difmuk <= d * u ) then
        return
      end if

    end if
!
!  Preparation for steps P and Q.
!
    omega = 0.3989423E+00 / s
    b1 = 0.04166667E+00 / mu
    b2 = 0.3E+00 * b1 * b1
    c3 = 0.1428571E+00 * b1 * b2
    c2 = b2 - 15.0E+00 * c3
    c1 = b1 - 6.0E+00 * b2 + 45.0E+00 * c3
    c0 = 1.0E+00 - b1 + 3.0E+00 * b2 - 15.0E+00 * c3
    c = 0.1069E+00 / mu

    if ( 0.0E+00 <= g ) then

      kflag = 0

      if ( ignpoi < 10 ) then

        px = -mu
        py = mu ** ignpoi / fact(ignpoi+1)

      else

        del = 0.8333333E-01 / fk
        del = del - 4.8E+00 * del * del * del
        v = difmuk / fk

        if ( 0.25E+00 < abs ( v ) ) then
          px = fk * log ( 1.0E+00 + v ) - difmuk - del
        else
          px = fk * v * v * ((((((( a7 &
            * v + a6 ) &
            * v + a5 ) &
            * v + a4 ) &
            * v + a3 ) &
            * v + a2 ) &
            * v + a1 ) &
            * v + a0 ) - del
        end if

        py = 0.3989423E+00 / sqrt ( fk )

      end if

      x = ( 0.5E+00 - difmuk ) / s
      xx = x * x
      fx = -0.5E+00 * xx
      fy = omega * ((( c3 * xx + c2 ) * xx + c1 ) * xx + c0 )

      if ( kflag <= 0 ) then

        if ( fy - u * fy <= py * exp ( px - fx ) ) then
          return
        end if

      else

        if ( c * abs ( u ) <= py * exp ( px + e ) - fy * exp ( fx + e ) ) then
          return
        end if

      end if

    end if
!
!  Exponential sample.
!
    do

      e = sexpo ( )
      u = 2.0E+00 * random_number_f ( ) - 1.0E+00
      if ( u < 0.0E+00 ) then
        t = 1.8E+00 - abs ( e )
      else
        t = 1.8E+00 + abs ( e )
      end if

      if ( t <= -0.6744E+00 ) then
        cycle
      end if

      ignpoi = int ( mu + s * t )
      fk = real ( ignpoi )
      difmuk = mu - fk

      kflag = 1
!
!  Calculation of PX, PY, FX, FY.
!
      if ( ignpoi < 10 ) then

        px = -mu
        py = mu ** ignpoi / fact(ignpoi+1)

      else

        del = 0.8333333E-01 / fk
        del = del - 4.8E+00 * del * del * del
        v = difmuk / fk

        if ( 0.25E+00 < abs ( v ) ) then
          px = fk * log ( 1.0E+00 + v ) - difmuk - del
        else
          px = fk * v * v * ((((((( a7 &
            * v + a6 ) &
            * v + a5 ) &
            * v + a4 ) &
            * v + a3 ) &
            * v + a2 ) &
            * v + a1 ) &
            * v + a0 ) - del
        end if

        py = 0.3989423E+00 / sqrt ( fk )

      end if

      x = ( 0.5E+00 - difmuk ) / s
      xx = x * x
      fx = -0.5E+00 * xx
      fy = omega * ((( c3 * xx + c2 ) * xx + c1 ) * xx + c0 )

      if ( kflag <= 0 ) then

        if ( fy - u * fy <= py * exp ( px - fx ) ) then
          return
        end if

      else

        if ( c * abs ( u ) <= py * exp ( px + e ) - fy * exp ( fx + e ) ) then
          return
        end if

      end if

    end do

  end if

end
