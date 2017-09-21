function snorm ( )

!*****************************************************************************80
!
!! SNORM samples the standard normal distribution.
!
!  Discussion:
!
!    This procedure corresponds to algorithm FL, with M = 5, in the reference.
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
!    Output, real ( kind = 4 ) SNORM, a random deviate from the distribution.
!
  implicit none

  real ( kind = 4 ) a(32)
  real ( kind = 4 ) aa
  real ( kind = 4 ) d(31)
  real ( kind = 4 ) h(31)
  integer ( kind = 4 ) i
  real ( kind = 4 ) random_number_f
  real ( kind = 4 ) s
  real ( kind = 4 ) snorm
  real ( kind = 4 ) t(31)
  real ( kind = 4 ) tt
  real ( kind = 4 ) u
  real ( kind = 4 ) ustar
  real ( kind = 4 ) w
  real ( kind = 4 ) y

  save a
  save d
  save h
  save t

  data a / &
        0.0000000E+00, 0.3917609E-01, 0.7841241E-01, 0.1177699E+00, &
        0.1573107E+00, 0.1970991E+00, 0.2372021E+00, 0.2776904E+00, &
        0.3186394E+00, 0.3601299E+00, 0.4022501E+00, 0.4450965E+00, &
        0.4887764E+00, 0.5334097E+00, 0.5791322E+00, 0.6260990E+00, &
        0.6744898E+00, 0.7245144E+00, 0.7764218E+00, 0.8305109E+00, &
        0.8871466E+00, 0.9467818E+00, 1.009990E+00,  1.077516E+00, &
        1.150349E+00,  1.229859E+00,  1.318011E+00,  1.417797E+00, &
        1.534121E+00,  1.675940E+00,  1.862732E+00,  2.153875E+00 /

  data d / &
        0.0000000E+00, 0.0000000E+00, 0.0000000E+00, 0.0000000E+00, &
        0.0000000E+00, 0.2636843E+00, 0.2425085E+00, 0.2255674E+00, &
        0.2116342E+00, 0.1999243E+00, 0.1899108E+00, 0.1812252E+00, &
        0.1736014E+00, 0.1668419E+00, 0.1607967E+00, 0.1553497E+00, &
        0.1504094E+00, 0.1459026E+00, 0.1417700E+00, 0.1379632E+00, &
        0.1344418E+00, 0.1311722E+00, 0.1281260E+00, 0.1252791E+00, &
        0.1226109E+00, 0.1201036E+00, 0.1177417E+00, 0.1155119E+00, &
        0.1134023E+00, 0.1114027E+00, 0.1095039E+00 /

  data h / &
        0.3920617E-01, 0.3932705E-01, 0.3950999E-01, 0.3975703E-01, &
        0.4007093E-01, 0.4045533E-01, 0.4091481E-01, 0.4145507E-01, &
        0.4208311E-01, 0.4280748E-01, 0.4363863E-01, 0.4458932E-01, &
        0.4567523E-01, 0.4691571E-01, 0.4833487E-01, 0.4996298E-01, &
        0.5183859E-01, 0.5401138E-01, 0.5654656E-01, 0.5953130E-01, &
        0.6308489E-01, 0.6737503E-01, 0.7264544E-01, 0.7926471E-01, &
        0.8781922E-01, 0.9930398E-01, 0.1155599E+00, 0.1404344E+00, &
        0.1836142E+00, 0.2790016E+00, 0.7010474E+00 /

  data t / &
        0.7673828E-03, 0.2306870E-02, 0.3860618E-02, 0.5438454E-02, &
        0.7050699E-02, 0.8708396E-02, 0.1042357E-01, 0.1220953E-01, &
        0.1408125E-01, 0.1605579E-01, 0.1815290E-01, 0.2039573E-01, &
        0.2281177E-01, 0.2543407E-01, 0.2830296E-01, 0.3146822E-01, &
        0.3499233E-01, 0.3895483E-01, 0.4345878E-01, 0.4864035E-01, &
        0.5468334E-01, 0.6184222E-01, 0.7047983E-01, 0.8113195E-01, &
        0.9462444E-01, 0.1123001E+00, 0.1364980E+00, 0.1716886E+00, &
        0.2276241E+00, 0.3304980E+00, 0.5847031E+00 /

  u = random_number_f ( )
  if ( u <= 0.5E+00 ) then
    s = 0.0E+00
  else
    s = 1.0E+00
  end if
  u = 2.0E+00 * u - s
  u = 32.0E+00 * u
  i = int ( u )
  if ( i == 32 ) then
    i = 31
  end if
!
!  Center
!
  if ( i /= 0 ) then

    ustar = u - real ( i )
    aa = a(i)

    do

      if ( t(i) < ustar ) then

        w = ( ustar - t(i) ) * h(i)

        y = aa + w

        if ( s /= 1.0E+00 ) then
          snorm = y
        else
          snorm = -y
        end if

        return

      end if

      u = random_number_f ( )
      w = u * ( a(i+1) - aa )
      tt = ( 0.5E+00 * w + aa ) * w

      do

        if ( tt < ustar ) then
          y = aa + w
          if ( s /= 1.0E+00 ) then
            snorm = y
          else
            snorm = -y
          end if
          return
        end if

        u = random_number_f ( )

        if ( ustar < u ) then
          exit
        end if

        tt = u
        ustar = random_number_f ( )

      end do

      ustar = random_number_f ( )

    end do
!
!  Tail
!
  else

    i = 6
    aa = a(32)

    do

      u = u + u

      if ( 1.0E+00 <= u ) then
        exit
      end if

      aa = aa + d(i)
      i = i + 1

    end do

    u = u - 1.0E+00
    w = u * d(i)
    tt = ( 0.5E+00 * w + aa ) * w

    do

      ustar = random_number_f ( )

      if ( tt < ustar ) then
        y = aa + w
        if ( s /= 1.0E+00 ) then
          snorm = y
        else
          snorm = -y
        end if
        return
      end if

      u = random_number_f ( )

      if ( u <= ustar ) then
        tt = u
      else
        u = random_number_f ( )
        w = u * d(i)
        tt = ( 0.5E+00 * w + aa ) * w
      end if

    end do

  end if

end
