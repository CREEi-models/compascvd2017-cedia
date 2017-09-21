function sexpo ( )

!*****************************************************************************80
!
!! SEXPO samples the standard exponential distribution.
!
!  Discussion:
!
!   This procedure corresponds to algorithm SA in the reference.
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
!    Output, real ( kind = 4 ) SEXPO, a random deviate from the standard
!    exponential distribution.
!
  implicit none

  real ( kind = 4 ) a
  integer ( kind = 4 ) i
  real ( kind = 4 ) q(8)
  real ( kind = 4 ) random_number_f
  real ( kind = 4 ) sexpo
  real ( kind = 4 ) u
  real ( kind = 4 ) umin
  real ( kind = 4 ) ustar

  save q

  data q / &
       0.6931472E+00, &
       0.9333737E+00, &
       0.9888778E+00, &
       0.9984959E+00, &
       0.9998293E+00, &
       0.9999833E+00, &
       0.9999986E+00, &
       0.9999999E+00 /

  a = 0.0E+00
  u = random_number_f ( )

  do

    u = u + u

    if ( 1.0E+00 < u ) then
      exit
    end if

    a = a + q(1)

  end do

  u = u - 1.0E+00

  if ( u <= q(1) ) then
    sexpo = a + u
    return
  end if

  i = 1
  ustar = random_number_f ( )
  umin = ustar

  do

    ustar = random_number_f ( )
    umin = min ( umin, ustar )
    i = i + 1

    if ( u <= q(i) ) then
      exit
    end if

  end do

  sexpo = a + umin * q(1)

  return
end
