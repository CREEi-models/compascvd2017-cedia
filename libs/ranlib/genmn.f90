subroutine genmn ( parm, x, work )

!*****************************************************************************80
!
!! GENMN generates a multivariate normal deviate.
!
!  Discussion:
!
!    The method is:
!    1) Generate P independent standard normal deviates - Ei ~ N(0,1)
!    2) Using Cholesky decomposition find A so that A'*A = COVM
!    3) A' * E + MEANV ~ N(MEANV,COVM)
!
!    Note that PARM contains information needed to generate the
!    deviates, and is set up by SETGMN.
!
!    PARM(1) contains the size of the deviates, P
!    PARM(2:P+1) contains the mean vector.
!    PARM(P+2:P*(P+3)/2+1) contains the upper half of the Cholesky
!    decomposition of the covariance matrix.
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
!  Parameters:
!
!    Input, real ( kind = 4 ) PARM(P*(P+3)/2+1), parameters set by SETGMN.
!
!    Output, real ( kind = 4 ) X(P), a random deviate from the distribution.
!
!    Workspace, real ( kind = 4 ) WORK(P).
!
  implicit none

  real ( kind = 4 ) ae
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icount
  integer ( kind = 4 ) j
  integer ( kind = 4 ) p
  real ( kind = 4 ) parm(*)
  real ( kind = 4 ) snorm
  real ( kind = 4 ) work(*)
  real ( kind = 4 ) x(*)

  p = int ( parm(1) )
!
!  Generate P independent normal deviates.
!
  do i = 1, p
    work(i) = snorm ( )
  end do
!
!  Compute X = MEANV + A' * WORK
!
  do i = 1, p
    icount = 0
    ae = 0.0E+00
    do j = 1, i
      icount = icount + j - 1
      ae = ae + parm(i+(j-1)*p-icount+p+1) * work(j)
    end do

    x(i) = ae + parm(i+1)

  end do

  return
end
