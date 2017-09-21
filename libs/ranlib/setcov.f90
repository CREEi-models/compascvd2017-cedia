subroutine setcov ( p, var, corr, covar )

!*****************************************************************************80
!
!! SETCOV sets a covariance matrix from variance and common correlation.
!
!  Discussion:
!
!    This procedure sets the covariance matrix from the variance and
!    common correlation.
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
!    Input, integer ( kind = 4 ) P, the number of variables.
!
!    Input, real ( kind = 4 ) VAR(P), the variances.
!
!    Input, real ( kind = 4 ) CORR, the common correlaton.
!
!    Output, real ( kind = 4 ) COVAR(P,P), the covariance matrix.
!
  implicit none

  integer ( kind = 4 ) p

  real ( kind = 4 ) corr
  real ( kind = 4 ) covar(p,p)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 4 ) var(p)

  do i = 1, p
    do  j = 1, p
      if ( i == j ) then
        covar(i,j) = var(i)
      else
        covar(i,j) = corr * sqrt ( var(i) * var(j) )
      end if
    end do
  end do

  return
end
