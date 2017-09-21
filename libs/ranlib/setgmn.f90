subroutine setgmn ( meanv, covm, p, parm )

!*****************************************************************************80
!
!! SETGMN sets data for the generation of multivariate normal deviates.
!
!  Discussion:
!
!    This procedure places P, MEANV, and the Cholesky factorization of
!    COVM in GENMN.
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
!    Input, real ( kind = 4 ) MEANV(P), the means of the multivariate
!    normal distribution.
!
!    Input/output, real ( kind = 4 ) COVM(P,P).  On input, the covariance
!    matrix of the multivariate distribution.  On output, the information
!    in COVM has been overwritten.
!
!    Input, integer ( kind = 4 ) P, the number of dimensions.
!
!    Output, real ( kind = 4 ) PARM(P*(P+3)/2+1), parameters needed to generate
!    multivariate normal deviates.
!
  implicit none

  integer ( kind = 4 ) p

  real ( kind = 4 ) covm(p,p)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icount
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  real ( kind = 4 ) meanv(p)
  real ( kind = 4 ) parm(p*(p+3)/2+1)

  if ( p <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SETGMN - Fatal error!'
    write ( *, '(a)' ) '  P was not positive.'
    stop 1
  end if
!
!  Store P.
!
  parm(1) = p
!
!  Store MEANV.
!
  do i = 2, p + 1
    parm(i) = meanv(i-1)
  end do
!
!  Compute the Cholesky decomposition.
!
  call spofa ( covm, p, p, info )

  if ( info /= 0) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SETGMN - Fatal error!'
    write ( *, '(a)' ) '  SPOFA finds COVM not positive definite.'
    stop 1
  end if
!
!  Store the upper half of the Cholesky factor.
!
  icount = p + 1

  do i = 1, p
    do j = i, p
      icount = icount + 1
      parm(icount) = covm(i,j)
    end do
  end do

  return
end
