subroutine prcomp ( maxobs, p, mean, xcovar, answer )

!*****************************************************************************80
!
!! PRCOMP prints covariance information.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2013
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
!    Input, real ( kind = 4 ) MEAN(P), the mean for each column.
!
!    Input, real ( kind = 4 ) XCOVAR(P,P), the variance/covariance matrix.
!
!    Input, real ( kind = 4 ) ANSWER(MAXOBS,P), the observed values.
!
  implicit none

  integer ( kind = 4 ) p
  integer ( kind = 4 ) maxobs

  real ( kind = 4 ) answer(maxobs,p)
  real ( kind = 4 ) dum1
  real ( kind = 4 ) dum2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 4 ) mean(p)
  real ( kind = 4 ) r4vec_covar
  real ( kind = 4 ) rcovar(p,p)
  real ( kind = 4 ) rmean(p)
  real ( kind = 4 ) rvar(p)
  real ( kind = 4 ) xcovar(p,p)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PRCOMP:'
  write ( *, '(a)' ) '  Print and compare covariance information'
  write ( *, '(a)' ) ' '

  do j = 1, p
    call stats ( answer(1,j), maxobs, rmean(j), rvar(j), &
      dum1, dum2 )
    write ( *, '(a,i4)' ) '  Variable Number ', j
    write ( *, '(a,g14.6,a,g14.6)' ) &
      '  Mean ', mean(j), ' Generated ', rmean(j)
    write ( *, '(a,g14.6,a,g14.6)' ) &
      '  Variance ', xcovar(j,j), ' Generated ', rvar(j)
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Covariances:'
  write ( *, '(a)' ) ' '

  do i = 1, p
    do j = 1, i - 1
      write ( *, '(a,i4,a,i4)' ) '  I = ', i, ' J = ', j
      rcovar(i,j) = r4vec_covar ( maxobs, answer(1,i), answer(1,j) )
      write ( *, '(a,g14.6,a,g14.6)' ) &
        '  Covariance ', xcovar(i,j), ' Generated ', rcovar(i,j)
    end do
  end do

  return
end
