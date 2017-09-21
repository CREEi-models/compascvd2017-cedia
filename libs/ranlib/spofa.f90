subroutine spofa ( a, lda, n, info )

!*****************************************************************************80
!
!! SPOFA factors a real symmetric positive definite matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Cleve Moler
!
!  Parameters:
!
!    Input/output, real ( kind = 4 ) A(LDA,N).  On input, the symmetric matrix
!    to be factored.  Only the diagonal and upper triangle are accessed.
!    On output, the strict lower triangle has not been changed.  The diagonal
!    and upper triangle contain an upper triangular matrix R such that
!    A = R' * R.  If INFO is nonzero, the factorization was not completed.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of the array A.
!    N <= LDA.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Output, integer ( kind = 4 ) INFO, error flag.
!    0, no error was detected.
!    K, the leading minor of order K is not positive definite.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) n

  real ( kind = 4 ) a(lda,n)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jm1
  integer ( kind = 4 ) k
  real ( kind = 4 ) s
  real ( kind = 4 ) sdot
  real ( kind = 4 ) t

  info = 0

  do j = 1, n
    info = j
    s = 0.0E+00
    jm1 = j - 1
    do k = 1, jm1
      t = a(k,j) - sdot ( k-1, a(1,k), 1, a(1,j), 1 )
      t = t / a(k,k)
      a(k,j) = t
      s = s + t * t
    end do
    s = a(j,j) - s
    if ( s <= 0.0E+00 ) then
      info = j
      return
    end if
    a(j,j) = sqrt ( s )
  end do

  return
end
