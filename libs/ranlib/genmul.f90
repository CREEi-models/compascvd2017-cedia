subroutine genmul ( n, p, ncat, ix )

!*****************************************************************************80
!
!! GENMUL generates a multinomial random deviate.
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
!    Luc Devroye,
!    Non-Uniform Random Variate Generation,
!    Springer, 1986,
!    ISBN: 0387963057,
!    LC: QA274.D48.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of events, which will be
!    classified into one of the NCAT categories.
!
!    Input, real ( kind = 4 ) P(NCAT-1).  P(I) is the probability that an event
!    will be classified into category I.  Thus, each P(I) must be between
!    0.0 and 1.0.  Only the first NCAT-1 values of P must be defined since
!    P(NCAT) would be 1.0 minus the sum of the first NCAT-1 P's.
!
!    Input, integer ( kind = 4 ) NCAT, the number of categories.
!
!    Output, integer ( kind = 4 ) IX(NCAT), a random observation from
!    the multinomial distribution.  All IX(i) will be nonnegative and their
!    sum will be N.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ncat

  integer ( kind = 4 ) i
  integer ( kind = 4 ) icat
  integer ( kind = 4 ) ignbin
  integer ( kind = 4 ) ix(ncat)
  integer ( kind = 4 ) ntot
  real ( kind = 4 ) p(ncat-1)
  real ( kind = 4 ) prob
  real ( kind = 4 ) ptot

  if ( n < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENMUL - Fatal error!'
    write ( *, '(a)' ) '  N < 0'
    stop 1
  end if

  if ( ncat <= 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENMUL - Fatal error!'
    write ( *, '(a)' ) '  NCAT <= 1'
    stop 1
  end if

  do i = 1, ncat - 1

    if ( p(i) < 0.0E+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GENMUL - Fatal error!'
      write ( *, '(a)' ) '  Some P(i) < 0.'
      stop 1
    end if

    if ( 1.0E+00 < p(i) ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GENMUL - Fatal error!'
      write ( *, '(a)' ) '  Some 1 < P(i).'
      stop 1
    end if

  end do

  ptot = 0.0E+00
  do i = 1, ncat - 1
    ptot = ptot + p(i)
  end do

  if ( 0.99999E+00 < ptot ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENMUL - Fatal error!'
    write ( *, '(a)' ) '  1 < Sum of P().'
    stop 1
  end if
!
!  Initialize variables.
!
  ntot = n
  ptot = 1.0E+00
  do i = 1, ncat
    ix(i) = 0
  end do
!
!  Generate the observation.
!
  do icat = 1, ncat - 1
    prob = p(icat) / ptot
    ix(icat) = ignbin ( ntot, prob )
    ntot = ntot - ix(icat)
    if ( ntot <= 0 ) then
      return
    end if
    ptot = ptot - p(icat)
  end do

  ix(ncat) = ntot

  return
end
