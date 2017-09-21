function gennf ( dfn, dfd, xnonc )

!*****************************************************************************80
!
!! GENNF generates a noncentral F random deviate.
!
!  Discussion:
!
!    This procedure generates a random deviate from the noncentral F
!    (variance ratio) distribution with DFN degrees of freedom in the
!    numerator, and DFD degrees of freedom in the denominator, and
!    noncentrality parameter XNONC.
!
!    It directly generates the ratio of noncentral numerator chisquare variate
!    to central denominator chisquare variate.
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
!    Input, real ( kind = 4 ) DFN, the numerator degrees of freedom.
!    1.0 < DFN.
!
!    Input, real ( kind = 4 ) DFD, the denominator degrees of freedom.
!    0.0 < DFD.
!
!    Input, real ( kind = 4 ) XNONC, the noncentrality parameter.
!    0.0 <= XNONC.
!
!    Output, real ( kind = 4 ) GENNF, a random deviate from the distribution.
!
  implicit none

  real ( kind = 4 ) dfd
  real ( kind = 4 ) dfn
  real ( kind = 4 ) genchi
  real ( kind = 4 ) gennch
  real ( kind = 4 ) gennf
  real ( kind = 4 ) xden
  real ( kind = 4 ) xnonc
  real ( kind = 4 ) xnum

  if ( dfn <= 1.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENNF - Fatal error!'
    write ( *, '(a)' ) '  DFN <= 1.0'
    stop 1
  end if

  if ( dfd <= 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENNF - Fatal error!'
    write ( *, '(a)' ) '  DFD <= 0.0'
    stop 1
  end if

  if ( xnonc < 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENNF - Fatal error!'
    write ( *, '(a)' ) '  XNONC < 0.0'
    stop 1
  end if

  xnum = gennch ( dfn, xnonc ) / dfn
  xden = genchi ( dfd ) / dfd

  gennf = xnum / xden

  return
end
