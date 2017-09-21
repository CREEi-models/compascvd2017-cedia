function genf ( dfn, dfd )

!*****************************************************************************80
!
!! GENF generates an F random deviate.
!
!  Discussion:
!
!    This procedure generates a random deviate from the F (variance ratio)
!    distribution with DFN degrees of freedom in the numerator
!    and DFD degrees of freedom in the denominator.
!
!    It directly generates the ratio of chisquare variates
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
!    0.0 < DFN.
!
!    Input, real ( kind = 4 ) DFD, the denominator degrees of freedom.
!    0.0 < DFD.
!
!    Output, real ( kind = 4 ) GENF, a random deviate from the distribution.
!
  implicit none

  real ( kind = 4 ) dfd
  real ( kind = 4 ) dfn
  real ( kind = 4 ) genchi
  real ( kind = 4 ) genf
  real ( kind = 4 ) xden
  real ( kind = 4 ) xnum

  if ( dfn <= 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENF - Fatal error!'
    write ( *, '(a)' ) '  DFN <= 0.0'
    stop 1
  end if

  if ( dfd <= 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENF - Fatal error!'
    write ( *, '(a)' ) '  DFD <= 0.0'
    stop 1
  end if

  xnum = genchi ( dfn ) / dfn
  xden = genchi ( dfd ) / dfd
  genf = xnum / xden

  return
end
