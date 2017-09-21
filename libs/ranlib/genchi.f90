function genchi ( df )

!*****************************************************************************80
!
!! GENCHI generates a Chi-Square random deviate.
!
!  Discussion:
!
!    This procedure generates a random deviate from the chi square distribution
!    with DF degrees of freedom random variable.
!
!    The algorithm exploits the relation between chisquare and gamma.
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
!    Input, real ( kind = 4 ) DF, the degrees of freedom.
!    0.0 < DF.
!
!    Output, real ( kind = 4 ) GENCHI, a random deviate from the distribution.
!
  implicit none

  real ( kind = 4 ) arg1
  real ( kind = 4 ) arg2
  real ( kind = 4 ) df
  real ( kind = 4 ) genchi
  real ( kind = 4 ) gengam

  if ( df <= 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENCHI - Fatal error!'
    write ( *, '(a)' ) '  DF <= 0.'
    write ( *, '(a,g14.6)' ) '  Value of DF: ', df
    stop 1
  end if

  arg1 = 1.0E+00
  arg2 = df / 2.0E+00

  genchi = 2.0E+00 * gengam ( arg1, arg2 )

  return
end
