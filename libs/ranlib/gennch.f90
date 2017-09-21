function gennch ( df, xnonc )

!*****************************************************************************80
!
!! GENNCH generates a noncentral Chi-Square random deviate.
!
!  Discussion:
!
!    This procedure generates a random deviate from the  distribution of a
!    noncentral chisquare with DF degrees of freedom and noncentrality parameter
!    XNONC.
!
!    It uses the fact that the noncentral chisquare is the sum of a chisquare
!    deviate with DF-1 degrees of freedom plus the square of a normal
!    deviate with mean XNONC and standard deviation 1.
!
!    A subtle ambiguity arises in the original formulation:
!
!      gennch = genchi ( arg1 ) + ( gennor ( arg2, arg3 ) ) ^ 2
!
!    because the compiler is free to invoke either genchi or gennor
!    first, both of which alter the random number generator state,
!    resulting in two distinct possible results.
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
!    1.0 < DF.
!
!    Input, real ( kind = 4 ) XNONC, the noncentrality parameter.
!    0.0 <= XNONC.
!
!    Output, real ( kind = 4 ) GENNCH, a random deviate from the distribution.
!
  implicit none

  real ( kind = 4 ) arg1
  real ( kind = 4 ) arg2
  real ( kind = 4 ) arg3
  real ( kind = 4 ) df
  real ( kind = 4 ) genchi
  real ( kind = 4 ) gennch
  real ( kind = 4 ) gennor
  real ( kind = 4 ) t1
  real ( kind = 4 ) t2
  real ( kind = 4 ) xnonc

  if ( df <= 1.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENNCH - Fatal error!'
    write ( *, '(a)' ) '  DF <= 1.'
    stop 1
  end if

  if ( xnonc < 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENNCH - Fatal error!'
    write ( *, '(a)' ) '  XNONC < 0.0.'
    stop 1
  end if

  arg1 = df - 1.0E+00
  arg2 = sqrt ( xnonc )
  arg3 = 1.0E+00

  t1 = genchi ( arg1 )
  t2 = gennor ( arg2, arg3 )

  gennch = t1 + t2 * t2

  return
end
