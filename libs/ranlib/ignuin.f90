function ignuin ( low, high )

!*****************************************************************************80
!
!! IGNUIN generates a random integer in a given range.
!
!  Discussion:
!
!    Each deviate K satisfies LOW <= K <= HIGH.
!
!    If (HIGH-LOW) > 2,147,483,561, this procedure prints an error message
!    and stops the program.
!
!    IGNLGI generates integer ( kind = 4 )s between 1 and 2147483562.
!
!    MAXNUM is 1 less than the maximum generatable value.
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
!    Input, integer ( kind = 4 ) LOW, HIGH, the lower and upper bounds.
!
!    Output, integer ( kind = 4 ) IGNUIN, a random deviate from
!    the distribution.
!
  implicit none

  integer ( kind = 4 ) err
  integer ( kind = 4 ) high
  integer ( kind = 4 ) i4_uni
  integer ( kind = 4 ) ign
  integer ( kind = 4 ) ignuin
  integer ( kind = 4 ) low
  integer ( kind = 4 ) maxnow
  integer ( kind = 4 ) maxnum
  parameter ( maxnum = 2147483561 )
  integer ( kind = 4 ) ranp1
  integer ( kind = 4 ) width

  if ( high < low ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'IGNUIN - Fatal error!'
    write ( *, '(a)' ) '  HIGH < LOW.'
    stop 1
  end if

  width = high - low

  if ( maxnum < width ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'IGNUIN - Fatal error!'
    write ( *, '(a)' ) '  Range HIGH-LOW is too large.'
    stop 1
  end if

  if ( low == high ) then
    ignuin = low
    return
  end if

  ranp1 = width + 1
  maxnow = ( maxnum / ranp1 ) * ranp1

  do

    ign = i4_uni ( ) - 1

    if ( ign <= maxnow ) then
      exit
    end if

  end do

  ignuin = low + mod ( ign, ranp1 )

  return
end
