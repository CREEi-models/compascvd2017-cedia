function lennob ( s )

!*****************************************************************************80
!
!! LENNOB counts the length of a string, ignoring trailing blanks.
!
!  Discussion:
!
!    This procedure returns the length of a string up to and including
!    the last non-blank character.
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
!    Input, character * ( * ) S, the string.
!
!    Output, integer ( kind = 4 ) LENNOB, the length of the string to the last
!    nonblank.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) lennob
  character * ( * ) s
  integer ( kind = 4 ) s_max

  s_max = len ( s )

  do i = s_max, 1, -1
    if ( s(i:i) /= ' ' ) then
      lennob = i
      return
    end if
  end do

  lennob = 0

  return
end
