subroutine phrtsd ( phrase, seed1, seed2 )

!*****************************************************************************80
!
!! PHRTST converts a phrase to a pair of random number generator seeds.
!
!  Discussion:
!
!    This procedure uses a character string to generate two seeds for the RGN
!    random number generator.
!
!    Trailing blanks are eliminated before the seeds are generated.
!
!    Generated seed values will fall in the range 1 to 2^30 = 1,073,741,824.
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
!    Input, character * ( * ) PHRASE, a phrase to be used for the
!    random number generation.
!
!    Output, integer ( kind = 4 ) SEED1, SEED2, the two seeds for the
!    random number generator, based on PHRASE.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ichr
  integer ( kind = 4 ) j
  integer ( kind = 4 ) lennob
  integer ( kind = 4 ) lphr
  character * ( * ) phrase
  integer ( kind = 4 ) seed1
  integer ( kind = 4 ) seed2
  integer ( kind = 4 ) shift(0:4)
  character * ( 86 ) table
  parameter ( table = &
    'abcdefghijklmnopqrstuvwxyz'// &
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ'// &
    '0123456789'// &
    '!@#$%^&*()_+[];:''"<>?,./' )
  integer ( kind = 4 ) twop30
  parameter ( twop30 = 1073741824 )
  integer ( kind = 4 ) values(5)

  save shift

  data shift / 1, 64, 4096, 262144, 16777216 /

  seed1 = 1234567890
  seed2 = 123456789

  lphr = lennob ( phrase )

  do i = 1, lphr

    ichr = index ( table, phrase(i:i) )
!
!  If the character does not occur, ICHR is returned as 0.
!
    ichr = mod ( ichr, 64 )

    if ( ichr == 0 ) then
      ichr = 63
    end if

    do j = 1, 5
      values(j) = ichr - j
      if ( values(j) < 1 ) then
        values(j) = values(j) + 63
      end if
    end do

    do j = 1, 5
      seed1 = mod ( seed1 + shift(j-1) * values(j), twop30 )
      seed2 = mod ( seed2 + shift(j-1) * values(6-j), twop30 )
    end do

  end do

  return
end
