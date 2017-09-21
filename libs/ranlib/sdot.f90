function sdot ( n, sx, incx, sy, incy )

!*****************************************************************************80
!
!! SDOT forms the dot product of two vectors.
!
!  Discussion:
!
!    This routine uses single precision real ( kind = 4 ) arithmetic.
!
!    This routine uses unrolled loops for increments equal to one.
!
!  Modified:
!
!    07 July 2007
!
!  Author:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for FORTRAN usage,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, pages 308-323, 1979.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vectors.
!
!    Input, real ( kind = 4 ) X(*), one of the vectors to be multiplied.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
!    Input, real ( kind = 4 ) Y(*), one of the vectors to be multiplied.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive
!    elements of Y.
!
!    Output, real ( kind = 4 ) SDOT, the dot product of X and Y.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 4 ) sdot
  real ( kind = 4 ) stemp
  real ( kind = 4 ) sx(*)
  real ( kind = 4 ) sy(*)

  sdot = 0.0E+00

  if ( n <= 0 ) then
    return
  end if

  stemp = 0.0E+00
!
!  Code for unequal increments or equal increments not equal to 1.
!
  if ( incx /= 1 .or. incy /= 1 ) then

    if ( incx < 0 ) then
      ix = ( - n + 1 ) * incx + 1
    else
      ix = 1
    end if

    if ( incy < 0 ) then
      iy = ( - n + 1 ) * incy + 1
    else
      iy = 1
    end if

    do i = 1, n
      stemp = stemp + sx(ix) * sy(iy)
      ix = ix + incx
      iy = iy + incy
    end do
!
!  Code for both increments equal to 1.
!
  else

    m = mod ( n, 5 )

    do i = 1, m
      stemp = stemp + sx(i) * sy(i)
    end do

    do i = m + 1, n, 5
      stemp = stemp &
       + sx(i)     * sy(i) &
       + sx(i + 1) * sy(i + 1) &
       + sx(i + 2) * sy(i + 2) &
       + sx(i + 3) * sy(i + 3) &
       + sx(i + 4) * sy(i + 4)
    end do

  end if

  sdot = stemp

  return
end
