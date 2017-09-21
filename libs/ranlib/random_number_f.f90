function random_number_f()

!*****************************************************************************80
!
!! random_number_f()
!
!  Discussion:
!
! A simple function wrapper for random_number_f intrinsic subroutine
!
! Author : Francois Laliberte-Auger for Chaire de recherche Industrielle Alliance
!		   sur les enjeux économiques des changements démographiques
!
  implicit none

  real ( kind = 4 ) random_number_f
  call random_number(random_number_f)
  if (random_number_f==0) then
  	random_number_f = tiny(random_number_f)
  end if
  return
end
