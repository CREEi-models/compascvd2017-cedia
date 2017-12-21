!   COMPAS: A Health Microsimulation Model for Quebec and Canada
!   Copyright (c) 2012-2017, COMPAS Development Team
!
!   This program is free software: you can redistribute it and/or modify
!   it under the terms of the GNU Affero General Public License as published
!   by the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   This program is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU Affero General Public License for more details.
!
!   You should have received a copy of the GNU Affero General Public License
!   along with this program.  If not, see <http://www.gnu.org/licenses/>.
! COMPAS settings list

! inputs from settings files
integer startyear
integer stopyear
integer stopyear_enter
integer gapyears
integer endstats
integer startage
integer stopage
logical doreps
integer nreps
logical newcohort
logical immigration
logical imortality
logical ihcarecost
logical ieducation
logical ismoke
logical ibmi
logical iquebec
logical updhcare
logical exportstata_opt
integer start_byear
integer end_byear
logical educ_intervention
integer ncoef ! number of separate coefficient set
logical decrcvd
logical decrmortality
double precision alpha
! parameters determined once loaded
integer nstartcohort
integer ncycle
integer nyears
integer nyearstats
integer nages
character*80 scenario

! outpath
character*80 outpath, path

!  format for output
character*80 fmt

! seed
integer :: values(1:8), q
integer, dimension(:), allocatable :: seed
integer, dimension(:,:), allocatable :: seed_cmp

! type of econometric models
enum , bind(c)
		enumerator :: probit = 1, oprobit = 2, logit = 3, cloglog = 4, mlogit = 5, poisson = 6, linear = 7, negbin = 8
end enum

! number of separate cases in simulation
integer nsimcases
