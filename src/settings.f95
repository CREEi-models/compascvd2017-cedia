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
