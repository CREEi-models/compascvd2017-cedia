! header for new cohorts
! sizes
integer ncohortsize, ncohortmodels
! new cohort model (similar to transit models, but with thresholds
type newcohortmodel
	character*80 label
	integer  spec
	integer  nvalues
	integer  npar
	type (params) param
	type (params) thres
end type newcohortmodel
! set of models
type (newcohortmodel), allocatable :: newcohort_models(:)
! choleski matrix for error terms
double precision, allocatable :: newcohort_choleski(:,:)
! baseline new cohort (from which new cohorts are generated
type (person), allocatable :: newpop(:)
double precision, allocatable :: calibeduc(:,:,:)
