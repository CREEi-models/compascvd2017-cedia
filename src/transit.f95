
type params
	integer npar
    character*40, allocatable :: var(:)
    double precision, allocatable :: val(:)
end type params

type model
	character*80 name
	integer spec
	integer neq
	integer nsex
	type (params), allocatable:: param(:)
	double precision dispersion
end type model

type prob
	character*80 disease
	double precision, allocatable :: value(:)
end type prob

type (model), allocatable :: transit_models(:)

type restriction
    character*20 disease
    integer eq
    character*20 var
    double precision val
end type restriction

integer nrestrictions
type (restriction), allocatable :: restrictions(:)

integer ntransit

double precision, allocatable :: calibmortality(:,:)

