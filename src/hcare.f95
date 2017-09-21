type hcmodel
	character*80 name
	integer spec
	integer nsex
	integer neq
	type (params), allocatable :: param(:)
	double precision dispersion
end type hcmodel

type (model), allocatable :: hcare_models(:)

integer nhcare
