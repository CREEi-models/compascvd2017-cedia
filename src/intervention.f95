! defines an intervention as a derived type

type intervention
    character*20 disease
    logical prevention 
    double precision prevention_prob 
    logical cure 
    double precision cure_prob
end type intervention

type (intervention), allocatable :: interventions(:)
integer ninterventions