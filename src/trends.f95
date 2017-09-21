! COMPAS: for trends 
double precision, allocatable :: mortalitytrend(:,:,:,:)
double precision, allocatable :: pop30trend(:,:,:)
double precision, allocatable :: immtrend(:,:,:,:)

integer nage_imm

type trend_cost
	double precision nights
	double precision specialist
	double precision generalist
	double precision homecare
	double precision drugs
	double precision nhome
end type trend_cost

type trend_newcohorts
	double precision educ(4)
	double precision bmi(3)
	double precision smoke(3)
end type trend_newcohorts

type (trend_newcohorts), allocatable :: trendnew(:)

type (trend_cost), allocatable :: trendhcare(:)
