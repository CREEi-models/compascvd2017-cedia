! COMPAS: for derived variables
	
type derivedvars
	character*80 label
	double precision value
end type derivedvars

integer nderivedvars
character*20, allocatable :: derivedvarlabels(:)


