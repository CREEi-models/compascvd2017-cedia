type statistics
    character*80 outname
    character*80 cmd
    character*80 varname
    character*80 byvarname,byvarname2
    integer gap,gap2
    logical wgt
    integer nbyvar,nbyvar2
    integer minbyvar,minbyvar2
    integer maxbyvar,maxbyvar2
    double precision, allocatable :: value(:,:,:)
    double precision, allocatable :: sd(:,:,:)
end type statistics

integer nstats
type (statistics), allocatable :: stats(:)
