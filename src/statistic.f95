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
