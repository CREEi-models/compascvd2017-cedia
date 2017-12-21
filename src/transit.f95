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
