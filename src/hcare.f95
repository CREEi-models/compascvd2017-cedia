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
