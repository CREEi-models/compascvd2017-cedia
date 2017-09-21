
STATAMOD 0.20.3. Copyright (c) 2006-2010 Andrew Shephard
========================================================

STATAMOD is a Fortran module that provides read/write access for Stata datasets.
It has only been tested with Intel Fortran and makes use of IFPORT. The code is
standard Fortran 2003, but will also work with Fortran 95 compilers that support
allocatable fields of derived types and real BOZ literals.

STATA READ SUPPORT
==================

* subroutine openStata(filename,unit [,cache])

This opens the Stata .dta file "filename", using the file unit number "unit". A
dataset must be open before any other (reading) STATAMOD commands can be used.
By default, the entire dataset is loaded into memory (cache=.true.). If you 
specify openStata(filename,unit,cache=.false.) then it will not do this. Instead,
it will just load the variables as requested from disk. As variables are stored
non-sequentially in the Stata dta format, this can be significantly slower than
memory access.

* subroutine descStata()

This describes the open dataset, printing output to the standard output. It is
much like Stata's own describe command.

* integer function nobsStata()

Returns an integer equal to the number of observations in the open dataset.

* integer function nobsStata()

Returns an integer equal to the number of variables in the open dataset.

* logical function existStata(varname)

Returns .true. or .false. depending on whether the variable exists in the open
dataset.

* subroutine readStata(readStataVar,varname)

Copies data from the Stata variable varname in the open dataset to readStataVar.
If nobsStata exceeds the dimension of readStataVar a warning is displayed.
Otherwise the data is copied to first nobsStata elements of readStataVar

* subroutine closeOpenStata()

Closes the open Stata dataset. Call this procedure after all the relevant data
has been read. This will close the file connected to the specified unit or free
memory used to temporarily store the dataset depending upon whether disk or
memory (default) mode was specified in the openStata() subroutine.


STATA READ SUPPORT EXAMPLE
==========================

use statamod
    
implicit none

real*8,  dimension(:),   allocatable :: hhincome
real*8,  dimension(:,:), allocatable :: hours
integer, parameter                   :: funit = 10

call openStata('statafile.dta',funit)
call descStata()

allocate(hhincome(nobsStata()))
allocate(hours(nobsStata(),2))

call readStata(hhincome,'income')
call readStata(hours(:,1),'hours_male')
call readStata(hours(:,2),'hours_female')

call closeOpenStata()


STATA WRITE SUPPORT
===================

* subroutine saveStata(fileName,thisUnit,obs [,label])
            
This opens the Stata .dta file filename for saving, using the specified unit
number thisunit. You must specify the number of obs to save (saving is on a
variable by variable basis), and you can optionaly specify a label for the
dataset. It will save the dataset in the Stata 7 SE file format.

* subroutine writeStata(thisVar, thisName [,thisLabel])

Saves the Fortran variable thisVar with the Stata name thisName. You can 
optionally label the variable with thisLabel. You should call writeStata for 
every variable you wish to save.

* subroutine closeSaveStata()

closes the open saveStata dataset. The file will only be written when this
routine is called.


FURTHER STATAMOD INFORMATION
============================

If you receive a stack overflow error when using the subroutine readStata()
increase the stack size in your compiler options. This may happen if the dataset
contains many observations (it does not depend upon how many variable there are
in the dataset).

stataMod should be able to read Stata dta files for versions 5 to 11. I have not
been able to test data created with Stata version 5 or 6 and neither do I have a
detailed description of the file format implemented in these versions. Please
contact me if such datasets produces any unexpected results


SOFTWARE LICENSE
================

STATAMOD is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

STATAMOD is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with STATAMOD.  If not, see <http://www.gnu.org/licenses/>.

