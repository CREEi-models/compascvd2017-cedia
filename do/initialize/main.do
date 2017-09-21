* main do-file for initialization of model

capture log close
clear all
set more off
set seed 1234

* main log of model
log using logs/initialize.txt, replace text

* call do file to clean-up cchs
do do/initialize/clean-cchs-2010.do

do do/initialize/clean-cchs-08-09

*do do/initialize/clean-cchs-2012

do do/initialize/append-cchs.do
do do/initialize/calibrate-age-education.do
do do/initialize/model-university.do
global startpop = ""
forvalues i = 1/100 {
* imputing age (CENSUS)
use temp/clean-cchs-first-calib.dta, clear
save  temp/clean-cchs.dta, replace
do do/initialize/impute-age.do

* imputing age (CENSUS)
do do/initialize/impute-homecare.do

* imputing education (LFS)

do do/initialize/impute-university.do

* imputing institutions (NHPS)
do do/initialize/impute-institution.do

* imputing mental illness (NHPS)
do do/initialize/impute-mental.do

* calibrate on age and nursing home status
do do/initialize/calibrate-age-institutions.do

* send to csv format
use data/input/startpop.dta, clear
*drop adl
drop if id==.

order id age byear educ4 sex imm province diabe hibpe hearte stroke cancre lunge mentae smoke bmi inv income ltc wgt
compress
save data/input/startpop`i'.dta, replace
if `i'!=1 {
global startpop = "$startpop data/input/startpop`i'"
}
preserve
	gen nobs = _N
	keep nobs
	keep if _n==1
    #d ;
	outsheet using data/input/sizeofstartpop`i'.csv,
	    delimiter(",")  nonames nolabel replace;
	restore;

	outsheet using data/input/startpop`i'.csv,
	    delimiter(",")  nonames nolabel replace;
	#d cr
}

use data/input/startpop1, clear
append using $startpop, gen(popnumber)
compress
preserve
keep if popnumber<50
save data/input/startpop-all1.dta,replace
restore
keep if popnumber>=50
save data/input/startpop-all2.dta,replace




log close
