* main code to produce statistics from a scenario
clear all
capture log close
set more off
global scenario = "reference"
global rep = "50" 

* read data from ascii file and transform to stata
cd ~/compas/do/stats/
!./readpop.sh $scenario $rep
!cp ~/compas/output/$scenario/simpop-1.dta ~/compas/output/$scenario/simpop.dta 
cd ~/compas
* creates base pop statistics
   do do/stats/popstats.do
* creates statistics by cohorts
   do do/stats/cohortstats.do
* insert additional code here
use output/$scenario/simpop-1.dta


if $rep > 2 {
global appended = ""
	forvalues i=2/$rep {
		global appended = "$appended output/$scenario/simpop-`i'.dta"
}
append using $appended,gen(replication)
}
replace wgt = round(wgt/$rep)
save output/$scenario/simpop-all.dta, replace


* erase traces of files

forvalues i=1/$rep {
erase output/$scenario/simpop-`i'.dta
}

