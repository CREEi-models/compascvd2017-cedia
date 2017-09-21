* create bmi trend
set more off
clear

global gapyrs = 2
global startyr = 2010
global stopyr  = 2050
global nyears  = ($stopyr - $startyr)/$gapyrs + 1
global opt "replace comma nonames nolabel"

* get average growth rates from input files
preserve
	insheet year tcam_normal tcam_obese tcam_veryobese using params/trends/tcam_bmi.csv, clear
	save temp/tcam_bmi.dta, replace
restore

* create trend
preserve
set obs $nyears
gen temp = $startyr + $gapyrs*(_n-1)
egen year = cut(temp), at(2010(10)2040 2060)
merge n:1 year using temp/tcam_bmi.dta
drop _merge
rename year year10
rename temp year
foreach n in "normal" "obese" "veryobese" {
	qui gen trend_`n' = 1 if _n==1
	forvalues i = 2/$nyears {
		qui replace trend_`n' = ((1 + tcam_`n')^$gapyrs) * trend_`n'[`i'-1] if _n==`i'
	}
}

* save trend to file
keep year trend_normal trend_obese trend_veryobese
list year trend_normal trend_obese trend_veryobese

outsheet using params/trends/trend_bmi.csv, $opt

restore
