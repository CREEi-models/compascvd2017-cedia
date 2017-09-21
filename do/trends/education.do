* create education trend
clear
set more off

global gapyrs = 2
global startyr = 2010
global stopyr  = 2050
global nyears  = ($stopyr - $startyr)/$gapyrs + 1
global opt "replace comma nonames nolabel"

* get average growth rates from input files
preserve
	insheet year tcam_lesshs tcam_hs tcam_college tcam_univ using params/trends/tcam_education.csv, clear
	save temp/tcam_education.dta, replace
restore

* create trend
preserve
set obs $nyears
gen temp = $startyr + $gapyrs*(_n-1)
egen year = cut(temp), at(2010(10)2040 2060)
merge n:1 year using temp/tcam_education.dta
drop _merge
rename year year10
rename temp year
foreach n in "lesshs" "hs" "college" "univ" {
	qui gen trend_`n' = 1 if _n==1
	forvalues i = 2/$nyears {
		qui replace trend_`n' = ((1 + tcam_`n')^$gapyrs) * trend_`n'[`i'-1] if _n==`i'
	}
}

keep year trend_lesshs trend_hs trend_college trend_univ
list year trend_lesshs trend_hs trend_college trend_univ

outsheet using params/trends/trend_educ.csv, $opt

restore
