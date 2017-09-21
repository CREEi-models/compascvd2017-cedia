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
	#d;
	insheet year tcam_nights tcam_specialist tcam_generalist tcam_drugs
		tcam_homecare tcam_nhome using data/other/hcare_costs.csv, clear;
	#d cr
	save temp/tcam_hcare.dta, replace
restore

* create trend
preserve
set obs $nyears
gen temp = $startyr + $gapyrs*(_n-1)
egen year = cut(temp), at(2010(10)2040 2060)
merge n:1 year using temp/tcam_hcare.dta
drop _merge
rename year year10
rename temp year
foreach n in "nights" "specialist" "generalist" "drugs" "homecare" "nhome" {
	qui gen trend_`n' = 1 if _n==1
	forvalues i = 2/$nyears {
		qui replace trend_`n' = ((1 + tcam_`n')^$gapyrs) * trend_`n'[`i'-1] if _n==`i'
	}
}

keep year trend_nights trend_specialist trend_generalist trend_drugs trend_homecare trend_nhome
list year trend_nights trend_specialist trend_generalist trend_drugs trend_homecare trend_nhome

outsheet using params/trends/trend_hcare.csv, $opt

restore
