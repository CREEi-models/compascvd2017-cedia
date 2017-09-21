* create smoking trend

clear
global gapyrs = 2
global startyr = 2010
global stopyr  = 2050
global nyears  = ($stopyr - $startyr)/$gapyrs + 1
global opt "replace comma nonames nolabel"

* get average growth rates from input files
preserve
	insheet year tcam_nonsmoker tcam_smoker tcam_former using params/trends/tcam_smoking.csv, clear
	save temp/tcam_smoking.dta, replace
restore

* create trend
preserve
set obs $nyears
gen temp = $startyr + $gapyrs*(_n-1)
egen year = cut(temp), at(2010(10)2040 2060)
merge n:1 year using temp/tcam_smoking.dta
drop _merge
rename year year10
rename temp year
foreach n in "nonsmoker" "smoker" "former" {
	qui gen trend_`n' = 1 if _n==1
	forvalues i = 2/$nyears {
		qui replace trend_`n' = ((1 + tcam_`n')^$gapyrs) * trend_`n'[`i'-1] if _n==`i'
	}
}

* save trend to file
keep year trend_nonsmoker trend_smoker trend_former
list year trend_nonsmoker trend_smoker trend_former

outsheet using params/trends/trend_smoke.csv, $opt

restore
