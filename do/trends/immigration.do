
set more off
clear

global opt "replace comma nonames nolabel"

* use file gotten from Stats Can on net immigration rates
import delimited "data/census/migration_2013_2038.csv", delimiter(";") varnames(1) clear

encode province, gen(prov)
drop province
rename prov province
recode province (2=5) (5=2)
label define prov ///
		   1 "Atlantic" ///
		   2 "Quebec" ///
		   3 "Ontario" ///
		   4 "Prairie" ///
                   5 "Colombie-Britanique" , modify
rename group_age agegroup
expand 2 if agegroup=="15-44", gen(new)
replace agegroup = "30-44" if new==1
drop if agegroup=="15-44" | agegroup=="0-14"
drop new
*encode group_age, gen(agegroup)
*drop group_age
*label drop agegroup
*recode agegroup (1=0) (2=15) (3=45) (4=65)

local j = 4
forvalues i = 2013(1)2038 {
rename v`j' y`i'
local j = `j'+1
replace y`i'=y`i'/2 if agegroup=="30-44"
}

forvalues i = 2010(1)2012{
gen y`i' = y2013
}

forvalues i = 2039(1)2051{
gen y`i' = y2038
}
encode sexe, gen(sex)
recode sex (2=0)
label drop sex
drop sexe
sort sex province agegroup

order province sex agegroup y2010-y2012 y2013-y2051

forvalues p = 1/5 {
	forvalues s = 0/1 {
		preserve
			keep if sex==`s'&province==`p'
			drop sex province
			*keep year qx*
			xpose,clear varname
			drop if _n==1
			rename _varname year
			rename v1 age30
			rename v2 age45
			rename v3 age65
			replace year = subinstr(year,"y","",.)
			destring year,replace
			order year
			sort year

			egen group_year = cut(year), at(2010(2)2052)
			drop year
			rename group_year year
			collapse (sum) age30 age45 age65, by(year)
			outsheet using "params/trends/trend-immigration-`p'-`s'.csv", $opt
		restore
	}
}

exit
