**************************************************************
* Prepare .csv file for population under 30 trend 2010-2050  *
**************************************************************
clear all
use "data/census/trend_age_pop_2010_2038.dta",replace
global opt "replace comma nonames nolabel"

*Keep only province, group of age and remove sexe "Tous" and "Etablissement"
keep if prov == 14 | prov == 24 | prov == 35 | prov == 49 | prov == 59
keep if agegroup <30
collapse (sum) y2010-y2038, by(prov)

*Recode province and group of province from east to west in 1 to 5
recode prov (14=1) (24=2) (35=3) (49=4) (59=5)

*Use or not use label
label drop prov
*label define prov 1 "Atlantic" 2 "Qu?bec" 3 "Ontario" 4 "Prairie" 5 "Colombie-Britanique" , modify

preserve
xpose,clear varname
drop if _n==1
rename _varname year
rename v1 atlantique
rename v2 quebec
rename v3 ontario
rename v4 prairie
rename v5 bc
replace year = subinstr(year,"y","",.)
destring year,replace
order year
set obs 41
local j = 1
forvalue i=2039(1)2050 {
replace year = `i' if _n>28+`j'
local j = `j'+1
}
replace atlantique = atlantique[29] if _n>29
replace quebec = quebec[29] if _n>29
replace ontario = ontario[29] if _n>29
replace prairie = prairie[29] if _n>29
replace bc = bc[29] if _n>29
keep if !mod(year,2)

* save in case want to use in model
outsheet using "params/trends/trend-pop-under30.csv", $opt

rename atlantique p1
rename quebec p2
rename ontario p3
rename prairie p4
rename bc p5

reshape long p, i(year) j(province)
rename p popunder30
order year province popunder30
list

save params/trends/trend-pop-under30.dta, replace


restore
