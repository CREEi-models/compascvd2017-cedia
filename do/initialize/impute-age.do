* get within age group distribution
preserve
use data/census/census-year-age-sex-province.dta, clear
keep if year==2010
drop if age<30
egen ageg = cut(age), at(30(5)80 101)
egen totpop = total(pop), by(province sex ageg)
gen ageprob = pop/totpop
sort province sex ageg age
by province sex ageg: gen cumageprob = sum(ageprob)
egen id = group(province sex ageg)
sort id age
by id: gen t = _n
keep province sex ageg id t cumageprob
reshape wide cumageprob, i(id) j(t)
keep province sex ageg cumageprob*
sort province sex ageg
save data/census/withinagegroupdist.dta, replace
restore

* load clean-cchs and impute age within age group
preserve 
use temp/clean-cchs.dta, clear
merge n:1 province sex ageg using data/census/withinagegroupdist.dta
drop if _merge==2
drop _merge
gen u = runiform()
gen age = .
gen cumageprob0 = 0
forvalues j = 1/21 {
    local j1 = `j'-1
    replace age = ageg + (`j'-1) if u<=cumageprob`j'&u>cumageprob`j1'&age==.
}

tab age province [fw=wgt]
label var age "age imputed within group census distribution"
drop u cumageprob*

gen byear = 2010 - age
label var byear "birth year"

save temp/clean-cchs.dta, replace
restore
