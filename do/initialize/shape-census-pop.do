* file to clean-up census population tables

import delimited "data/census/census-pop-2010.csv", delimiter(";")  clear
drop if age == "All ages"
drop if sex=="Both sexes"
drop vector coordinate
rename value pop
rename ref_date year
rename geo province
gen female = sex=="Females"
tab female
drop sex
rename female sex
label def sex 0 "male" 1 "female"
label values sex sex
label var sex "gender (female = 1)"
label var pop "population"
replace age= "100 years" if age=="100 years and over"
replace age = "1 years" if age=="1 year"
gen s = strlen(age)
drop if s>9
drop s
gen str agex = word(age,1)
drop age
rename agex age
destring age, replace
label var age "age"
label var year "year"

gen prov = 1 if inlist(province,"Newfoundland and Labrador","Nova Scotia","New Brunswick","Prince Edward Island")
replace prov = 2 if inlist(province,"Quebec")
replace prov = 3 if inlist(province,"Ontario")
replace prov = 4 if inlist(province,"Manitoba","Saskatchewan","Alberta")
replace prov = 5 if inlist(province,"British Columbia")

label def prov 1 "atlantic" 2 "quebec" 3 "ontario" 4 "prairies" 5 "british columbia"
label val prov prov
drop province
rename prov province
label var province "province"
drop if pop==.
drop if province==.

preserve
collapse (sum) pop, by(year province age sex)
save data/census/census-year-age-sex-province.dta, replace
restore


preserve
collapse (sum) pop, by(year province age)
save data/census/census-year-age-province.dta, replace
restore

preserve
collapse (sum) pop, by(year age)
save data/census/census-year-age.dta, replace
restore
