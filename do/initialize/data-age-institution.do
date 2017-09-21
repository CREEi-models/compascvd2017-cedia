**************************************************
* Prepare data for calibrate-age-institution.do  *
**************************************************
clear all
use "data/census/census-pop-institution.dta",replace
*NH population
gen popnh = nursing_chronic_ltc + inst_physical + inst_psychiatric

*Keep only province, group of age and remove sexe "Tous" and "Etablissement"
keep if prov == 14 | prov == 24 | prov == 35 | prov == 49 | prov == 59
keep if group_age >=30 & group_age<=85
drop if trim(sex)=="Tous" | trim(sex)=="Etablissement"

*Recode province and group of province from east to west in 1 to 5
recode prov (14=1) (24=2) (35=3) (49=4) (59=5)

encode sexe, gen(sex)
recode sex (2=0)

*Use or not use label
label drop prov
*label define prov 1 "Atlantic" 2 "Qu�bec" 3 "Ontario" 4 "Prairie" 5 "Colombie-Britanique" , modify
label drop sex
*label define sex 0 "Male" 1 "Female", modify
label drop group_age

*Rename variables
rename group_age ageg5
rename prov province

*Keep only useful variables, sort and order variable
keep ageg5 province sex popnh
sort province sex ageg5
order ageg5 province sex popnh

save "data/census/census-pop-nh.dta",replace
