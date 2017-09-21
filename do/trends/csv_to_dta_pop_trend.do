clear all
cd ~/compas

import delimited "data/census/projection_2013_2038_eng.csv", delimiter(";") bindquote(strict) varnames(1) stripquote(yes)
local j = 4
forvalues i = 2013(1)2038 {
rename v`j' y`i'
replace y`i' = y`i'*1000
recast long y`i', force
local j = `j'+1
}
replace agegroup = subinstr(agegroup," years","",.)
replace agegroup = subinstr(agegroup," year","",.)
replace agegroup = subinstr(agegroup,"Under 1","0",.)
replace agegroup = subinstr(agegroup,"100 and over","100",.)
destring agegroup, replace
encode sex, gen(sexe)
recode sexe (2=0)
label drop sexe
drop sex

encode geography2, gen(prov)
drop geography2
recode prov (1=48) (2=59) (3=46) (4=13) (5=10) (6=12) (7=35)  (8=11)  (9=24) (10=47)   
label define prov ///
		   10  "Terre-Neuve-et-Labrador" ///
           11 "Ile-du-Prince-edouard" ///
		   12 "Nouvelle-ecosse" ///
           13 "Nouveau-Brunswick" ///
		   14 "Atlantic" ///
		   24 "Quebec" ///
		   35 "Ontario" ///
		   46 "Manitoba" ///
		   47 "Saskatchewan" ///
		   48 "Alberta" ///
		   49 "Prairie" ///
           59 "Colombie-Britanique" , modify

order prov sexe		   

tempfile temp1 
tempfile temp2            

save "`temp1'", replace

clear

import delimited "data/census/pop_2010_2013_sc_eng.csv", delimiter(";") bindquote(strict) varnames(1) stripquote(yes)
rename agegroup56 agegroup
local j = 4
forvalues i = 2010(1)2013 {
rename v`j' y`i'
local j = `j'+1
}
replace agegroup = subinstr(agegroup," years","",.)
replace agegroup = subinstr(agegroup," year","",.)
replace agegroup = subinstr(agegroup,"100 and over","100",.)
destring agegroup, replace

encode sex, gen(sexe)
recode sexe (2=0)
label drop sexe
drop sex

encode geography, gen(prov)
drop geography
recode prov (1=48) (2=59) (3=46) (4=13) (5=10) (6=12) (7=35)  (8=11)  (9=24) (10=47)   
label define prov ///
		   10  "Terre-Neuve-et-Labrador" ///
           11 "Ile-du-Prince-edouard" ///
		   12 "Nouvelle-ecosse" ///
           13 "Nouveau-Brunswick" ///
		   14 "Atlantic" ///
		   24 "Quebec" ///
		   35 "Ontario" ///
		   46 "Manitoba" ///
		   47 "Saskatchewan" ///
		   48 "Alberta" ///
		   49 "Prairie" ///
           59 "Colombie-Britanique" , modify

order prov sexe	
drop y2013
save "`temp2'", replace
use "`temp1'"
merge 1:1 prov sexe agegroup using "`temp2'"
drop _merge

order prov sexe	agegroup y2010-y2012 y2013-y2038

preserve
keep if prov>=10 & prov <=13
collapse (sum) y2010-y2038 , by(sexe agegroup)
forvalues i = 2010(1)2038 {
recast long y`i', force
}
gen prov = 14
save "`temp1'", replace
restore 

        
preserve
keep if prov>=46 & prov <=48
collapse (sum) y2010-y2038 , by(sexe agegroup)
forvalues i = 2010(1)2038 {
recast long y`i', force
}
gen prov = 49
save "`temp2'", replace
restore 


append using "`temp1'"
append using "`temp2'"

save "data/census/trend_age_pop_2010_2038.dta",replace
