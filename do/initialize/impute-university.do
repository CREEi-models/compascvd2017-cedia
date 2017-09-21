* imputation of university diploma among those with post-secondary education
preserve
use temp/clean-cchs.dta, clear
sum
gen ageg_tmp = ageg
recode ageg (70/max=70)
merge n:1 ageg sex province using temp/prob-university.dta
drop if _merge==2
drop _merge
gen u = runiform()
replace educ4 = 3 + (u<puniv) if educ4==3
replace ageg=ageg_tmp
tab province educ4 [fw=wgt], row
drop u puniv ageg_tmp
save temp/clean-cchs.dta, replace
restore


