*COMPAS output profile
cd "`3'"
capture log close
log using log_figure, replace

global actual = "`1'"
global master = "`2'"
*Population

use "../params/trends/trend-pop-under30.dta", clear
collapse (sum) popunder30, by(year)
save "../params/trends/trend-pop-under30-canada.dta", replace
	use "./$actual/pop_sum_age_value.dta", clear
	rename pop pop_$actual
	merge 1:1 year age using "./$master/pop_sum_age_value.dta"
	rename pop pop_$master
	egen age5 = cut(age), at(30,50,70,90,111)
	collapse (sum) pop_$actual pop_$master, by(year age5)
	keep if year < 2052
	merge m:1 year using "../params/trends/trend-pop-under30-canada.dta"
	replace popunder30=popunder30/1e6

foreach run in $actual $master {
	*Population

	replace pop_`run'=pop_`run'/1e6
	sort year age5
	egen age90_`run'=total(pop_`run'), by(year)
	replace age90_`run'=age90_`run'+popunder30

	egen age70_`run'=total(pop_`run') if age5<90, by(year)
	replace age70_`run'=age70_`run'+popunder30

	egen age50_`run'=total(pop_`run') if age5<70, by(year)
	replace age50_`run'=age50_`run'+popunder30

	egen age30_`run'=total(pop_`run') if age5<50, by(year)
	replace age30_`run'=age30_`run'+popunder30


	twoway area age90_`run' age70_`run' age50_`run' age30_`run' popunder30 year, ///
	color(emidblue  bluishgray gs8 ltblue  dknavy) ylabel(0(5)45) ///
	xlabel(2010(5)2050)  ///
	xtitle("Ann�e") ytitle("Population (en millions)") ///
	legend(order(1 "Plus de 90 ans" 2 "70 � 90 ans" 3 "50 � 70 ans" ///
	4 "30 � 50 ans" 5 "Moins de 30 ans") symxsize(5.0)) graphregion(color(white)) title("`run'")   name("pop_`run'", replace)
}
graph combine pop_$actual pop_$master, title("Population") ycommon

graph export "./rapport/pop.eps", as(eps) replace
/*
gen age90 = age90_a - age90_m
gen age70 = age70_a - age70_m
gen age50 = age50_a - age50_m
gen age30 = age30_a - age30_m


twoway area age90 age70 age50 age30 year, ///
color(emidblue  maroon red ltblue forest_green)  ///
xlabel(2010(5)2050)  ///
xtitle("Ann�e") ytitle("Population (en millions)") ///
legend(order(1 "Plus de 90 ans" 2 "70 � 90 ans" 3 "50 � 70 ans" ///
4 "30 � 50 ans" 5 "Moins de 30 ans") symxsize(5.0)) graphregion(color(white)) title("Diff pop")   name("diff_pop", replace)
*/
*EDUCATION
global variable "lesshs hs college university"
local first = 1
foreach run in $actual $master {
	foreach var of global variable {
		if `first'==1 {
		use "./`run'/`var'_mean_year_value.dta", clear
		local first = 2
		rename `var' `var'_a
		rename `var'_sd `var'_sd_a
		continue
		}
		merge 1:1 year using "./`run'/`var'_mean_year_value.dta"
		drop _merge
		if "`run'" == "$actual" {
			rename `var' `var'_a
			rename `var'_sd `var'_sd_a

		}
		else {
			rename `var' `var'_m
			rename `var'_sd `var'_sd_m
		}
	}
}

keep if year<=2050
twoway (line lesshs_a year, lp(solid) lc(navy)) (line lesshs_m year, lp(dash) lc(navy)) ///
 (line hs_a year, lp(solid) lc(maroon))  (line hs_m year, lp(dash) lc(maroon)) ///
 (line college_a year, lp(solid) lc(forest_green)) (line college_m year, lp(dash) lc(forest_green)) ///
 (line university_a year, lp(solid) lc(orange_red))(line university_m year, lp(dash) lc(orange_red)), ///
xlabel(2010(5)2050) graphregion(color(white)) title("Education") ///
legend(order(1 "Sans dipl�me - $actual" 2 "Sans dipl�me - $master" 3 "DES - $actual" 4 "DES - $master " ///
5 "Post-secondaire - $actual" 6 "Post-secondaire - $master" 7 "Universitaire - $actual" 8 "Universitaire - $master") size(vsmall))
graph export "./rapport/educ.eps", as(eps) replace


*Disease
global name "Diabete Hypertension Hearte Stroke Cancer Lunge Mental"
global variable "diabe hibpe hearte stroke cancre lunge mentae nvrsmoke former smoker nonobese obese veryobese"
local first = 1
foreach run in $actual $master {
	foreach var of global variable {
		if `first'==1 {
		use "./`run'/`var'_mean_year_value.dta", clear
		local first = 2
		rename `var' `var'_a
		rename `var'_sd `var'_sd_a
		continue
		}
		merge 1:1 year using "./`run'/`var'_mean_year_value.dta"
		drop _merge
		if "`run'" == "$actual" {
			rename `var' `var'_a
			rename `var'_sd `var'_sd_a

		}
		else {
			rename `var' `var'_m
			rename `var'_sd `var'_sd_m
		}
	}
}
keep if year<=2050
global variable "diabe hibpe hearte stroke cancre lunge mentae"
local n : word count $name
di `n'
forvalues i = 1/`n' {
local var : word `i' of $variable
local name : word `i' of $name
twoway (line `var'_a year, lp(solid) lc(navy)) (line `var'_m year, lp(dash) lc(navy)), ///
xlabel(2010(10)2050) graphregion(color(white)) title(`name') ///
legend(order(1 "New" 2 "Old") symxsize(5.5)) name("`var'", replace)
}

graph combine diabe hibpe hearte stroke,  graphregion(color(white))
graph export "./rapport/disease1.eps", as(eps) replace

graph combine cancre lunge mentae,  graphregion(color(white))
graph export "./rapport/disease2.eps", as(eps) replace

graph combine diabe hibpe hearte stroke cancre lunge mentae,cols(2) graphregion(color(white))
graph export "./rapport/disease3.eps", as(eps) replace

*Smoker

twoway (line nvrsmoke_a year, lp(solid) lc(navy)) (line nvrsmoke_m year, lp(dash) lc(navy)) ///
(line former_a year, lp(solid) lc(maroon)) (line former_m year, lp(dash) lc(maroon)) ///
(line smoker_a year, lp(solid) lc(forest_green)) (line smoker_m year, lp(dash) lc(forest_green)), ///
xlabel(2010(10)2050) graphregion(color(white)) title("Smoker") ///
legend(order(1 "Nvr. smoke - $actual" 2 "Nvr smoke - $master" 3 "Former smoker - $actual" 4 "Former Smoker - $master" 5 "Smoker - $actual" 6 "Smoker - $master") ///
symxsize(5.5))  name("smoker", replace)

graph export "./rapport/smoker.eps", as(eps) replace

*BMI

twoway (line nonobese_a year, lp(solid) lc(navy)) (line nonobese_m year, lp(dash) lc(navy)) ///
(line obese_a year, lp(solid) lc(maroon)) (line obese_m year, lp(dash) lc(maroon)) ///
(line veryobese_a year, lp(solid) lc(forest_green)) (line veryobese_m year, lp(dash) lc(forest_green)), ///
xlabel(2010(10)2050) graphregion(color(white)) title("Obese") ///
legend(order(1 "Non Obese - $actual" 2 "Non Obese - $master" 3 "Obese - $actual" 4 "Obese - $master" 5 "Very Obese - $actual" 6 "Very Obese - $master") ///
symxsize(5.5))  name("obese", replace)

graph export "./rapport/obese.eps", as(eps) replace

*Disability

*By year
global variable "adl iadl cognitive"
local first = 1
foreach run in $actual $master {
	foreach var of global variable {
		if `first'==1 {
		use "./`run'/`var'_mean_year_value.dta", clear
		di "Pop"
		local first = 2
		rename `var' `var'_a
		rename `var'_sd `var'_sd_a
		continue
		}
		merge 1:1 year using "./`run'/`var'_mean_year_value.dta"
		drop _merge
		if "`run'" == "$actual" {
			rename `var' `var'_a
			rename `var'_sd `var'_sd_a

		}
		else {
			rename `var' `var'_m
			rename `var'_sd `var'_sd_m
		}
	}
}
keep if year<=2050

twoway (line adl_a year, lp(solid) lc(navy)) (line adl_m year, lp(dash) lc(navy)) ///
(line iadl_a year, lp(solid) lc(maroon)) (line iadl_m year, lp(dash) lc(maroon)) ///
(line cognitive_a year, lp(solid) lc(forest_green)) (line cognitive_m year, lp(dash) lc(forest_green)), ///
xlabel(2010(10)2050) graphregion(color(white)) title("Disability") ///
legend(order(1 "ADL - $actual" 2 "ADL - $master" 3 "IADL - $actual" 4 "IADL - $master" 5 "Cog. - $actual" 6 "Cog. - $master") ///
symxsize(5.5))  name("disability", replace)

graph export "./rapport/disability.eps", as(eps) replace

/*
*by age

global variable "adl iadl cognitive"
local first = 1
foreach run in $actual $master {
	foreach var of global variable {
		if `first'==1 {
		use "./`run'/`var'_mean_age_value.dta", clear
		di "Pop"
		local first = 2
		rename `var' `var'_a
		rename `var'_sd `var'_sd_a
		continue
		}
		merge 1:1 year age using "./`run'/`var'_mean_age_value.dta"
		drop _merge
		if "`run'" == "$actual" {
			rename `var' `var'_a
			rename `var'_sd `var'_sd_a

		}
		else {
			rename `var' `var'_m
			rename `var'_sd `var'_sd_m
		}
	}
}
keep if year<=2050
keep if age < 100
twoway (line adl_a age if year ==2030, lp(solid) lc(navy)) (line adl_m age if year ==2030, lp(dash) lc(navy)) ///
(line iadl_a age if year ==2030, lp(solid) lc(maroon)) (line iadl_m age if year ==2030, lp(dash) lc(maroon)) ///
(line cognitive_a age if year ==2030, lp(solid) lc(forest_green)) (line cognitive_m age if year ==2030, lp(dash) lc(forest_green)) , ///
xlabel(30(10)100) graphregion(color(white)) ///
legend(order(1 "ADL 2030 - $actual" 2 "ADL 2030 - $master" 3 "IADL 2030 - $actual" 4 "IADL 2030 - $master" 5 "Cog. 2030 - $actual" 6 "Cog. 2030 - $master" ) ///
 size(small) symxsize(5.5)) name("disability2030", replace)


twoway (line adl_a age if year ==2050, lp(solid) lc(navy)) (line adl_m age if year ==2050, lp(dash) lc(navy)) ///
(line iadl_a age if year ==2050, lp(solid) lc(maroon)) (line iadl_m age if year ==2050, lp(dash) lc(maroon)) ///
(line cognitive_a age if year ==2050, lp(solid) lc(forest_green)) (line cognitive_m age if year ==2050, lp(dash) lc(forest_green)), ///
xlabel(30(10)100) graphregion(color(white)) ///
legend(order(1 "ADL 2050 - $actual" 2 "ADL 2050 - $master" 3 "IADL 2050 - $actual" 4 "IADL 2050 - $master" 5 "Cog. 2050 - $actual" 6 "Cog. 2050 - $master") ///
size(small) symxsize(5.5)) name("disability2050", replace)


graph combine disability2030 disability2050, title("Disability") ycommon graphregion(color(white))

graph export "./rapport/disability-age.eps", as(eps) replace
*/

*Life expectancy
*ydisease ydisable
global variable "ydead"
local first = 1
foreach run in $actual $master {
	foreach var of global variable {
		if `first'==1 {
		use "./`run'/`var'_mean_age_value.dta", clear
		local first = 2
		rename `var' mx2_`var'_`run'
		drop `var'_sd
		continue
		}
		merge 1:1 year age using "./`run'/`var'_mean_age_value.dta"
		rename `var' mx2_`var'_`run'
		drop _merge `var'_sd
	}
}
preserve
merge 1:1 year age using ./$master/pop_sum_age_value.dta
drop if year > 2052

egen age5 = cut(age), at(30,50,65,75,85,111)
recast long pop, force
collapse (mean) mx2_ydead_$actual mx2_ydead_$master [fw=pop], by(year age5)


twoway (line mx2_ydead_$actual year if age==30, lw(medthick) lc(navy)) ///
 (line mx2_ydead_$master year if age==30, lw(medthick) lp(dash) lc(navy)) ///
(line mx2_ydead_$actual year if age==50, lw(medthick) lc(maroon)) ///
(line mx2_ydead_$master year if age==50, lw(medthick) lp(dash) lc(maroon)) ///
(line mx2_ydead_$actual year if age==65, lw(medthick) lc(forest_green)) ///
(line mx2_ydead_$master year if age==65, lw(medthick) lc(forest_green) lp(dash)) ///
(line mx2_ydead_$actual year if age==75, lw(medthick) lc(orange_red)) ///
(line mx2_ydead_$master year if age==75, lw(medthick) lc(orange_red) lp(dash)) ///
,  xlabel(2010(10)2050, angle(-60)) ytitle("Mortality rate") xtitle("Year") ///
title(`run') graphregion(color(white)) legend(order(1 "30-50 - $actual" 2 "30-50 $mastet" ///
3 "50-65 - $actual" 4 "50-65 - $master" 5 "65-75 - $actual" 6 "65-75 -$master" ///
7 "75-85 - $actual" 8 "75-85 -$master")) name("ydead", replace)

graph export "./rapport/mortalityrate.eps", as(eps) replace


restore
foreach run in $actual $master {
	foreach var of global variable {
		capture drop pop
		capture drop mx Lx temp_Tx temp_Tx2 Tx

		gen pop = 100000
		gen mx =1-((1-mx2_`var'_`run')^.5)
		replace mx = 2*mx/(2+mx)
		bysort   year (age): replace pop = pop[_n-1]*(1-mx[_n-1]) if pop[_n-1]!=.
		by  year: gen Lx = (pop+pop[_n+1])/2
		by  year: gen temp_Tx = sum(Lx)
		by  year: egen temp_Tx2 = total(Lx)
		by year: gen Tx = temp_Tx2-temp_Tx[_n-1] if temp_Tx[_n-1]!=.
		by  year: replace Tx = temp_Tx2 if  temp_Tx[_n-1]==.
		gen eh_`var'_`run' = Tx/pop
	}
}
keep if year>=2010 & year<=2050

global yaxis_label  `" "Life expectancy" "Life expectancy without disease" "Life expectancy without disability" "'
local n : word count $variable
foreach run in $actual $master {
	forvalues i = 1/`n'{
		local var : word `i' of $variable
		local var_label : word `i' of $yaxis_label
		twoway (line eh_`var'_`run' year if age==45, lw(medthick)) ///
		(line eh_`var'_`run' year if age==55, lw(medthick)) ///
		(line eh_`var'_`run' year if age==65, lw(medthick)) ,  xlabel(2010(10)2050, angle(-60)) ///
		ytitle("`var_label'") xtitle("Ann�e")  graphregion(color(white)) ylabel(0(10)50) title(`run') ///
		legend(order(1 "At 45 years" 2 "At 55 years" 3 "At 65 years")) name("`var'_`run'", replace)
	}
}


graph combine ydead_$actual ydead_$master , title("Life expectancy") ycommon
graph export "./rapport/eh.eps", as(eps) replace
*graph combine ydisable_$actual ydisable_$master , title("Life expectancy without disease") ycommon
*graph export "./rapport/ehdisease.eps", as(eps) replace
*graph combine ydisease_$actual ydisease_$master , title("Life expectancy without disability") ycommon
*graph export "./rapport/ehdisability.eps", as(eps) replace

*Outcome - Canada
global variable "cost_nights cost_specialist cost_generalist cost_drugs cost_homecare_f"
foreach run in $actual $master {
	foreach var of global variable {
		use "./`run'/`var'_mean_province_value.dta",clear
		merge 1:1 province year using ./`run'/pop_sum_province_value.dta
		recast long pop, force
		collapse (mean) `var' [fw=pop], by(year)
		label var `var'
		save ./`run'/`var'_mean_year_value.dta,replace
	}
}
#delimit ;
global variable "hc_nights hc_specialist hc_generalist hc_hui hc_drugs hc_homecare_f hc_homecare_i hc_homecare_sp hours_homecare_i hours_homecare_sp
                 cost_nights cost_specialist cost_generalist cost_drugs cost_homecare_f nhome";
global name `" "Nbr nights" "Specialist visit" "Generalist visits" "HUI" "Cons. Drugs" "Prop. homecare Formal" "Prop. homecare Informal." "Prop. Homecare spouse."
				"hours homecare informal." "hours homecare spouse." "cost nights" "cost specialist" "cost generalist" "cost drug" "cost homecare form." "Pop Nursing home" "';
#delimit cr
local first = 1
foreach run in $actual $master {
	foreach var of global variable {
		if `first'==1 {
		use "./`run'/`var'_mean_year_value.dta", clear
		local first = 2
		rename `var' `var'_a
		capture drop `var'_sd
		continue
		}
		merge 1:1 year using "./`run'/`var'_mean_year_value.dta"
		drop _merge
		if "`run'" == "$actual" {
			rename `var' `var'_a
			capture drop `var'_sd

		}
		else {
			rename `var' `var'_m
			capture drop `var'_sd
		}
	}
}
keep if year<=2050
local n : word count $name
di
forvalues i = 1/`n' {
local var : word `i' of $variable
local name : word `i' of $name
twoway (line `var'_a year, lp(solid) lc(navy)) (line `var'_m year, lp(dash) lc(navy)), ///
xlabel(2010(10)2050) graphregion(color(white)) title(`name') ///
legend(order(1 "$actual" 2 "$master") symxsize(5.5) size(small)) name("`var'", replace)
}
graph combine hc_generalist cost_generalist hc_specialist cost_specialist, title("Utilization 1/4") graphregion(color(white)) name("hc1",replace)
graph export "./rapport/utilizationCAN1.eps", as(eps) replace
graph combine hc_nights cost_nights hc_drugs cost_drugs , title("Utilization 2/4") graphregion(color(white)) name("hc2",replace)
graph export "./rapport/utilizationCAN2.eps", as(eps) replace
graph combine  hc_homecare_i hours_homecare_i hc_homecare_sp hours_homecare_sp,title("Utilization 3/4") graphregion(color(white)) name("hc3",replace)
graph export "./rapport/utilizationCAN3.eps", as(eps) replace
graph combine  hc_homecare_f cost_homecare_f hc_hui  nhome  , title("Utilization 4/4") graphregion(color(white)) name("hc4",replace)
graph export "./rapport/utilizationCAN4.eps", as(eps) replace




log close log_figure
