*Age ajusted mortality rate

*Test decrease mortality in 2024

global path = "/Users/UQAM/compas_travail/output-scenario"

global scenario "reference decrcvd decrcvd1 decrcvd2 decrcvd3"
 
foreach run of global scenario {
	use "$path/`run'/ydead_cvd_sum_age_value1.dta",clear
	global file = ""
	forvalues i = 2/100 {
	global file = "$file $path/`run'/ydead_cvd_sum_age_value`i'.dta"
	}

	append using $file, gen(coef)
	rename ydead_cvd ydead_`run'
	drop ydead_cvd_sd
	save $path/`run'/ydead_cvd_sum_age_value.dta, replace


	use "$path/`run'/pop_sum_age_value1.dta",clear

	global file = ""
	forvalues i = 2/100 {
	global file = "$file $path/`run'/pop_sum_age_value`i'.dta"
	}

	append using $file, gen(coef)
	rename pop pop_`run'
	drop pop_sd
	save $path/`run'/pop_sum_age_value.dta, replace
}


use "$path/reference/ydead_cvd_sum_age_value.dta",clear
merge m:1 year age coef using "$path/reference/pop_sum_age_value.dta"
drop _merge

foreach run in decrcvd decrcvd decrcvd1 decrcvd2 decrcvd3{
	merge m:1 year age coef using "$path/`run'/pop_sum_age_value.dta"
	drop _merge
	merge 1:1 year age coef cvd using "$path/`run'/ydead_cvd_sum_age_value.dta"
	drop _merge

}

foreach run of global scenario {
	preserve
	keep if year >=2012 & year<=2050
	keep if age <=100
	keep if cvd ==1 
	bysort coef age year coef: gen tx_dead_cvd_`run' = ydead_`run'/pop_`run'
	bysort coef age (year) : gen pop_dead_2012_`run' = tx_dead_cvd_`run'*pop_`run'[1]
	collapse (sum) pop_dead_2012_`run' pop_`run' , by(year coef)
	bysort coef (year): gen adj_mx_`run' = pop_dead_2012_`run'/pop_`run'[1]
	bysort coef (year): gen diff_adj_mx_`run' = (adj_mx_`run'[1]-adj_mx_`run')/adj_mx_`run'[1]
		
	bysort year: egen pct5d_`run' = pctile(diff_adj_mx_`run'), p(5)
	bysort year: egen pct95d_`run' = pctile(diff_adj_mx_`run'), p(95)
	bysort year: egen meand_`run' = mean(diff_adj_mx_`run')
	bysort year : keep if _n==1
	di  "Diff in mortality for `run' : " meand_`run'[7] " ( " pct5d_`run'[7] " ; " pct95d_`run'[7] " ) "
	restore
}

*CVD and mortality

**Cost
global path = "/PATH/OUTPUT/"

global scenario "reference decrcvd decrcvd1 decrcvd2 decrcvd3 decrcvd4 decrcvd5 decrcvd6"
global scenario1 "decrcvd decrcvd1 decrcvd2 decrcvd3 decrcvd4 decrcvd5 decrcvd6"

global variable "cost_nights cost_generalist cost_specialist"
foreach run of global scenario {
	foreach var of global variable {
		use "$path/`run'/`var'_sum_year_value1.dta", clear 
		global file = ""
		forvalues i = 2/100 {
		global file = "$file $path/`run'/`var'_sum_year_value`i'.dta"
		}
		append using $file, gen(coef)
		rename `var' `var'_`run'
		drop `var'_sd
		tempfile `var'_`run'
		save ``var'_`run''
	}
}

	local first = 1

foreach run of global scenario {
	foreach var of global variable {
		if `first'==1 {
			use ``var'_`run'', clear
			local first = 2	
		}	
		else {
			merge 1:1 year coef using ``var'_`run''
			drop _merge
		}
	}
}

gen n = year-2012
local ratio_cost_nights = 1.6
local ratio_cost_specialist = 7.9
local ratio_cost_generalist = 2.4

local discrate = 0.03
foreach run of global scenario1 { 
	foreach var of global variable {
		gen d_`var'_`run' = `var'_reference-`var'_`run'

		gen van_`var'_`run' = d_`var'_`run'*(1+`discrate')^(-n)
		if "`var'"=="cost_nights" {
			replace van_`var'_`run' = van_`var'_`run'*`ratio_cost_nights'
		}
		else if "`var'"=="cost_generalist" {
			replace van_`var'_`run' = van_`var'_`run'*`ratio_cost_generalist'
		}
		else if "`var'"=="cost_specialist" {
				replace van_`var'_`run' = van_`var'_`run'*`ratio_cost_specialist'
		}
		bysort coef (year): gen tvan_`var'_`run' = sum(van_`var'_`run')
		
		bysort coef (year): replace tvan_`var'_`run' = tvan_`var'_`run'*2- tvan_`var'_`run'/(_n*2)
	}
}

save "$path/cost.dta",replace
use "$path/cost.dta",clear


drop if year ==2010 | year==2052 
*increase cost in time in base 100

local ratio_cost_nights = 1.6
local ratio_cost_specialist = 7.9
local ratio_cost_generalist = 2.4

foreach run of global scenario { 
 gen cost_doctors_`run' = cost_specialist_`run'*`ratio_cost_specialist' + cost_generalist_`run'*`ratio_cost_generalist'
}
global variable "cost_nights cost_generalist cost_specialist cost_doctors"


foreach run of global scenario { 
	foreach var of global variable {
		bysort coef (year): gen `var'_`run'_100 = (`var'_`run'[_n]-`var'_`run'[1])/`var'_`run'[1]*100+100
		bysort year: egen pct5_`var'_`run' = pctile(`var'_`run'_100), p(5)
		bysort year: egen mean_`var'_`run' = mean(`var'_`run'_100)
		bysort year: egen pct95_`var'_`run' = pctile(`var'_`run'_100), p(95)
		
	}
}

foreach run of global scenario1 { 
	foreach var of global variable {
		gen d_`var'_`run'_100 = `var'_`run'_100-`var'_reference_100
		bysort year: egen pct5d_`var'_`run' = pctile(d_`var'_`run'_100), p(5)
		bysort year: egen meand_`var'_`run' = mean(d_`var'_`run'_100)
		bysort year: egen pct95d_`var'_`run' = pctile(d_`var'_`run'_100), p(95)
	}
}


foreach var of global variable {
		
			sort year d_`var'_decrcvd3_100
			by year : gen zero_pct_tmp = _n if d_`var'_decrcvd3_100>=0 & d_`var'_decrcvd3_100[_n-1]< 0
			by year : egen pct0_`var'_decrcvd3 = max(zero_pct_tmp)
			drop zero_pct_tmp
	}

bysort year: keep if _n==1
drop *1 *2 *4 *5 *6
global path_exp = "/PATH/TABLE-FIG/"

keep if inlist(year,2020,2030,2040,2050)


gen year_ref = year
gen year_cvd = year
gen year_cvd3 = year

recode year_ref  (2020=1) (2030=5) (2040=9) (2050=13)
recode year_cvd3 (2020=2) (2030=6) (2040=10) (2050=14)
recode year_cvd  (2020=3) (2030=7) (2040=11) (2050=15)
twoway  (bar mean_cost_nights_reference year_ref) ///
		(bar mean_cost_nights_decrcvd3 year_cvd3) /// 
	    (bar mean_cost_nights_decrcvd year_cvd), ///
		legend(row(1) order(1 "Baseline" 2 "Mortality-Based CVD Decrease" 3 "Incidence-Based CVD Decrease") symxsize(5.4) size(small)) ///
		xlabel( 2 "2020" 6 "2030" 10 "2040" 14 "2050", noticks) xtitle("Year") ylabel(100(20)220) ///
		ytitle("Cost (2012C$, 2012=100)" " ") graphregion(color(white)) name(cost_nights,replace)
		graph export "$path_exp/scenario/cost_nights_bar.tiff", as(tif) replace	
		
twoway  (bar mean_cost_doctors_reference year_ref) ///
		(bar mean_cost_doctors_decrcvd3 year_cvd3) /// 
		(bar mean_cost_doctors_decrcvd year_cvd) , ///
		legend(row(1) order(1 "Baseline" 2 "Mortality-Based CVD Decrease" 3 "Incidence-Based CVD Decrease") symxsize(5.4) size(small)) ///
		xlabel( 2 "2020" 6 "2030" 10 "2040" 14 "2050", noticks) xtitle("Year")  ylabel(100(20)220) ///
		ytitle("Cost (2012C$, 2012=100)" " ") graphregion(color(white)) name(cost_doctors,replace)
		graph export "$path_exp/scenario/cost_doctors_bar.tiff", as(tif) replace	


preserve

keep mean*doctors*  pct*doctors* year

restore


keep if inlist(year,2050)
preserve
keep year pct0*
keep pct* mean*


**Desease
global path = "/PATH/OUTPUT/"

global scenario "reference decrcvd decrcvd1 decrcvd2 decrcvd3"
global variable "hearte stroke hibpe mentae cancre diabe"
foreach run of global scenario {
	foreach var of global variable {
		use "$path/`run'/`var'_mean_year_value1.dta", clear 
		global file = ""
		forvalues i = 2/100 {
		global file = "$file $path/`run'/`var'_mean_year_value`i'.dta"
		}
		append using $file, gen(coef)
		rename `var' `var'_`run'
		drop `var'_sd
		tempfile `var'_`run'
		save ``var'_`run''
	}
}

	local first = 1

foreach run of global scenario {
	foreach var of global variable {
		if `first'==1 {
			use ``var'_`run'', clear
			local first = 2	
		}	
		else {
			merge 1:1 year coef using ``var'_`run''
			drop _merge
		}
	}
}
foreach var of global variable {
	bysort year: egen pct5_`var'_reference = pctile(`var'_reference), p(5)
	bysort year: egen pct95_`var'_reference = pctile(`var'_reference), p(95)
	bysort year: egen mean_`var'_reference = mean(`var'_reference)
}	
foreach run in decrcvd decrcvd1 decrcvd2 decrcvd3 { 
	foreach var of global variable {
		local abbrev = abbrev("`run'",4)
		gen d_`var'_`run' = `var'_reference-`var'_`run'

		bysort year: egen pct5d_`var'_`run' = pctile(d_`var'_`run'), p(5)
		bysort year: egen pct95d_`var'_`run' = pctile(d_`var'_`run'), p(95)
		bysort year: egen meand_`var'_`run' = mean(d_`var'_`run')

		bysort year: egen pct5_`var'_`run' = pctile(`var'_`run'), p(5)
		bysort year: egen pct95_`var'_`run' = pctile(`var'_`run'), p(95)
		bysort year: egen mean_`var'_`run' = mean(`var'_`run')
	}
}
bysort year : keep if _n==1

keep if inlist(year,2012,2020,2034,2050)
global path_exp = "/PATH/TABLE-FIG/"
local cell = 3
putexcel set "$path_exp/scenario/diff2.xlsx",  sheet(ref) modify

foreach var of global variable {
	preserve
	di "`var'"
	keep year pct5_`var'_reference mean_`var'_reference pct95_`var'_reference
	order year pct5_`var'_reference mean_`var'_reference pct95_`var'_reference

	replace pct5_`var'_reference = pct5_`var'_reference
	replace mean_`var'_reference = mean_`var'_reference
	replace pct95_`var'_reference = pct95_`var'_reference
		mkmat year pct5_`var'_reference mean_`var'_reference pct95_`var'_reference, matrix(temp)
	putexcel  B`cell' = matrix(temp)
	matrix drop temp
	
	local cell = `cell'+7
	list year pct5_`var'_reference mean_`var'_reference pct95_`var'_reference	
	restore
}

foreach run in decrcvd decrcvd1 decrcvd2 decrcvd3 { 
	putexcel set "$path_exp/scenario/diff2.xlsx",  sheet(`run') modify

	di "`run'" 
	local cell = 3
	foreach var of global variable {
	preserve
	di "`var'" 
	keep year pct5_`var'_`run' mean_`var'_`run' pct95_`var'_`run'
	order year pct5_`var'_`run' mean_`var'_`run' pct95_`var'_`run'
	
	replace pct5_`var'_`run' = pct5_`var'_`run'
	replace mean_`var'_`run' = mean_`var'_`run'
	replace pct95_`var'_`run' = pct95_`var'_`run'
	mkmat year pct5_`var'_`run' mean_`var'_`run' pct95_`var'_`run', matrix(temp)
	putexcel  B`cell' = matrix(temp)
	*export excel using "$path_exp/scenario/diff.xlsx", sheet("`run'")  cell(A`cell') firstrow(variables) sheetmodify
	matrix drop temp
	list year pct5_`var'_`run' mean_`var'_`run' pct95_`var'_`run'
	local cell = `cell'+7

	restore
	}
}

**Mortality

global scenario "reference decrcvd decrcvd1 decrcvd2 decrcvd3"
global variable "ydead"
foreach run of global scenario {
	foreach var of global variable {
		use "$path/`run'/`var'_mean_age_value1.dta", clear 
		global file = ""
		forvalues i = 2/100 {
		global file = "$file $path/`run'/`var'_mean_age_value`i'.dta"
		}
		append using $file, gen(coef)
		rename `var' `var'_`run'
		drop `var'_sd
		tempfile `var'_`run'
		save ``var'_`run''
	}
}

	local first = 1

foreach run of global scenario {
	foreach var of global variable {
		if `first'==1 {
			use ``var'_`run'', clear
			local first = 2	
			rename `var'_`run' mx2_`run'
		}	
		else {
			merge 1:1 year coef age using ``var'_`run''
			rename `var'_`run' mx2_`run'
			drop _merge
		}
	}
}

foreach run of global scenario { 
		capture drop pop
		capture drop mx Lx temp_Tx temp_Tx2 Tx 
		
		gen pop = 100000
		gen mx =1-((1-mx2_`run')^.5)
		replace mx = 2*mx/(2+mx)
		bysort  coef year  (age): replace pop = pop[_n-1]*(1-mx[_n-1]) if pop[_n-1]!=.
		by  coef year : gen Lx = (pop+pop[_n+1])/2	
		by  coef year : gen temp_Tx = sum(Lx)
		by  coef year : egen temp_Tx2 = total(Lx)
		by coef year : gen Tx = temp_Tx2-temp_Tx[_n-1] if temp_Tx[_n-1]!=.
		by  coef year : replace Tx = temp_Tx2 if  temp_Tx[_n-1]==.
		gen eh_`run' = Tx/pop	
}

keep if year >=2012 & year <=2050	
keep if age ==65

bysort year: egen pct5_eh_reference = pctile(eh_reference), p(5)
bysort year: egen pct95_eh_reference = pctile(eh_reference), p(95)
bysort year: egen mean_eh_reference = mean(eh_reference)

foreach run in decrcvd decrcvd1 decrcvd2 decrcvd3 { 
		gen d_eh_`run' = eh_reference-eh_`run'
		sort year d_eh_`run'
		by year : gen zero_pct_tmp = _n if d_eh_`run'<=0 & d_eh_`run'[_n+1]>0
		by year : egen zero_pct_`run' = max(zero_pct_tmp)
		drop zero_pct_tmp
		bysort year: egen pct5d_eh_`run' = pctile(d_eh_`run'), p(5)
		bysort year: egen pct95d_eh_`run' = pctile(d_eh_`run'), p(95)
		bysort year: egen meand_eh_`run' = mean(d_eh_`run')

		bysort year: egen pct5_eh_`run' = pctile(eh_`run'), p(5)
		bysort year: egen pct95_eh_`run' = pctile(eh_`run'), p(95)
		bysort year: egen mean_eh_`run' = mean(eh_`run')
}

sort year (coef)


bysort year : keep if _n==1
keep if year >=2012
drop if year ==2052

keep if inlist(year,2012,2020,2030,2050)

preserve 

keep year zero_pct_decrcvd zero_pct_decrcvd3
list

restore
keep year pct5_eh_reference pct95_eh_reference mean_eh_reference pct5d_eh_decrcvd pct95d_eh_decrcvd meand_eh_decrcvd pct5d_eh_decrcvd1 pct95d_eh_decrcvd1 meand_eh_decrcvd1 pct5d_eh_decrcvd2 pct95d_eh_decrcvd2 meand_eh_decrcvd2 pct5d_eh_decrcvd3 pct95d_eh_decrcvd3 meand_eh_decrcvd3

order year pct5_eh_reference mean_eh_reference pct95_eh_reference pct5d_eh_decrcvd meand_eh_decrcvd pct95d_eh_decrcvd pct5d_eh_decrcvd1 meand_eh_decrcvd1 pct95d_eh_decrcvd1 pct5d_eh_decrcvd2 meand_eh_decrcvd2 pct95d_eh_decrcvd2 pct5d_eh_decrcvd3 meand_eh_decrcvd3 pct95d_eh_decrcvd3 pct5_eh_reference


** VAN **


global path = "/PATH/OUTPUT/"

global scenario "reference decrcvd decrcvd1 decrcvd2 decrcvd3 decrcvd4 decrcvd5 decrcvd6"
global scenario_1 " decrcvd decrcvd1 decrcvd2 decrcvd3 decrcvd4 decrcvd5 decrcvd6 "

foreach run of global scenario {
	use "$path/`run'/pop_sum_age_value1.dta",clear
	global file = ""
	forvalues i = 2/100 {
	global file = "$file $path/`run'/pop_sum_age_value`i'.dta"
	}

	append using $file, gen(coef)
	rename pop pop_`run'
	drop pop_sd
	save $path/`run'/pop_sum_age_value.dta, replace

}


use "$path/reference/pop_sum_age_value.dta",clear

foreach run of global scenario_1 {
	merge 1:1 year age coef using "$path/`run'/pop_sum_age_value.dta"
	drop _merge
}

collapse (sum) pop_decrcvd pop_decrcvd1 pop_decrcvd2 pop_decrcvd3 pop_decrcvd4 pop_decrcvd5 pop_decrcvd6 pop_reference, by(year coef)
drop if year==2010 | year==2052
local discrate = 0.03
local valueyear = 200000
bysort coef (year) : gen n = (_n-1)*2

expand 2
sort coef year
bysort coef year : replace n = n-1+_n
bysort coef year : replace year = year+_n-1


foreach run of global scenario { 
replace pop_`run' = (pop_`run'[_n-1]+pop_`run'[_n+1])/2 if mod(year,2)
}

foreach run of global scenario_1 { 
		gen d_pop_`run' = pop_`run'-pop_reference
		gen value_life_`run' = d_pop_`run'*`valueyear'
		gen discount_value_`run' = value_life_`run' *(1+`discrate')^(-n)
		bysort coef (year): gen tot_discount_`run' = sum(discount_value_`run')
	}

save "$path/lifevalue.dta",replace



keep pct* mean* year
by year : keep if _n==1
keep if inlist(year,2020,2030,2035,2050)

list year mean_discount_value_decrcvd mean_discount_value_decrcvd1 mean_discount_value_decrcvd2 mean_discount_value_decrcvd3

order year pct5_discount_value_decrcvd mean_discount_value_decrcvd pct95_discount_value_decrcvd ///
pct5_discount_value_decrcvd1 mean_discount_value_decrcvd1 pct95_discount_value_decrcvd1 ///
pct5_discount_value_decrcvd2 mean_discount_value_decrcvd2 pct95_discount_value_decrcvd2 ///
pct5_discount_value_decrcvd3 mean_discount_value_decrcvd3 pct95_discount_value_decrcvd3 



***** Graph alpha ****
use "$path/cost.dta",clear
recode year (2034=2035)
merge 1:1 year coef using "$path/lifevalue.dta"
drop if mod(year,2)!=0 & year!=2035
drop if year ==2034

keep if inlist(year,2020,2035,2050)

global path = "/PATH/OUTPUT/"
global path_exp = "/PATH/TABLE-FIG/"

global scenario "reference decrcvd decrcvd1 decrcvd2 decrcvd3 decrcvd4 decrcvd5 decrcvd6"
global scenario_1 " decrcvd decrcvd1 decrcvd2 decrcvd3 decrcvd4 decrcvd5 decrcvd6 "

global cost "cost_nights cost_generalist cost_specialist totcost totvalue"


foreach run of global scenario_1 { 
		
	}
	
foreach run of global scenario1 {

	
	gen tvan_totvalue_`run' = tvan_cost_nights_`run' + tvan_cost_generalist_`run'+ tvan_cost_specialist_`run'+ tot_discount_`run'
	gen tvan_totcost_`run' = tvan_cost_nights_`run' + tvan_cost_generalist_`run'+ tvan_cost_specialist_`run'
}
foreach run of global scenario_1 {
		sort year tot_discount_`run'
		by year : gen zero_pct_tmp = _n if tot_discount_`run'>=0 & tot_discount_`run'[_n-1]< 0
		by year : egen pct0_discount_`run' = max(zero_pct_tmp)
		drop zero_pct_tmp
		bysort year : egen pct5_discount_value_`run' = pctile(tot_discount_`run'), p(5)
		bysort year : egen pct95_discount_value_`run' = pctile(tot_discount_`run'), p(95)
		bysort year : egen mean_discount_value_`run' = mean(tot_discount_`run')
		
		foreach var of global cost {
			sort year tvan_`var'_`run'
			by year : gen zero_pct_tmp = _n if tvan_`var'_`run'>=0 & tvan_`var'_`run'[_n-1]< 0
			by year : egen pct0_`var'_`run' = max(zero_pct_tmp)
			drop zero_pct_tmp
			bysort year: egen pct5v_`var'_`run' = pctile(tvan_`var'_`run'), p(5)
			bysort year: egen pct95v_`var'_`run' = pctile(tvan_`var'_`run'), p(95)
			bysort year: egen meanv_`var'_`run' = mean(tvan_`var'_`run')
			bysort year: egen pct25v_`var'_`run' = pctile(tvan_`var'_`run'), p(25)
			bysort year: egen pct75v_`var'_`run' = pctile(tvan_`var'_`run'), p(55)
		}
}

bysort year : keep if _n ==1
preserve 

keep year  pct0_totvalue_decrcvd pct0_cost_nights_decrcvd  pct0_cost_generalist_decrcvd pct0_cost_specialist_decrcvd pct0_discount_decrcvd ///
		pct0_totvalue_decrcvd3 pct0_cost_nights_decrcvd3  pct0_cost_generalist_decrcvd3 pct0_cost_specialist_decrcvd3 pct0_discount_decrcvd3 
list year pct0_totvalue_decrcvd pct0_cost_nights_decrcvd  pct0_cost_generalist_decrcvd pct0_cost_specialist_decrcvd pct0_discount_decrcvd
list year pct0_totvalue_decrcvd3 pct0_cost_nights_decrcvd3  pct0_cost_generalist_decrcvd3 pct0_cost_specialist_decrcvd3 pct0_discount_decrcvd3 

restore
keep year  pct* mean*

/*** Tableau van */
preserve
keep pct5v_cost_nights_decrcvd  pct5v_cost_generalist_decrcvd  pct5v_cost_specialist_decrcvd  pct5_discount_value_decrcvd pct5v_totcost_decrcvd pct5v_totvalue_decrcvd ///
	 meanv_cost_nights_decrcvd  meanv_cost_generalist_decrcvd  meanv_cost_specialist_decrcvd  mean_discount_value_decrcvd meanv_totcost_decrcvd meanv_totvalue_decrcvd ///
	 pct95v_cost_nights_decrcvd  pct95v_cost_generalist_decrcvd  pct95v_cost_specialist_decrcvd  pct95_discount_value_decrcvd pct95v_totcost_decrcvd pct95v_totvalue_decrcvd ///
	 pct5v_cost_nights_decrcvd3  pct5v_cost_generalist_decrcvd3  pct5v_cost_specialist_decrcvd3  pct5_discount_value_decrcvd3 pct5v_totcost_decrcvd3 pct5v_totvalue_decrcvd3 ///
	 meanv_cost_nights_decrcvd3  meanv_cost_generalist_decrcvd3  meanv_cost_specialist_decrcvd3  mean_discount_value_decrcvd3  meanv_totcost_decrcvd3 meanv_totvalue_decrcvd3 ///
	 pct95v_cost_nights_decrcvd3  pct95v_cost_generalist_decrcvd3  pct95v_cost_specialist_decrcvd3  pct95_discount_value_decrcvd3 pct95v_totcost_decrcvd3 pct95v_totvalue_decrcvd3 ///
	 year
	 
foreach var in	 pct5v_cost_nights_decrcvd  pct5v_cost_generalist_decrcvd  pct5v_cost_specialist_decrcvd  pct5_discount_value_decrcvd pct5v_totcost_decrcvd  pct5v_totvalue_decrcvd ///
	 meanv_cost_nights_decrcvd  meanv_cost_generalist_decrcvd  meanv_cost_specialist_decrcvd  mean_discount_value_decrcvd meanv_totcost_decrcvd meanv_totvalue_decrcvd  ///
	 pct95v_cost_nights_decrcvd  pct95v_cost_generalist_decrcvd  pct95v_cost_specialist_decrcvd  pct95_discount_value_decrcvd pct95v_totcost_decrcvd pct95v_totvalue_decrcvd {
	 
	 rename `var' `var'0
	 
	 }

	 reshape long  pct5v_cost_nights_decrcvd  pct5v_cost_generalist_decrcvd  pct5v_cost_specialist_decrcvd  pct5_discount_value_decrcvd pct5v_totcost_decrcvd pct5v_totvalue_decrcvd ///
	 meanv_cost_nights_decrcvd  meanv_cost_generalist_decrcvd  meanv_cost_specialist_decrcvd  mean_discount_value_decrcvd meanv_totcost_decrcvd meanv_totvalue_decrcvd ///
	 pct95v_cost_nights_decrcvd  pct95v_cost_generalist_decrcvd  pct95v_cost_specialist_decrcvd  pct95_discount_value_decrcvd pct95v_totcost_decrcvd pct95v_totvalue_decrcvd ///
	 , i(year) j(alpha)

sort alpha year

order	pct5v_cost_nights_decrcvd  meanv_cost_nights_decrcvd pct95v_cost_nights_decrcvd ///
		pct5v_cost_generalist_decrcvd meanv_cost_generalist_decrcvd  pct95v_cost_generalist_decrcvd ///
		pct5v_cost_specialist_decrcvd meanv_cost_specialist_decrcvd pct95v_cost_specialist_decrcvd ///
		pct5_discount_value_decrcvd mean_discount_value_decrcvd pct95_discount_value_decrcvd ///
		pct5v_totvalue_decrcvd meanv_totvalue_decrcvd pct95v_totvalue_decrcvd ///
		pct5v_totcost_decrcvd meanv_totcost_decrcvd pct95v_totcost_decrcvd

restore 
	
foreach var in	 pct5v_totvalue_decrcvd pct25v_totvalue_decrcvd  pct75v_totvalue_decrcvd  meanv_totvalue_decrcvd  pct95v_totvalue_decrcvd {
	 
	 rename `var' `var'0
	 
	 }
keep pct5v_totvalue_decrcvd* pct25v_totvalue_decrcvd*  pct75v_totvalue_decrcvd*  meanv_totvalue_decrcvd*  pct95v_totvalue_decrcvd* year
reshape long  pct5v_totvalue_decrcvd pct25v_totvalue_decrcvd  pct75v_totvalue_decrcvd  meanv_totvalue_decrcvd  pct95v_totvalue_decrcvd, i(year) j(alpha)


recode alpha (0=1) (1=0.66) (2=.33)(3=0)(4=0.1667)(5=.5)(6=.833)




replace meanv_totvalue_decrcvd = meanv_totvalue_decrcvd /1e9
replace pct5v_totvalue_decrcvd = pct5v_totvalue_decrcvd /1e9
replace pct95v_totvalue_decrcvd = pct95v_totvalue_decrcvd /1e9
replace pct25v_totvalue_decrcvd =  pct25v_totvalue_decrcvd/1e9
replace  pct75v_totvalue_decrcvd = pct75v_totvalue_decrcvd/1e9
set scheme s1mono

twoway (bar  meanv_totvalue_decrcvd alpha if year==2050,  barwidth(.05) fcolor(gs12)) (rcap pct5v_totvalue_decrcvd pct95v_totvalue_decrcvd alpha if year==2050, color(black)), ///
xlab(0(.5)1,format(%9.1f) labsize(medsmall))            ///
xmlabel(0.166666667(.166666667).33333333 .66666666667(.166666666).833333333 ,format(%9.3f) labsize(medsmall))            ///
xtitle("Share of CVD mortality reduction attributable to reduced incidence", size(small))  ytitle(`"Total discounted value, 2012-2050 (billions of 2012C$)"',size(small)) legend(off) graphregion(color(white)) ylabel(0(20)100)
graph export "$path_exp/scenario/alpha.tiff", as(tif) replace

 



*****Population
global path = "/PATH/OUTPUT/"

global scenario "reference decrcvd decrcvd1 decrcvd2 decrcvd3 decrcvd4 decrcvd5 decrcvd6"
global scenario_1 " decrcvd decrcvd1 decrcvd2 decrcvd3 decrcvd4 decrcvd5 decrcvd6 "

foreach run of global scenario {
	use "$path/`run'/pop_sum_age_value1.dta",clear
	global file = ""
	forvalues i = 2/100 {
	global file = "$file $path/`run'/pop_sum_age_value`i'.dta"
	}

	append using $file, gen(coef)
	rename pop pop_`run'
	drop pop_sd
	save $path/`run'/pop_sum_age_value.dta, replace

}


use "$path/reference/pop_sum_age_value.dta",clear

foreach run of global scenario_1 {
	merge 1:1 year age coef using "$path/`run'/pop_sum_age_value.dta"
	drop _merge
}

collapse (sum) pop_decrcvd pop_decrcvd1 pop_decrcvd2 pop_decrcvd3 pop_decrcvd4 pop_decrcvd5 pop_decrcvd6 pop_reference, by(year coef)

bysort coef (year) : gen n = (_n-1)*2

expand 2
sort coef year
bysort coef year : replace n = n-1+_n
bysort coef year : replace year = year+_n-1


foreach run of global scenario { 
replace pop_`run' = (pop_`run'[_n-1]+pop_`run'[_n+1])/2 if mod(year,2)
}

	bysort year: egen pct5_pop_reference = pctile(pop_reference), p(5)
	bysort year: egen pct95_pop_reference = pctile(pop_reference), p(95)
	bysort year: egen mean_pop_reference = mean(pop_reference)
	
foreach run in decrcvd  decrcvd3 { 
	
	gen d_pop_`run' = (pop_`run'-pop_reference)
	gen perc_pop_`run' = (pop_`run'-pop_reference)/pop_reference
	bysort year: egen pct5d_pop_`run' = pctile(d_pop_`run'), p(5)
	bysort year: egen pct95d_pop_`run' = pctile(d_pop_`run'), p(95)
	bysort year: egen meand_pop_`run' = mean(d_pop_`run')
	
	bysort year: egen pct5_perc_pop_`run' = pctile(perc_pop_`run'), p(5)
	bysort year: egen pct95_perc_pop_`run' = pctile(perc_pop_`run'), p(95)
	bysort year: egen mean_perc_pop_`run' = mean(perc_pop_`run')

}

bysort year : keep if _n==1

keep year mean* pct*

keep if inlist(year,2020,2030,2035,2050)
