clear all
capture log close
set more off

cd ~/compas
program def popstatsbyage

		* prevalence by 5 year age cells

		*use output/$scenario/simpop.dta,clear
		drop if year>2050
		egen age5 = cut(age), at(30(5)110)
		qui tab educ, gen(educ_)
		qui tab smoke, gen(smoke_)
		qui tab bmi, gen(bmi_)
		gen cognitive= (inv==2|inv==5|inv==7)
		gen iadl =(inv==3|inv==5|inv==6|inv==7)
		gen adl =(inv==4|inv==6|inv==7)
		gen nhome =(ltc==3)
		gen homecare =(ltc==2)
		rename bmi_2 obese
		rename bmi_3 veryobese
		rename smoke_2 smoker
		rename smoke_3 former
		rename educ_1 lesshs
		rename educ_2 hs
		rename educ_3 college
		rename educ_4 univ
		gen pop = 1
		foreach v of var cost_* {
			replace `v' = . if `v'==0
		}
		#d ;
		global vars = "pop imm sex diabe hibpe hearte stroke cancre lunge mentae  
		obese veryobese smoker former lesshs hs college univ cognitive iadl adl
		hc_* income
		nhome homecare
		cost_* hours_* nyrs* agefirst* agedead
		total* ever*";
		#d cr

		collapse ($stat) $vars [fw=wgt], by(year age5 province) fast
		*save output/$scenario/`stat'byyearage.dta, replace
end

program def popstats
		* prevalence overall
		*use output/$scenario/simpop.dta	, clear
		drop if year>2050
		qui tab educ, gen(educ_)
		qui tab smoke, gen(smoke_)
		qui tab bmi, gen(bmi_)
		gen cognitive= (inv==2|inv==5|inv==7)
		gen iadl =(inv==3|inv==5|inv==6|inv==7)
		gen adl =(inv==4|inv==6|inv==7)
		gen nhome =(ltc==3)
		gen homecare =(ltc==2)
		rename bmi_2 obese
		rename bmi_3 veryobese
		rename smoke_2 smoker
		rename smoke_3 former
		rename educ_1 lesshs
		rename educ_2 hs
		rename educ_3 college
		rename educ_4 univ	
		gen pop = 1
		foreach v of var cost_* {
			replace `v' = . if `v'==0
		}
		#d ;
		global vars = "pop age imm sex diabe hibpe hearte stroke cancre lunge mentae  
		obese veryobese smoker former lesshs hs college univ cognitive iadl adl
		hc_* income
		homecare nhome
		cost_*  hours_* nyrs* agefirst* agedead
		total* ever*";
		#d cr
		collapse ($stat) $vars [fw=wgt], by(year province) fast
		if ("`stat'"=="sum") {
			merge 1:1 year province using params/trends/trend-pop-under30.dta
			gen totpop = pop + popunder30
			label var totpop "total population, includes <30"
		}
		*save output/$scenario/`stat'byyear.dta, replace
end


*all
#d ;
		global statvars = "pop imm sex diabe hibpe hearte stroke cancre lunge mentae  
		obese veryobese smoker former lesshs hs college univ cognitive iadl adl
		hc_* income
		homecare nhome
		cost_*  hours_* nyrs* agefirst* agedead
		total* ever*";
		#d cr
		
timer on 1
parallel setclusters 50,force
global stat = "mean"
parallel append, do(popstats) programs(popstats) e("output/$scenario/simpop-%01.0f, 1/$rep")
 foreach v of var $statvars {
			local l`v' : variable label `v'
			if `"`l`v''"' == "" {
				local l`v' "`v'"
			}
		}
collapse (mean) $statvars , by(year province) fast
foreach v of var $statvars {
			label var `v' "`l`v''"
		}
save output/$scenario/meanbyyear.dta, replace
timer off 1
timer list
timer clear

timer on 1
global stat = "sum"
parallel append, do(popstats) programs(popstats) e("output/$scenario/simpop-%01.0f, 1/$rep")
 foreach v of var $statvars {
			local l`v' : variable label `v'
			if `"`l`v''"' == "" {
				local l`v' "`v'"
			}
		}
collapse (mean) $statvars , by(year province) fast
foreach v of var $statvars {
			label var `v' "`l`v''"
		}
save output/$scenario/sumbyyear.dta, replace
timer off 1
timer list
timer clear
*by age 
timer on 1
parallel setclusters 50,force
global stat = "mean"
parallel append, do(popstatsbyage) programs(popstatsbyage) e("output/$scenario/simpop-%01.0f, 1/$rep")
 foreach v of var $statvars {
			local l`v' : variable label `v'
			if `"`l`v''"' == "" {
				local l`v' "`v'"
			}
		}
collapse (mean) $statvars , by(year province age5) fast
foreach v of var $statvars {
			label var `v' "`l`v''"
		}
save output/$scenario/meanbyyearage.dta, replace
timer off 1
timer list
timer clear

timer on 1
global stat = "sum"
parallel append, do(popstatsbyage) programs(popstatsbyage) e("output/$scenario/simpop-%01.0f, 1/$rep")
 foreach v of var $statvars {
			local l`v' : variable label `v'
			if `"`l`v''"' == "" {
				local l`v' "`v'"
			}
		}
collapse (mean) $statvars , by(year province age5) fast
foreach v of var $statvars {
			label var `v' "`l`v''"
		}
save output/$scenario/sumbyyearage.dta, replace
timer off 1
timer list
timer clear
