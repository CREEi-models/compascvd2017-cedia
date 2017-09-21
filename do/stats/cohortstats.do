* main code to produce statistics from a scenario
clear all
capture log close
set more off

cd ~/compas

program def cohortstats
		* prevalence by 5 year age cells

		*use output/$scenario/simpop.dta,clear
		qui sum byear
		local maxbyear = r(max)+2
		*qui egen byear2 = cut(byear), at(1910(2)`maxbyear')
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
		qui gen pop = 1
		*sort byear2 year
		*qui by byear2: replace wgt = wgt[1]
		foreach v of var cost_* {
			replace `v' = . if `v'==0
		}
		#d ;
		global vars = "pop imm sex diabe hibpe hearte stroke cancre lunge mentae  
		obese veryobese smoker former lesshs hs college univ cognitive iadl adl
		hc_* cost_* hours* nyrs* agefirst* agedead total* ever* income homecare nhome";
		#d cr
		 foreach v of var $vars {
			local l`v' : variable label `v'
			if `"`l`v''"' == "" {
				local l`v' "`v'"
			}
		}
		collapse ($stat) $vars [fw=wgt], by(byear year province) fast
		qui gen age = year - byear 
		foreach v of var $vars {
			label var `v' "`l`v''"
		}
		*save output/$scenario/`stat'bycohortyear.dta, replace
end
#d ;
		global statvars = "pop imm sex diabe hibpe hearte stroke cancre lunge mentae  
		obese veryobese smoker former lesshs hs college cognitive iadl adl
		hc_* cost_* hours* nyrs* agefirst* agedead total* ever* age income homecare nhome";
		#d cr
timer on 1

parallel setclusters 50,force
global stat = "mean"
parallel append, do(cohortstats) programs(cohortstats) e("output/$scenario/simpop-%01.0f, 1/$rep")
 foreach v of var $statvars {
			local l`v' : variable label `v'
			if `"`l`v''"' == "" {
				local l`v' "`v'"
			}
		}
collapse (mean) $statvars , by(byear year province) fast
foreach v of var $statvars {
			label var `v' "`l`v''"
		}
save output/$scenario/meanbycohortyear.dta, replace
timer off 1
timer list
timer clear

timer on 1
global stat = "sum"
parallel append, do(cohortstats) programs(cohortstats) e("output/$scenario/simpop-%01.0f, 1/$rep")
 foreach v of var $statvars {
			local l`v' : variable label `v'
			if `"`l`v''"' == "" {
				local l`v' "`v'"
			}
		}
collapse (mean) $statvars , by(byear year province) fast
foreach v of var $statvars {
			label var `v' "`l`v''"
		}
save output/$scenario/sumbycohortyear.dta, replace
timer off 1
timer list
timer clear
