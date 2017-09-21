


use  temp/clean-cchs-2010.dta, clear
set more off

append using temp/clean-cchs-08-09,  gen(flag)

keep if ageg<45
sort flag
by flag :egen tot_wgt_year = total(wgt)
egen tot_wgt = total(wgt)
gen tot_wgt_2010=tot_wgt_year[1]

gen new_wgt = tot_wgt_2010*wgt/tot_wgt
by flag :egen tot_new_wgt_year = total(new_wgt)
egen tot_new_wgt = total(new_wgt)

replace wgt=round(new_wgt)

save temp/temp1, replace

use  temp/clean-cchs-2010.dta, clear
set more off

append using temp/clean-cchs-08-09 ,  gen(flag)

keep if ageg>=45
sort flag
by flag :egen tot_wgt_year = total(wgt)
egen tot_wgt = total(wgt)
gen tot_wgt_2010=tot_wgt_year[1]

gen new_wgt = tot_wgt_2010*wgt/tot_wgt
by flag :egen tot_new_wgt_year = total(new_wgt)
egen tot_new_wgt = total(new_wgt)

replace wgt=round(new_wgt)

save temp/temp2, replace

use temp/temp1, clear

append using temp/temp2


#d ;
keep id ageg province sex bmi hibpe lunge diabe hearte cancre stroke  
 inv smoke imm educ4 income ltc wgt flag;
order id ageg province sex bmi hibpe lunge diabe hearte cancre stroke  
 inv smoke imm educ4 income ltc wgt;
qui reg id ageg province sex bmi hibpe lunge diabe hearte cancre stroke  
 inv smoke imm educ4 income ltc wgt;
keep if e(sample)==1;
#d cr

sum

save temp/clean-cchs-first.dta, replace
