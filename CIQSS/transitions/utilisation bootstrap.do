
clear all
clear matrix
capture log close

cd "T:\Projet 3985-S003\compas"
set maxvar 10000
set more off
use "data\clean_CCHS.dta", replace

*globals
global yvar "generalist specialist nights"
global yvar_d "homecare_f homecare_i"
global demo "imm hs college univ quebec ontario prairies bc"
global age "age50m age50p"
global cc "diabe hibpe cancre hearte stroke lunge mentae"
global risk "obese veryobese smoker former"
global invalidity "adl iadl cognitive"

gen boot_weight=0
global n_rep=300
matrix generalist=J($n_rep,27,0)
matrix generalist_conv=J($n_rep,1,0)
matrix specialist=J($n_rep,27,0)
matrix specialist_conv=J($n_rep,27,0)
matrix nights=J($n_rep,27,0)
matrix nights_conv=J($n_rep,27,0)

set seed 1
* voir combien d'individus distincts dans la base
*************************************************
*****************REGRESSIONS*********************
*************************************************
forvalue i=1/$n_rep {
sort REALUKEY
bsample 14939, cluster(REALUKEY) weight(boot_weight)
replace boot_weight=boot_weight*WT64LS
preserve
drop if status==3 //garde les gens en menage prive seulement
keep if year>1999 // estime l'utilisation seulement a partir de 2000 pour limiter les chances qu'il y ait des changements importants
qui nbreg generalist sexe $age $demo $cc $risk $invalidity [pw=boot_weight] if  flag_hibpe_long==0 & flag_stroke_long==0 & flag_diabe_long==0,  cluster(id) iterate(50)	
matrix generalist[`i',1]=e(b)
matrix generalist_conv[`i',1]=e(converged)
if `i'==1{
#delimit ;
local rname="sexe age50m age50p imm hs college univ quebec ontario prairies bc diabe hibpe cancre hearte stroke lunge mentae obese veryobese smoker former adl iadl cognitive constant lnalpha";
#delimit cr
matrix colnames generalist=`rname'
}

restore
preserve 
drop if status==3 //garde les gens en menage prive seulement
keep if year>1999 // estime l'utilisation seulement a partir de 2000 pour limiter les chances qu'il y ait des changements importants
qui nbreg specialist sexe $age $demo $cc $risk $invalidity [pw=boot_weight] if  flag_hibpe_long==0 & flag_stroke_long==0 & flag_diabe_long==0,  cluster(id) iterate(50)	
matrix specialist[`i',1]=e(b)
matrix specialist_conv[`i',1]=e(converged)
if `i'==1{
#delimit ;
local rname="sexe age50m age50p imm hs college univ quebec ontario prairies bc diabe hibpe cancre hearte stroke lunge mentae obese veryobese smoker former adl iadl cognitive constant lnalpha";
#delimit cr
matrix colnames specialist=`rname'
}
restore
preserve
drop if status==3 //garde les gens en menage prive seulement
keep if year>1999 // estime l'utilisation seulement a partir de 2000 pour limiter les chances qu'il y ait des changements importants
qui nbreg nights sexe $age $demo $cc $risk $invalidity [pw=boot_weight] if  flag_hibpe_long==0 & flag_stroke_long==0 & flag_diabe_long==0,  cluster(id) iterate(50)	
matrix nights[`i',1]=e(b)
matrix nights_conv[`i',1]=e(converged)
if `i'==1{
#delimit ;
local rname="sexe age50m age50p imm hs college univ quebec ontario prairies bc diabe hibpe cancre hearte stroke lunge mentae obese veryobese smoker former adl iadl cognitive constant lnalpha";
#delimit cr
matrix colnames nights=`rname'
}
restore
}
clear
svmat generalist, names(col)
saveold "data\generalist_coef.dta", replace version(13) 
clear
svmat generalist_conv, names(col)
saveold "data\generalist_conv.dta", replace version(13)
clear

clear
svmat specialist, names(col)
saveold "data\specialist_coef.dta", replace version(13) 
clear
svmat specialist_conv, names(col)
saveold "data\specialist_conv.dta", replace version(13)
clear

clear
svmat nights, names(col)
saveold "data\nights_coef.dta", replace version(13) 
clear
svmat nights_conv, names(col)
saveold "data\nights_conv.dta", replace version(13)
clear
