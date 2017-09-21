clear all
capture log close
set more off

cd "T:\Projet 3985-S003\compas\"
set maxvar 10000
set more off
use "data\clean_CCHS.dta", replace
*Modele: utilisation coupes transversales
*echantillon
drop if status==3 //garde les gens en menage prive seulement
keep if year>1999 // estime l'utilisation seulement a partir de 2000 pour limiter les chances qu'il y ait des changements importants

*globals
global yvar "generalist specialist nights"
global yvar_d "homecare_f homecare_i"
global demo "imm hs college univ quebec ontario prairies bc"
global age "age50m age50p"
global cc "diabe hibpe cancre hearte stroke lunge mentae"
global risk "obese veryobese smoker former"
global invalidity "adl iadl cognitive"

qui do "J:\LOGICIELS\STATA\Fichiers ADO - ADO files\estout.ado"

**************Modele de type binomial negatif - nuits et consultations**********

foreach var of varlist $yvar  {

nbreg `var' sexe $age $demo $cc $risk $invalidity [pw=WT64LS] if  flag_hibpe_long==0 & flag_stroke_long==0 & flag_diabe_long==0,  cluster(id)
estimates store b_`var'
margins, dydx(*) post	
estimates store b_`var'_margin
}

estout b_generalist b_specialist b_nights, stat(N) 
estout b_generalist_margin b_specialist_margin b_nights_margin, stat(N) cell(b(star fmt(%9.3f)))

********************Modele de type logit - domicile et medic********************


logit drugs sexe $age $demo $cc $risk $invalidity [pw=WT64LS] if  flag_hibpe_long==0 & flag_stroke_long==0 & flag_diabe_long==0,  cluster(id)
estimates store b_drugs
mat table=r(table)
mat a=(table)'	
putexcel set "T:\Projet 3985-S003\compas\output\Utilisation_new_educ.xlsx", sheet(drugs) modify
putexcel A2=matrix(a), names colwise
margins, dydx(*) post
estimates store b_drugs_margin


estout b_drugs b_drugs_margin , stat(N) cell(b(star fmt(%9.3f)))
estout  , stat(N)


********************Modele de type mullogit multinomial- soins domicile *************************
/****** Gen new level_care avec level_care =1 si home_care_i == .*/
replace level_care =. if homecare_i == .
mlogit level_care sexe $age $demo $cc $risk $invalidity [pw=WT64LS] if  flag_hibpe_long==0 & flag_stroke_long==0 & flag_diabe_long==0,  cluster(id)
estimates store b_levelcare
margins, dydx(*) post	

estimates store b_levelcare_margin
estout b_levelcare ,stats(N)
estout b_levelcare_margin ,stats(N) cell(b(star fmt(%9.3f)))


capture log close
