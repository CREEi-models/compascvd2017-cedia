*Creation  of the dataset  

clear all
clear matrix
set maxvar 10000
set more off

scalar start = 1994
scalar stop  = 2010

cd "\compas\creation-variable"
use "S:\ENSP - NPHS_HH_LONG\NPHS_ENSP_HH_LONG\main_principal\data_donnees\main_principal\stata_fr\SDDS3225_NPHS_ENSP_HH_LONG_C1_C9_LONG_F1_fra.dta", clear  // Volet ménage (tous les cycles)
gen AOD = YOD -YOB if YOD < 9000 //Age of death

local t=start

	forvalue i = 1/9{
	gen status`t' =  substr(LONGPAT,`i',1)
	destring status`t', replace
	local `t'=`++t'
	local `t'=`++t' // Deux ans entre les cycles
						
	}

do "do\classement_CC.do"
do "do\other_variable.do"
do "do\classement_facteur_risque.do"
do "do\classement_invalidite.do"
do "do\utilisation_ressources.do"
do "do\codes_medic"
do "do\drugs"
		
*indicateur= "0" si vivant, "1" si decedes dans le cycle suivante, "." apres
forvalue t = `=start'(2)`=stop'{
	gen ind_deces`t' = (YOD-1 == `t')|(YOD-2 == `t') // meurt a la periode suivante
		
	if(`t'!=`=start')	{
	
	*Correction pour les YOD non-declarees, mais decede dans la variable status
	*(on met l'annee de dececs juste avant le cycle ou on observe decede)
	local j = `t'-2
	replace ind_deces`t' = 1 	if status`t'==2 & status`j' !=2 & YOD==9999
	replace YOD = `t'-1 		if status`t'==2 & status`j' !=2 & YOD==9999 
	
	*On met des points une fois la personne deja morte
	replace ind_deces`t' = . if ind_deces`j'==1 | ind_deces`j'==.
	
	}
}	
   
	  
order REALUKEY SEX IMM YOB YOD AOD LONGPAT status* $demo

#delimit ;
keep REALUKEY SEX IMM DESIGPRV YOB YOD AOD LONGPAT  WF6DLF WT64LS 
		status* ind_deces* $demo 	
		$prevalence $never  hearte_attack* hearte_angina* hearte_chf*
		fumeur* IMC* cognitive* adl* iadl* hui*      
		generalist* specialist* drugs* 
		night* nights* homecare* homecare_f* homecare_i*
		medic1_* medic2_* medic3_* medic4_* medic5_* medic6_* medic7_* medic8_* medic9_* medic10_* medic11_* medic12_* 
		drug1_* drug2_* drug3_* drug4_* drug5_* drug6_* drug7_* drug8_* drug9_* drug10_* drug11_* drug12_* drug13_* drug14_* drug15_* drug16_* drug17_* drug18_* drug19_* drug20_* drug21_* ; 
		
#delimit cr


replace SEX = 0 if SEX==1  // Homme = 0
replace SEX = 1 if SEX==2  // Femme = 1
replace IMM = 0 if IMM==2  // non-immigrant =0, immigrants =1
replace IMM=. if IMM==9
rename IMM imm
rename SEX sexe
*label drop _all

gen id = _n

save "\data.dta", replace

*******************************************************************************
*******************************************************************************
*******************************************************************************

use "\data.dta", clear

#delimit ;

reshape long status ind_deces 
age scol quebec ontario prairies atlantic bc totincome couple enfant 	
diabe hibpe cancre hearte stroke lunge mentae diabe_never stroke_never hibpe_never hearte_angina hearte_attack hearte_chf
generalist specialist drugs night nights homecare homecare_f homecare_i 
medic1_ medic2_ medic3_ medic4_ medic5_ medic6_ medic7_ medic8_ medic9_ medic10_ medic11_ medic12_ 
drug1_ drug2_ drug3_ drug4_ drug5_ drug6_ drug7_ drug8_ drug9_ drug10_ drug11_ drug12_ drug13_ drug14_ drug15_ drug16_ drug17_ drug18_ drug19_ drug20_ drug21_ 
cognitive adl iadl hui fumeur IMC IMC_C IMC_FEM, i(REALUKEY) j(Année) ;

#delimit cr

*idiabe ihibpe icancre ihearte istroke ilunge imentae
	 
drop if ind_deces==.

forvalues i = 1/12{

rename medic`i'_  medic`i'
}

*Variable de province en 1994
gen quebec_ini = (DESIGPRV==24) if DESIGPRV
gen atlantic_ini = (DESIGPRV==10 | DESIGPRV==11 | DESIGPRV==12 |DESIGPRV==13)
gen ontario_ini = (DESIGPRV==35)
gen prairies_ini = (DESIGPRV==46 | DESIGPRV==47 | DESIGPRV==48)
gen bc_ini = (DESIGPRV==59)


*Variables d'age

egen age5  = cut(age), at(20(5)100)
egen age10 = cut(age), at(20(10)100)
egen age40 = cut(age), at(20,40,65,100)	

gen age50m = min(age5,50)
gen age50p = max(age5-50,0)	

*Variable de revenus
rename totincome income

recode income (1/5=1) (6/7=2) (8/9=3) (10=4) (11/12=5) // moins de 10 000$ ensemble et plus de 80000$ ensemble

*Panel
tsset id Année, delta(2)

*ADl-IADL : creer des variables indicatrices
*forvalues i=0/2 {
*gen iadl`i' = (iadl==`i')	
*replace iadl`i'=. if iadl==. // pour que les missing le restent
*}


*invalidite: cre la variable a 7 categories

gen inv7=. 
global inv "cognitiv adl iadl"
replace inv7=1 if cognitiv==0 & adl==0 &iadl==0 
replace inv7=2 if cognitiv==1 & adl==0 &iadl==0
replace inv7=3 if cognitiv==0 & adl==0 &iadl>0
replace inv7=4 if cognitiv==0 & adl==1 &iadl==0
replace inv7=5 if cognitiv==1 & adl==0 &iadl>0
replace inv7=7 if cognitiv==1 & adl==1 &iadl==0 //Il y a 0.02% des gens dans cette cateorie et elle me semble peu logique (ADl, cognitif mais pas IADL, je l'ai recodee a 1,1,1)
replace inv7=6 if cognitiv==0 & adl==1 &iadl>0
replace inv7=7 if cognitiv==1 & adl==1 &iadl>0


*soins longue duree: creer variable categorie
*0 = ni soins a domicile ni institution 1=recoit des soins a domicile 2=est en institution

gen ltc=. 
replace ltc=0 if homecare==0
replace ltc=1 if homecare==1 
replace ltc=2 if status==3

gen nhome =(status==3)

*Soins a domicile: creer variable categorie: 1 formels, 2 informels, 3 les 2

gen level_care=. 
replace level_care=1 if homecare_f==1 & homecare_i!=1
replace level_care=2 if homecare_i==1 & homecare_f!=1
replace level_care=3 if homecare_i==1 & homecare_f==1


*Tabagisme: creer des variables indicatrices
gen neversmoked =(fumeur==0)
gen smoker = (fumeur==1)
gen former =(fumeur==2)

foreach var of varlist neversmoked smoker former {
replace `var'=. if fumeur==. //correction pour les NSP, R et SO
}


*IMC: creer des variables indicatrices
gen nonobese =(IMC==0)
gen obese =(IMC==1)
gen veryobese =(IMC==2)

foreach var of varlist nonobese obese veryobese {
replace `var'=. if IMC==. //correction pour les NSP, R et SO

}

*Education: creer des variables indicatrices
gen lesshs = (scol==0)
gen hs = (scol==1) 
gen college = (scol==2)
gen univ = (scol==3)


foreach var of varlist lesshs hs college univ {

replace `var'=. if scol==.  //correction pour les NSP, R et SO

}



rename Année year

do "do\dispute.do"
do "do\absorbing.do"
do "do\incidence_remission.do"
drop if age<30

quietly do "do\label.do"


save "\data\clean_CCHS", replace



