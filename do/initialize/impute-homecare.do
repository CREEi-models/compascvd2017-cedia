*** Impute homecare for 2010 because homecare is in the survey but not in the PUMF
preserve
use temp/clean-cchs.dta, clear

//Impute for 2010
* parameter estimates from CCHS 2010 (homecare is in the survey but not in the PUMF)
scalar coef_inst_age0   = 0.017786		// splines d'ages
scalar coef_inst_age75  = 0.0780361
scalar coef_inst_imm 	= 0.041716		// 0 : non-immig , 1:immigrant
scalar coef_inst_sex	= 0.1085491		// sexe: 0:homme, 1: femme
scalar coef_inst_adl	= 0.860386		// 1 adl +
scalar coef_inst_iadl	= 2.343526		// 1+ iadl
scalar coef_inst_cogn	= 0.5239294		// cognitif
scalar coef_inst_const  = -4.552331

* covariates
gen adl = (inv==4 | inv==6  | inv==7) if inv!=.
gen iadl = (inv==3 | inv==5  | inv==6| inv==7) if inv!=.
gen cognitive = (inv==2 | inv==5  | inv==7) if inv!=.
gen splineage0 = min(age,75)
gen splineage75= max(age-75,0)

*Scores
gen xbeta_homecare =	coef_inst_age0 	* splineage0  + ///
					coef_inst_age75   * splineage75 + ///
					coef_inst_imm	 	* imm 	  + ///
					coef_inst_sex		* sex + ///
					coef_inst_adl		* adl		+ ///
					coef_inst_iadl		* iadl		+ ///
					coef_inst_cogn		* cognitive		+ ///
					coef_inst_const

gen probhomecare = exp(xbeta_homecare)/(1+exp(xbeta_homecare))


gen u = runiform()
gen care =(u<=probhomecare) if probhomecare!=.
replace ltc = 2 if care ==1 & flag ==0
drop u probhomecare adl iadl cognitive splineage0 splineage75 xbeta_homecare
save temp/clean-cchs.dta, replace
restore
/*
*Impute for 2012
preserve
use temp/clean-cchs.dta, clear

* parameter estimates from CCHS 2012 (homecare is in the survey but not in the PUMF)
scalar coef_inst_age0   = 0.0217217		// splines d'ages
scalar coef_inst_age75  = 0.0866456
scalar coef_inst_imm 	= -0.2806149		// 0 : non-immig , 1:immigrant
scalar coef_inst_sex	= 0.200277		// sexe: 0:homme, 1: femme
scalar coef_inst_adl	= 1.499102		// 1 adl +
scalar coef_inst_iadl	= 2.372641		// 1+ iadl
scalar coef_inst_const  = -4.949173

* covariates
gen adl = (inv==4 | inv==6  | inv==7) if inv!=.
gen iadl = (inv==3 | inv==5  | inv==6| inv==7) if inv!=.
gen cognitive = (inv==2 | inv==5  | inv==7) if inv!=.
gen splineage0 = min(age,75) if age!=.
gen splineage75= max(age-75,0) if age!=.
gen xbeta_homecare =	coef_inst_age0 	* splineage0  + ///
					coef_inst_age75   * splineage75 + ///
					coef_inst_imm	 	* imm 	  + ///
					coef_inst_sex		* sex + ///
					coef_inst_adl		* adl		+ ///
					coef_inst_iadl		* iadl		+ ///
					coef_inst_const

gen probhomecare = exp(xbeta_homecare)/(1+exp(xbeta_homecare))


gen u = runiform()
gen care =(u<=probhomecare) if probhomecare!=.

replace ltc = care if flag == 2
drop u probhomecare adl iadl cognitive splineage0 splineage75 xbeta_homecare flag
save temp/clean-cchs.dta, replace

restore
*/
