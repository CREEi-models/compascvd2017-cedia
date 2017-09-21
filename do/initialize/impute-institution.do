preserve
use temp/clean-cchs.dta, clear
* parameter estimates from NHPS for institutionalization
scalar coef_inst_age0   = 0.1037178		// splines d'ages
scalar coef_inst_age75  = 0.1052882
scalar coef_inst_imm 	= -0.9730108		// 0 : non-immig , 1:immigrant
scalar coef_inst_sex	= 0.0473625 		// sexe: 0:homme, 1: femme
scalar coef_inst_adl	= 3.815055		// 1+ adl
scalar coef_inst_iadl	= -0.5592247		// 1+ iadl
scalar coef_inst_cogn	= 0.9756004		// cognitive
scalar coef_inst_const  = -13.14913
* covariates
gen adl = (inv==4 | inv==6  | inv==7) if inv!=.
gen iadl = (inv==3 | inv==5  | inv==6| inv==7) if inv!=.
gen cognitive = (inv==2 | inv==5  | inv==7) if inv!=.
gen splineage0 = min(age,75) if age!=.
gen splineage75= max(age-75,0) if age!=.
gen xbeta_inst =	coef_inst_age0 	* splineage0  + ///
					coef_inst_age75   * splineage75 + ///
					coef_inst_imm	 	* imm 	  + ///
					coef_inst_sex		* sex + ///
					coef_inst_adl		* adl		+ ///
					coef_inst_iadl		* iadl		+ ///
					coef_inst_cogn		* cognitive		+ ///
					coef_inst_const											
gen probinst = exp(xbeta_inst)/(1+exp(xbeta_inst)) 

egen ageg5 = cut(age), at(30 75(5)85 110)
table ageg5 province [fw=wgt], content(mean probinst)  
gen u = runiform() 
gen inv3 =(u<=probinst) if probinst!=.
table ageg5 province [fw=wgt], content(mean inv3)  
drop ageg5
replace ltc = 3 if inv3==1
tab age ltc [fw=wgt],  row
tab province ltc [fw=wgt], row
drop u probinst adl iadl cognitive splineage0 splineage75 xbeta_inst		
save temp/clean-cchs.dta, replace
restore
