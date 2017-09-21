*transitions entre les 3 categories de soins de sante

clear all
clear matrix
capture log close

cd "T:\Projet 3985-S003\compas\"
set maxvar 10000
set more off
use "data\clean_CCHS.dta", replace


***********
*** LTC ***
***********


gen f_ltc = f.ltc

mlogit  f_ltc					///
		age50* 					/// Effets d'age
		diabe hibpe cancre 		///
		hearte stroke lunge 	///
		mentae	 				/// Prevalence d'autres maladies
	    smoker	former			/// Indicatrices fumeur, ancien fumeur
		obese veryobese			/// Indicatrice obese, tres obese
		sexe imm 				/// Caracteristiques socio-ecn
		hs college univ			///
		i.ltc adl iadl cognitive	///
		quebec 					///
		ontario 				///
		prairies bc				/// indicatrice prov
		[pw=WT64LS]	 			///
		if  flag_hibpe_long==0 & flag_stroke_long==0 & flag_diabe_long==0 & ltc!=2 ///
	  ,cluster(id) 
ereturn list
*margins, dydx(*) post		
*estimates store ltc_margin


*estout ltc , drop(0b*) cell(b( fmt(%9.6f))) stats(N)
*estout ltc_margin , cell(b(star fmt(%9.3f))) stats(N)
******************
*** Invalidite ***
******************

gen f_inv7 = f.inv7

mlogit  f_inv7					///
		age50*  				/// Effets d'age
		diabe hibpe cancre 		///
		hearte stroke lunge 	///
		mentae	 				/// Prevalence d'autres maladies
		smoker	former			/// Indicatrices fumeur, ancien fumeur
		obese veryobese			/// Indicatrice obese, tres obese
		sexe imm 				/// Caracteristiques socio-ecn
		hs college univ			///		
		i.inv7					/// invalidite
		quebec					///
		ontario					///
		prairies bc				/// indicatrice prov
		[pw=WT64LS]	 			///
		if  flag_hibpe_long==0 & flag_stroke_long==0 & flag_diabe_long==0 & ltc!=2  ,cluster(id) 
ereturn list
*margins, dydx(*) post
*estimates store invalidite_margin

*estout invalidite , drop(1b*) cell(b(star fmt(%9.6f))) stats(N)


****************
*** Maladies ***
****************
gen age60m = min(age,60)
gen age60p = max(age-60,0)
cloglog idiabe					///
		age60* 					/// Effets d'age
		i.year ///
		smoker	former			/// Indicatrices fumeur, ancien fumeur
		obese veryobese			/// Indicatrice obese, tres obese
		sexe imm 				/// Caracteristiques socio-ecn
		hs college univ			///		
		quebec					///
		ontario					///
		prairies bc				/// indicatrice prov
		[pw=WT64LS]	 			///
		if  flag_diabe_long==0 & ltc!=2 ,cluster(id)
		ereturn list
*margins, dydx(*) post	
*estimates store inc_diab_pond_margin 		
		
***************************************************************************
cloglog ihibpe					///
		age60* 					/// Effets d'age
		i.year ///
		diabe	 				/// Prevalence d'autres maladies
		smoker	former			/// Indicatrices fumeur, ancien fumeur
		obese veryobese			/// Indicatrice obese, tres obese
		sexe imm 				/// Caracteristiques socio-ecn
		hs college univ			///		
		quebec					///
		ontario					///
		prairies bc				/// indicatrice prov
		[pw=WT64LS]				///
		if  flag_hibpe_long==0 & flag_diabe_long==0 & ltc!=2 ,cluster(id)
ereturn list
*margins, dydx(*) post	
*estimates store inc_hyper_pond_margin 		

	
***************************************************************************
cloglog icancre					///
		age60* 					/// Effets d'age
		i.year ///
		smoker	former			/// Indicatrices fumeur, ancien fumeur
		obese veryobese			/// Indicatrice obese, tres obese
		sexe imm 				/// Caracteristiques socio-ecn
		hs college univ			///		
		quebec					///
		ontario					///
		prairies bc				/// indicatrice prov
		[pw=WT64LS]	 			///
		if  ltc!=2,cluster(id)
		ereturn list
*margins, dydx(*) post
*estimates store inc_cancer_pond_margin

logit rcancre					///
		age60* 					/// Effets d'age
		i.year						/// Prevalence d'autres maladies
		smoker	former			/// Indicatrices fumeur, ancien fumeur
		obese veryobese			/// Indicatrice obese, tres obese
		sexe imm 				/// Caracteristiques socio-ecn
		hs college univ			///		
		quebec					///
		ontario					///
		prairies bc				/// indicatrice prov
		[pw=WT64LS]	 			///
		if  ltc!=2,cluster(id)	
		ereturn list
*margins, dydx(*) post
*estimates store rem_cancer_pond_margin	
		
***************************************************************************
cloglog ihearte						///
		age60* 						/// Effets d'age
		i.year ///
		hibpe diabe 				/// Prevalence d'autres maladies
		smoker	former				/// Indicatrices fumeur, ancien fumeur
		obese veryobese				/// Indicatrice obese, tres obese
		sexe imm 					/// Caracteristiques socio-ecn
		hs college univ				///		
		quebec						///
		ontario						///
		prairies bc					/// indicatrice prov		[pw=WT64LS]	 ///
		if  flag_hibpe_long==0 & flag_diabe_long==0  & ltc!=2,cluster(id)	
		ereturn list
*margins, dydx(*) post
*estimates store inc_heart_pond_margin 	

logit rhearte						///
		age60* 						/// Effets d'age
		i.year ///
		hibpe diabe 				/// Prevalence d'autres maladies
		smoker	former				/// Indicatrices fumeur, ancien fumeur
		obese veryobese				/// Indicatrice obese, tres obese
		sexe imm 					/// Caracteristiques socio-ecn
		hs college univ				///		
		quebec						///
		ontario						///
		prairies bc					/// indicatrice prov		[pw=WT64LS]	 ///
		if  flag_hibpe_long==0 & flag_diabe_long==0  & ltc!=2,cluster(id)
		ereturn list
*margins, dydx(*) post
*estimates store rem_heart_pond_margin 		
		
***************************************************************************
preserve 
drop if age < 40
gen age85m = min(age,85)
gen age85p = max(age-85,0)
cloglog istroke						///
		age85* 		/// Effets d'age
		i.year ///
		hearte hibpe				///
		diabe cancre  				/// Prevalence d'autres maladies
		smoker	former				/// Indicatrices fumeur, ancien fumeur
		obese veryobese				/// Indicatrice obese, tres obese
		sexe imm 					/// Caracteristiques socio-ecn
		hs college univ				///		
		quebec						///
		ontario						///
		prairies bc					/// indicatrice prov
		[pw=WT64LS]	 				///
		if  flag_hibpe_long==0 & flag_stroke_long==0 & flag_diabe_long==0  & ltc!=2,cluster(id)			
ereturn list
		*margins, dydx(*) post
*estimates store inc_stroke_pond_margin 			
		
logit rstroke						///
		age50* 		/// Effets d'age
		i.year ///
		hearte hibpe				///
		diabe cancre  				/// Prevalence d'autres maladies
		smoker	former				/// Indicatrices fumeur, ancien fumeur
		obese veryobese				/// Indicatrice obese, tres obese
		sexe imm 					/// Caracteristiques socio-ecn
		hs college univ				///		
		quebec						///
		ontario						///
		prairies bc					/// indicatrice prov
		[pw=WT64LS]	 				///
		if  flag_hibpe_long==0 & flag_stroke_long==0 & flag_diabe_long==0 & ltc!=2  ,cluster(id)
ereturn list
		*logit rstroke i.sexe [pw=WT64LS] if  flag_hibpe_long==0 & flag_stroke_long==0 & flag_diabe_long==0 & ltc!=2  ,cluster(id)
*estimates store rem_stroke_pond 
		

*margins, dydx(*) post
*estimates store rem_stroke_pond_margin 

*estout rem_stroke_pond* ,cell(b(star fmt(%9.6f))) stats(N)
*gen wgt = round(WT64LS)
*tab rstroke sexe [w=wgt] if  flag_hibpe_long==0 & flag_stroke_long==0 & flag_diabe_long==0 & ltc!=2, col



//estout inc_stroke_pond rem_stroke_pond ,cell(b(star fmt(%9.5f))) stats(N)

//estout inc_stroke_pond_margin rem_stroke_pond_margin ,cell(b(star fmt(%9.5f))) stats(N)
restore
***************************************************************************
gen age45m = min(age,45)
gen age45p = max(age-45,0)
cloglog ilunge						///
		age45* 						/// Effets d'age
		i.year 							/// Prevalence d'autres maladies
		smoker	former				/// Indicatrices fumeur, ancien fumeur
		obese veryobese				/// Indicatrice obese, tres obese
		sexe imm 					/// Caracteristiques socio-ecn
		hs college univ				///		
		quebec						///
		ontario						///
		prairies bc					/// indicatrice prov		
		[pw=WT64LS]	 				///
		if  ltc!=2,cluster(id)	
ereturn list
		*margins, dydx(*) post	
*estimates store inc_lung_pond_margin 		
	
***************************************************************************
preserve 
drop if age < 55
gen age85m = min(age,85)
gen age85p = max(age-85,0)
gen year2010 = (year==2008)
cloglog imentae						///
		age85* 						/// Effets d'age
		i.year 			/// Prevalence d'autres maladies
		smoker	former				/// Indicatrices fumeur, ancien fumeur
		obese veryobese				/// Indicatrice obese, tres obesee
		sexe imm 					/// Caracteristiques socio-ecn
		hs college univ				///		
		quebec						///
		ontario						///
		prairies bc					/// indicatrice prov
		[pw=WT64LS]					///
		if  ltc!=2,cluster(id)
ereturn list				
*margins, dydx(*) post	
*estimates store inc_mental_pond_margin 		
restore
//estout inc_mental_pond inc_mental_pond_margin ,cell(b(star fmt(%9.5f))) stats(N)

***************************************************************************		
		
*estout inc_*_pond ,cell(b(star fmt(%9.6f))) stats(N)
*estout rem_*_pond ,cell(b(star fmt(%9.6f))) stats(N)

*estout inc_*_pond_margin ,cell(b(star fmt(%9.5f))) stats(N)
*estout rem_*_pond_margin ,cell(b(star fmt(%9.5f))) stats(N)
***********
*** IMC ***
***********

gen f_IMC = f.IMC
gen f_IMC_C = f.IMC_C
replace IMC_FEM = int(IMC_FEM)
gen f_IMC_FEM = f.IMC_FEM


***************************************************************************
mlogit  f_IMC						///
		age50* 						/// Effets d'age
		smoker	former				/// Indicatrices fumeur, ancien fumeur
		obese veryobese				/// Indicatrice obese, tres obese
		sexe imm 					/// Caracteristiques socio-ecn
		hs college univ				///		
		quebec						///
		ontario						///
		prairies bc					/// indicatrice prov
		[pw=WT64LS]	 				///
		if ltc!=2 ,cluster(id) 
ereturn list		
margins, dydx(*) post
estimates store imc_margin

estout imc, cell(b(star fmt(%9.6f))) stats(N)
/*
mlogit  f_IMC_FEM						///
		age50* 						/// Effets d'age
		smoker	former				/// Indicatrices fumeur, ancien fumeur
		IMC_C			/// Indicatrice obese, tres obese
		sexe imm 					/// Caracteristiques socio-ecn
		hs college univ				///		
		quebec						///
		ontario						///
		prairies bc					/// indicatrice prov
		[pw=WT64LS]	 				///
		,cluster(id) baseoutcome(0)
			
estimates store imc_fem
margins, dydx(*) post
estimates store imc_fem_margin

estout imc_fem_margin

predict IMC_P_FEM1-IMC_P_FEM10 if e(sample)
egen IMC_P_FEM_max =  rowmax(IMC_P_FEM1-IMC_P_FEM10)
gen IMC_P_FEM = .
forvalues i = 1/10 {
	replace IMC_P_FEM = 17.5+((`i'-1)*2.5) if IMC_P_FEM_max== IMC_P_FEM`i' & IMC_P_FEM_max != .
}

gen log_IMC = log(IMC_C)
gen log_f_IMC = log(f_IMC_C)

reg log_f_IMC ///
		age50* 						/// Effets d'age
		smoker	former				/// Indicatrices fumeur, ancien fumeur
		sexe imm 					/// Caracteristiques socio-ecn
		hs college univ				///		
		quebec						///
		ontario						///
		prairies bc					/// indicatrice prov
		log_IMC 	/// Indicatrice obese, tres obese
		[pw=WT64LS]	 				///
		,cluster(id) 
estimates store imc_c
margins, dydx(*) post
estimates store imc_c_margin
estout imc_c
*/
/*		
predict IMC_P_C if e(sample)
replace IMC_P_C = exp(IMC_P_C)
egen IMC_P = cut(IMC_P_C), at(0,20(2.5)40,90)
egen IMC_P_T = cut(f_IMC_C), at(0,20(2.5)40,90)
*/
*****************
*** mortalite ***
*****************

		
	cloglog ind_deces				///
		age50* 						/// Effets d'age
		diabe hibpe cancre hearte	///
		stroke lunge mentae 		/// Prevalence d'autres maladies
		smoker former 				/// Indicatrices fumeur, ancien fumeur
		obese veryobese				/// Indicatrice obese, tres obese
		sex imm 					/// Caractestiques socio-ecn
		hs college univ				///
		quebec ontario prairies bc	/// indicatrice provinces
		i.inv7 nhome						///
		[pw=WT64LS]	 				///
		if  flag_hibpe_long==0 & flag_stroke_long==0 & flag_diabe_long==0 & year!=2008,cluster(id)
ereturn list
*margins, dydx(*) post
*estimates store alive_margin
			
*estout alive alive_margin, cell(b(star fmt(%9.6f))) stats(N)
	
	
*****************	
*** tabagisme ***	
*****************
gen f_fumeur= f.fumeur
gen f_fum_arret = (f_fumeur==2) if fumeur==1
gen f_fum_debut = (f_fumeur==1) if fumeur==0
gen f_fum_recom  = (f_fumeur==1) if fumeur==2


cloglog f_fum_debut				///
		age50*					///
		obese veryobese			/// Indicatrice obese,tres obese
		sex imm 				/// Caracteristiques socio-ecn
		hs college univ			///
		quebec ontario			///
		prairies bc 			/// indicatrice qc
		[pw=WT64LS]	 			///
		if ltc!=2,cluster(id)
ereturn list	
margins, dydx(*) post		
estimates store fum_debut_margin	
		
cloglog f_fum_arret				///
		age50*					///
		obese veryobese			/// Indicatrice obese, tres obese
		sex imm 				/// Caracteristiques socio-ecn
		hs college univ			///
		quebec ontario			///
		prairies bc 			/// indicatrice qc
		[pw=WT64LS]	 			///
		if  ltc!=2,cluster(id)
ereturn list
margins, dydx(*) post	
estimates store fum_arret_margin
	
cloglog f_fum_recom				///
		age50*					///
		obese veryobese			/// Indicatrice obese, tres obese
		sex imm 				/// Caracteristiques socio-ecn
		hs college univ			///
		quebec ontario			///
		prairies bc 			/// indicatrice qc
		[pw=WT64LS]	 			///
		if  ltc!=2,cluster(id)
 ereturn list
margins, dydx(*) post	
estimates store fum_recom_margin
		
estout fum_* , cell(b(star fmt(%9.6f))) stats(N)


**************
*** Revenu ***
**************


gen f_income = f.income

mlogit  f_income			///
		age50*  				/// Effets d'ege
		sexe imm 				/// Caracteristiques socio-ecn
		hs college univ			///
		i.income				/// revenu
		quebec					///
		ontario					///
		prairies bc				/// indicatrice prov
		[pw=WT64LS]	 			///
		if  ltc!=2,cluster(id) 
ereturn list
*margins, dydx(*) post
*estout income, drop(1b*)

capture log close


