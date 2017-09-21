*transitions entre les 3 categories de soins de sante

clear all
clear matrix
capture log close

cd "T:\Projet 3985-S003\compas\Transitions pour PLOS ONE"
set maxvar 10000
set more off
use "data\clean_CCHS_plos_one.dta", replace


gen boot_weight=0
global n_rep=300
matrix ltc=J($n_rep,84,0)
matrix ltc_conv=J($n_rep,1,0)
matrix inv=J($n_rep,210,0)
matrix inv_conv=J($n_rep,1,0)
matrix idiabe=J($n_rep,24,0)
matrix idiabe_conv=J($n_rep,1,0)
matrix ihibpe=J($n_rep,25,0)
matrix ihibpe_conv=J($n_rep,1,0)
matrix icancre=J($n_rep,24,0)
matrix icancre_conv=J($n_rep,1,0)
matrix rcancre=J($n_rep,24,0)
matrix rcancre_conv=J($n_rep,1,0)
matrix ihearte=J($n_rep,26,0)
matrix ihearte_conv=J($n_rep,1,0)
matrix rhearte=J($n_rep,26,0)
matrix rhearte_conv=J($n_rep,1,0)
matrix istroke=J($n_rep,28,0)
matrix istroke_conv=J($n_rep,1,0)
matrix rstroke=J($n_rep,28,0)
matrix rstroke_conv=J($n_rep,1,0)
matrix  ilunge=J($n_rep,24,0)
matrix ilunge_conv=J($n_rep,1,0)
matrix  imentae=J($n_rep,24,0)
matrix imentae_conv=J($n_rep,1,0)
matrix  imc=J($n_rep,48,0)
matrix imc_conv=J($n_rep,1,0)
matrix  mortalite=J($n_rep,31,0)
matrix mortalite_conv=J($n_rep,1,0)
matrix fum_deb=J($n_rep,14,0)
matrix fum_deb_conv=J($n_rep,1,0)
matrix fum_arret=J($n_rep,14,0)
matrix fum_arret_conv=J($n_rep,1,0)
matrix fum_recom=J($n_rep,14,0)
matrix fum_recom_conv=J($n_rep,1,0)
matrix revenu=J($n_rep,85,0)
matrix revenu_conv=J($n_rep,1,0)
gen f_ltc = f.ltc
gen f_inv7 = f.inv7
gen age60m = min(age,60)
gen age60p = max(age-60,0)
gen age85m = min(age,85)
gen age85p = max(age-85,0)
gen age45m = min(age,45)
gen age45p = max(age-45,0)
gen year2010 = (year==2008)
gen f_IMC = f.IMC
gen f_IMC_C = f.IMC_C
replace IMC_FEM = int(IMC_FEM)
gen f_IMC_FEM = f.IMC_FEM
gen f_fumeur= f.fumeur
gen f_fum_arret = (f_fumeur==2) if fumeur==1
gen f_fum_debut = (f_fumeur==1) if fumeur==0
gen f_fum_recom  = (f_fumeur==1) if fumeur==2
gen f_income = f.income
set seed 1
by REALUKEY, sort: gen nvals=_n==1
count if nvals
*************************************************
*****************REGRESSIONS*********************
*************************************************
forvalue i=1/$n_rep {
sort REALUKEY
bsample 14939, cluster(REALUKEY) weight(boot_weight)
replace boot_weight=boot_weight*WT64LS


qui mlogit  f_ltc					///
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
		[pw=boot_weight]	 			///
		if  flag_hibpe_long==0 & flag_stroke_long==0 & flag_diabe_long==0 & ltc!=2 ///
	  ,cluster(id) iterate(50)	
matrix ltc[`i',1]=e(b)
matrix ltc_conv[`i',1]=e(converged)
if `i'==1{
#delimit ;
local rname= "age50m0 age50p0 diabe0 hibpe0 cancre0 hearte0 stroke0 lunge0 mentae0 smoker0 former0 obese0 veryobese0 sexe0 imm0 hs0 college0 univ0 ltc1_0 ltc2_0 adl0 iadl0 cognitive0 quebec0 ontario0 prairies0 bc0 constant0
			  age50m1 age50p1 diabe1 hibpe1 cancre1 hearte1 stroke1 lunge1 mentae1 smoker1 former1 obese1 veryobese1 sexe1 imm1 hs1 college1 univ1 ltc1_1 ltc2_1 adl1 iadl1 cognitive1 quebec1 ontario1 prairies1 bc1 constant1
			  age50m2 age50p2 diabe2 hibpe2 cancre2 hearte2 stroke2 lunge2 mentae2 smoker2 former2 obese2 veryobese2 sexe2 imm2 hs2 college2 univ2 ltc1_2 ltc2_2 adl2 iadl2 cognitive2 quebec2 ontario2 prairies2 bc2 constant2";
#delimit cr
matrix colnames ltc=`rname'
}
qui mlogit  f_inv7					///
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
		[pw=boot_weight]	 			///
		if  flag_hibpe_long==0 & flag_stroke_long==0 & flag_diabe_long==0 & ltc!=2  ,cluster(id) iterate(50)	
matrix inv[`i',1]=e(b)
matrix inv_conv[`i',1]=e(converged)
if `i'==1{
#delimit ;
local rname= "age50m1 age50p1 diabe1 hibpe1 cancre1 hearte1 stroke1 lunge1 mentae1 smoker1 former1 obese1 veryobese1 sexe1 imm1 hs1 college1 univ1 inv1_1 inv2_1 inv3_1 inv4_1 inv5_1 inv6_1 inv7_1 quebec1 ontario1 prairies1 bc1 constant1	  
			  age50m2 age50p2 diabe2 hibpe2 cancre2 hearte2 stroke2 lunge2 mentae2 smoker2 former2 obese2 veryobese2 sexe2 imm2 hs2 college2 univ2 inv1_2 inv2_2 inv3_2 inv4_2 inv5_2 inv6_2 inv7_2 quebec2 ontario2 prairies2 bc2 constant2
			  age50m3 age50p3 diabe3 hibpe3 cancre3 hearte3 stroke3 lunge3 mentae3 smoker3 former3 obese3 veryobese3 sexe3 imm3 hs3 college3 univ3 inv1_3 inv2_3 inv3_3 inv4_3 inv5_3 inv6_3 inv7_3 quebec3 ontario3 prairies3 bc3 constant3
			  age50m4 age50p4 diabe4 hibpe4 cancre4 hearte4 stroke4 lunge4 mentae4 smoker4 former4 obese4 veryobese4 sexe4 imm4 hs4 college4 univ4 inv1_4 inv2_4 inv3_4 inv4_4 inv5_4 inv6_4 inv7_4 quebec4 ontario4 prairies4 bc4 constant4
			  age50m5 age50p5 diabe5 hibpe5 cancre5 hearte5 stroke5 lunge5 mentae5 smoker5 former5 obese5 veryobese5 sexe5 imm5 hs5 college5 univ5 inv1_5 inv2_5 inv3_5 inv4_5 inv5_5 inv6_5 inv7_5 quebec5 ontario5 prairies5 bc5 constant5
			  age50m6 age50p6 diabe6 hibpe6 cancre6 hearte6 stroke6 lunge6 mentae6 smoker6 former6 obese6 veryobese6 sexe6 imm6 hs6 college6 univ6 inv1_6 inv2_6 inv3_6 inv4_6 inv5_6 inv6_6 inv7_6 quebec6 ontario6 prairies6 bc6 constant6
			  age50m7 age50p7 diabe7 hibpe7 cancre7 hearte7 stroke7 lunge7 mentae7 smoker7 former7 obese7 veryobese7 sexe7 imm7 hs7 college7 univ7 inv1_7 inv2_7 inv3_7 inv4_7 inv5_7 inv6_7 inv7_7 quebec7 ontario7 prairies7 bc7 constant7";
#delimit cr
matrix colnames inv=`rname'
}

qui cloglog idiabe					///
		age60* 					/// Effets d'age
		i.year ///
		smoker	former			/// Indicatrices fumeur, ancien fumeur
		obese veryobese			/// Indicatrice obese, tres obese
		sexe imm 				/// Caracteristiques socio-ecn
		hs college univ			///		
		quebec					///
		ontario					///
		prairies bc				/// indicatrice prov
		[pw=boot_weight]	 			///
		if  flag_diabe_long==0 & ltc!=2 ,cluster(id) iterate(50)	
matrix idiabe[`i',1]=e(b)
if `i'==1{
local rname:colnames e(b)
matrix colnames idiabe=`rname'
matname idiabe constant, col(24) explicit
}
qui cloglog ihibpe					///
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
		[pw=boot_weight]				///
		if  flag_hibpe_long==0 & flag_diabe_long==0 & ltc!=2 ,cluster(id) iterate(50)	
matrix ihibpe[`i',1]=e(b)
if `i'==1{
local rname:colnames e(b)
matrix colnames ihibpe=`rname'
matname ihibpe constant, col(25) explicit
}
qui cloglog icancre					///
		age60* 					/// Effets d'age
		i.year ///
		smoker	former			/// Indicatrices fumeur, ancien fumeur
		obese veryobese			/// Indicatrice obese, tres obese
		sexe imm 				/// Caracteristiques socio-ecn
		hs college univ			///		
		quebec					///
		ontario					///
		prairies bc				/// indicatrice prov
		[pw=boot_weight]	 			///
		if  ltc!=2,cluster(id) iterate(50)	
matrix icancre[`i',1]=e(b)
if `i'==1{
local rname:colnames e(b)
matrix colnames icancre=`rname'
matname icancre constant, col(24) explicit
}
qui logit rcancre					///
		age60* 					/// Effets d'age
		i.year						/// Prevalence d'autres maladies
		smoker	former			/// Indicatrices fumeur, ancien fumeur
		obese veryobese			/// Indicatrice obese, tres obese
		sexe imm 				/// Caracteristiques socio-ecn
		hs college univ			///		
		quebec					///
		ontario					///
		prairies bc				/// indicatrice prov
		[pw=boot_weight]	 			///
		if  ltc!=2,cluster(id)	iterate(50)	
matrix rcancre[`i',1]=e(b)
if `i'==1{
local rname:colnames e(b)
matrix colnames rcancre=`rname'
matname rcancre constant, col(24) explicit
}
qui cloglog ihearte						///
		age60* 						/// Effets d'age
		i.year ///
		hibpe diabe 				/// Prevalence d'autres maladies
		smoker	former				/// Indicatrices fumeur, ancien fumeur
		obese veryobese				/// Indicatrice obese, tres obese
		sexe imm 					/// Caracteristiques socio-ecn
		hs college univ				///		
		quebec						///
		ontario						///
		prairies bc					/// indicatrice prov		
		[pw=boot_weight]	 ///
		if  flag_hibpe_long==0 & flag_diabe_long==0  & ltc!=2,cluster(id) iterate(50)		
matrix ihearte[`i',1]=e(b)
if `i'==1{
local rname:colnames e(b)
matrix colnames ihearte=`rname'
matname ihearte constant, col(26) explicit
}
qui logit rhearte						///
		age60* 						/// Effets d'age
		i.year ///
		hibpe diabe 				/// Prevalence d'autres maladies
		smoker	former				/// Indicatrices fumeur, ancien fumeur
		obese veryobese				/// Indicatrice obese, tres obese
		sexe imm 					/// Caracteristiques socio-ecn
		hs college univ				///		
		quebec						///
		ontario						///
		prairies bc					/// indicatrice prov	
		[pw=boot_weight]	 ///
		if  flag_hibpe_long==0 & flag_diabe_long==0  & ltc!=2,cluster(id) iterate(50)	
matrix rhearte[`i',1]=e(b)
if `i'==1{
local rname:colnames e(b)
matrix colnames rhearte=`rname'
matname rhearte constant, col(26) explicit
}
preserve 
drop if age < 40

qui cloglog istroke						///
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
		[pw=boot_weight]	 				///
		if  flag_hibpe_long==0 & flag_stroke_long==0 & flag_diabe_long==0  & ltc!=2,cluster(id)	iterate(50)	
matrix istroke[`i',1]=e(b)
if `i'==1{
local rname:colnames e(b)
matrix colnames istroke=`rname'
matname istroke constant, col(28) explicit
}
qui logit rstroke						///
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
		[pw=boot_weight]	 				///
		if  flag_hibpe_long==0 & flag_stroke_long==0 & flag_diabe_long==0 & ltc!=2  ,cluster(id) iterate(50)	
matrix rstroke[`i',1]=e(b)
if `i'==1{
local rname:colnames e(b)
matrix colnames rstroke=`rname'
matname rstroke constant, col(28) explicit
}
restore

qui cloglog ilunge						///
		age45* 						/// Effets d'age
		i.year 							/// Prevalence d'autres maladies
		smoker	former				/// Indicatrices fumeur, ancien fumeur
		obese veryobese				/// Indicatrice obese, tres obese
		sexe imm 					/// Caracteristiques socio-ecn
		hs college univ				///		
		quebec						///
		ontario						///
		prairies bc					/// indicatrice prov		
		[pw=boot_weight]	 				///
		if  ltc!=2,cluster(id) iterate(50)	
matrix ilunge[`i',1]=e(b)
if `i'==1{
local rname:colnames e(b)
matrix colnames ilunge=`rname'
matname ilunge constant, col(24) explicit
}
preserve 
drop if age < 55
qui cloglog imentae						///
		age85* 						/// Effets d'age
		i.year 			/// Prevalence d'autres maladies
		smoker	former				/// Indicatrices fumeur, ancien fumeur
		obese veryobese				/// Indicatrice obese, tres obesee
		sexe imm 					/// Caracteristiques socio-ecn
		hs college univ				///		
		quebec						///
		ontario						///
		prairies bc					/// indicatrice prov
		[pw=boot_weight]					///
		if  ltc!=2,cluster(id) iterate(50)	
matrix imentae[`i',1]=e(b)
if `i'==1{
local rname:colnames e(b)
matrix colnames imentae=`rname'
matname imentae constant, col(24) explicit
}
restore


qui mlogit  f_IMC						///
		age50* 						/// Effets d'age
		smoker	former				/// Indicatrices fumeur, ancien fumeur
		obese veryobese				/// Indicatrice obese, tres obese
		sexe imm 					/// Caracteristiques socio-ecn
		hs college univ				///		
		quebec						///
		ontario						///
		prairies bc					/// indicatrice prov
		[pw=boot_weight]	 				///
		if ltc!=2 ,cluster(id) iterate(50)	
matrix imc[`i',1]=e(b)
matrix imc_conv[`i',1]=e(converged)
if `i'==1{
#delimit ;
local rname= "age50m0 age50p0 smoker0 former0 obese0 veryobese0 sexe0 imm0 hs0 college0 univ0 quebec0 ontario0 prairies0 bc0 constant0	  
			  age50m1 age50p1 smoker1 former1 obese1 veryobese1 sexe1 imm1 hs1 college1 univ1 quebec1 ontario1 prairies1 bc1 constant1
			  age50m2 age50p2 smoker2 former2 obese2 veryobese2 sexe2 imm2 hs2 college2 univ2 quebec2 ontario2 prairies2 bc2 constant2";
#delimit cr
matrix colnames imc=`rname'
}
qui cloglog ind_deces				///
		age50* 						/// Effets d'age
		diabe hibpe cancre hearte	///
		stroke lunge mentae 		/// Prevalence d'autres maladies
		smoker former 				/// Indicatrices fumeur, ancien fumeur
		obese veryobese				/// Indicatrice obese, tres obese
		sex imm 					/// Caractestiques socio-ecn
		hs college univ				///
		quebec ontario prairies bc	/// indicatrice provinces
		i.inv7 nhome						///
		[pw=boot_weight]	 				///
		if  flag_hibpe_long==0 & flag_stroke_long==0 & flag_diabe_long==0 & year!=2008,cluster(id) iterate(50)	
matrix mortalite[`i',1]=e(b)
if `i'==1{
local rname:colnames e(b)
matrix colnames mortalite=`rname'
matname mortalite constant, col(31) explicit
}



qui cloglog f_fum_debut				///
		age50*					///
		obese veryobese			/// Indicatrice obese,tres obese
		sex imm 				/// Caracteristiques socio-ecn
		hs college univ			///
		quebec ontario			///
		prairies bc 			/// indicatrice qc
		[pw=boot_weight]	 			///
		if ltc!=2,cluster(id) iterate(50)	
matrix fum_deb[`i',1]=e(b)
if `i'==1{
local rname:colnames e(b)
matrix colnames fum_deb=`rname'
matname fum_deb constant, col(14) explicit
}
qui cloglog f_fum_arret				///
		age50*					///
		obese veryobese			/// Indicatrice obese, tres obese
		sex imm 				/// Caracteristiques socio-ecn
		hs college univ			///
		quebec ontario			///
		prairies bc 			/// indicatrice qc
		[pw=boot_weight]	 			///
		if  ltc!=2,cluster(id) iterate(50)	
matrix fum_arret[`i',1]=e(b)
if `i'==1{
local rname:colnames e(b)
matrix colnames fum_arret=`rname'
matname fum_arret constant, col(14) explicit
}
qui cloglog f_fum_recom				///
		age50*					///
		obese veryobese			/// Indicatrice obese, tres obese
		sex imm 				/// Caracteristiques socio-ecn
		hs college univ			///
		quebec ontario			///
		prairies bc 			/// indicatrice qc
		[pw=boot_weight]	 			///
		if  ltc!=2,cluster(id) iterate(50)	
matrix fum_recom[`i',1]=e(b)
if `i'==1{
local rname:colnames e(b)
matrix colnames fum_recom=`rname'
matname fum_recom constant, col(14) explicit
}


qui mlogit  f_income			///
		age50*  				/// Effets d'ege
		sexe imm 				/// Caracteristiques socio-ecn
		hs college univ			///
		i.income				/// revenu
		quebec					///
		ontario					///
		prairies bc				/// indicatrice prov
		[pw=boot_weight]	 			///
		if  ltc!=2,cluster(id) iterate(50)	
matrix revenu[`i',1]=e(b)
matrix revenu_conv[`i',1]=e(converged)
if `i'==1{
#delimit ;
local rname= "age50m1 age50p1 sexe1 imm1 hs1 college1 univ1 income1_1 income2_1 income3_1 income4_1 income5_1 quebec1 ontario1 prairies1 bc1 constant1	  
			  age50m2 age50p2 sexe2 imm2 hs2 college2 univ2 income1_2 income2_2 income3_2 income4_2 income5_2 quebec2 ontario2 prairies2 bc2 constant2
			  age50m3 age50p3 sexe3 imm3 hs3 college3 univ3 income1_3 income2_3 income3_3 income4_3 income5_3 quebec3 ontario3 prairies3 bc3 constant3
			  age50m4 age50p4 sexe4 imm4 hs4 college4 univ4 income1_4 income2_4 income3_4 income4_4 income5_4 quebec4 ontario4 prairies4 bc4 constant4
			  age50m5 age50p5 sexe5 imm5 hs5 college5 univ5 income1_5 income2_5 income3_5 income4_5 income5_5 quebec5 ontario5 prairies5 bc5 constant5";
#delimit cr
matrix colnames revenu=`rname'
}
}
forvalues i=3/10{
matname idiabe year_`i', columns(`i') explicit 
}

forvalues i=3/10{
matname ihibpe year_`i', columns(`i') explicit 
}

forvalues i=3/10{
matname icancre year_`i', columns(`i') explicit 
}
forvalues i=3/10{
matname rcancre year_`i', columns(`i') explicit 
}
forvalues i=3/10{
matname ihearte year_`i', columns(`i') explicit 
}
forvalues i=3/10{
matname rhearte year_`i', columns(`i') explicit 
}
forvalues i=3/10{
matname istroke year_`i', columns(`i') explicit 
}
forvalues i=3/10{
matname rstroke year_`i', columns(`i') explicit 
}
forvalues i=3/10{
matname ilunge year_`i', columns(`i') explicit 
}
forvalues i=3/10{
matname imentae year_`i', columns(`i') explicit 
}
forvalues i=23/29{
matname mortalite inv_`i', columns(`i') explicit 
}


*clear
*local rname: colnames ltc
*svmat ltc, names(var)
*local i=1
*foreach name in `rname'{
*di "`name'"
*rename var`i' `name'
*local i=`i'+1
*} 

clear
svmat ltc, names(col)
saveold "data\ltc_coef.dta", replace version(13) 
clear
svmat ltc_conv, names(col)
saveold "data\ltc_conv.dta", replace version(13)
clear

clear
svmat inv, names(col)
saveold "data\inv_coef.dta", replace  version(13)
clear
svmat inv_conv, names(col)
saveold "data\inv_conv.dta", replace version(13)
clear

svmat idiabe, names(col)
saveold "data\idiabe_coef.dta", replace version(13)
clear
*svmat idiabe_conv, names(col)
*save "data\idiabe_conv.dta", replace
*clear

svmat ihibpe, names(col)
saveold "data\ihipbe_coef.dta", replace  version(13) 
clear
*svmat ihibpe_conv, names(col)
*save "data\ihibpe_conv.dta", replace
*clear

svmat icancre, names(col)
saveold "data\icancre_coef.dta", replace  version(13)
clear
*svmat icancre_conv, names(col)
*save "data\icancre_conv.dta", replace
*clear

svmat rcancre, names(col)
saveold "data\rcancre_coef.dta", replace  version(13)
clear
*svmat rcancre_conv, names(col)
*save "data\rcancre_conv.dta", replace
*clear

svmat ihearte, names(col)
saveold "data\ihearte_coef.dta", replace  version(13)
clear
*svmat ihearte_conv, names(col)
*save "data\ihearte_conv.dta", replace
*clear

svmat rhearte, names(col)
saveold "data\rhearte_coef.dta", replace  version(13)
clear
*svmat rhearte_conv, names(col)
*save "data\rhearte_conv.dta", replace
*clear


svmat istroke, names(col)
saveold "data\istroke_coef.dta", replace  version(13)
clear
*svmat istroke_conv, names(col)
*save "data\istroke_conv.dta", replace
*clear

svmat rstroke, names(col)
saveold "data\rstroke_coef.dta", replace  version(13)
clear
*svmat rstroke_conv, names(col)
*save "data\rstroke_conv.dta", replace
*clear

svmat ilunge, names(col)
saveold "data\ilunge_coef.dta", replace  version(13)
clear
*svmat ilunge_conv, names(col)
*save "data\ilunge_conv.dta", replace
*clear

svmat imentae, names(col)
saveold "data\imentae_coef.dta", replace  version(13)
clear
*svmat imentae_conv, names(col)
*save "data\imentae_conv.dta", replace
*clear

svmat imc, names(col)
saveold "data\imc_coef.dta", replace  version(13)
clear
svmat imc_conv, names(col)
saveold "data\imc_conv.dta", replace version(13)
clear

svmat mortalite, names(col)
saveold "data\mortalite_coef.dta", replace  version(13)
clear
*svmat mortalite_conv, names(col)
*save "data\mortalite_conv.dta", replace
*clear

svmat fum_deb, names(col)
saveold "data\fum_deb_coef.dta", replace  version(13)
clear
*svmat fum_deb_conv, names(col)
*save "data\fum_deb_conv.dta", replace
*clear

svmat fum_arret, names(col)
saveold "data\fum_arret_coef.dta", replace  version(13)
clear
*svmat fum_arret_conv, names(col)
*save "data\fum_arret_conv.dta", replace
*clear

svmat fum_recom, names(col)
saveold "data\fum_recom_coef.dta", replace  version(13)
clear
*svmat fum_recom_conv, names(col)
*save "data\fum_recom_conv.dta", replace
*clear

svmat revenu, names(col)
saveold "data\revenu_coef.dta", replace  version(13)
*clear
*svmat revenu_conv, names(col)
*save "data\revenu_conv.dta", replace

