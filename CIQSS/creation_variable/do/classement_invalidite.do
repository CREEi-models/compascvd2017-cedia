*Classement de l'invalidite - Steeve Marchand 



*************************************************************
*** On veut une dichotomique pour les problemes cognitifs ***
*************************************************************
scalar start=1994
scalar stop=2010
local t=start
foreach x of var HSC?_26 { // 

gen cognitive`t' = (`x'>=3 & `x'<6) if status`t'!=2  	// Probleme de memoire : Tres porte a oublier ou incapable de se rappeler
replace cognitive`t'=. if `x'>=6 & `x'<=9

local `t'= `++t'
local `t'= `++t' 
}


************************************************************************
*** On veut une dichotomique pour les ADL (Activity of Daily Living) ***
************************************************************************
scalar start=1994
scalar stop=2010
local t=start

foreach x of var  RAC?_6E {  // a besoin d'aide pour les soins personels 
gen adl`t' =0 if  status`t'!=2
replace adl`t'=1 if `x'==1
replace adl`t'=. if `x'>=6 & `x'<=9 //NSP, R, ND et SO a manquantes

local `t'= `++t' 
local `t'= `++t' 
}


local t=start

foreach x of var  RAC?_6F {  // a besoin d'aide pour se deacer dans la maison
replace adl`t' = 1 if  `x' ==1  & status`t'!=2 //remplace adl par 1 si cette autre variable de limitation est a oui
replace adl`t' = 0 if  `x' ==2  & status`t'!=2 & adl`t'==. //remplace adl par 0 si cette autre variable de limitation est a non et qu'auparavant elle etait manquante

//il ne reste plus de corrections a faire pour NSP, R, ND et SO, ceux qui doivent l'etre le sont deja

local `t'= `++t' 
local `t'= `++t' 
}



*******************************************************************************************
*** On veut une variable 0,1,2+ pour les IADL (Instrumental Activities of Daily Living) ***
*******************************************************************************************
scalar start=1994
scalar stop=2010
local t=start


foreach x of var  RAC?_6A {  // a besoin d'aide pour prerer les repas 
gen iadl`t' =0 if  status`t'!=2
replace iadl`t' = (iadl`t') + 1 if  `x' ==1  & status`t'!=2
replace iadl`t'=. if `x'>=6 & `x'<=9  //NSP, R, ND et SO a manquantes


local `t'= `++t' 
local `t'= `++t' 
}

local t=start

foreach x of var  RAC?_6B {  // a besoin d'aide pour faire les courses 
replace iadl`t' = (iadl`t') + 1 if  `x' ==1  & status`t'!=2
replace iadl`t'=0 if `x'==2 & iadl`t'==. //remplace iadl par 0 si cette autre variable de limitation est a non et qu'auparavant elle etait manquante

local `t'= `++t' 
local `t'= `++t' 
}
local t=start

foreach x of var  RAC?_6C {  // a besoin d'aide pour faire les taches menageres 
replace iadl`t' = (iadl`t') + 1 if  `x' ==1  & status`t'!=2
replace iadl`t'=0 if `x'==2 & iadl`t'==. //remplace iadl par 0 si cette autre variable de limitation est a non et qu'auparavant elle etait manquante


local `t'= `++t' 
local `t'= `++t' 
}


*Veut avoir comme derniere valeur 1+ limitations dans les iadl
forvalue t=`=start'(2)`=stop'{ 
recode iadl`t' (1/3=1)
}


*************************************************************************
*** On veut une variable continue pour les HUI (Health Utility Index) ***
*************************************************************************

local t=start
foreach x of var HSC?DHSI { 
gen hui`t' = `x' if  status`t'!=2 
replace hui`t'=. if `x'>=1 

local `t'=`++t'
local `t'=`++t'  
}


***********************************
*** correction pour non reponse ***
***********************************

 *Correction necessaire pour ceux qui n'ont pas repondu a, mais qui ont repondu a t-2 et t+2
 *Leur met la reponse de t-2
scalar start=1996
scalar stop=2008
forvalue t=`=start'(2)`=stop-2'{
local t1 = `t'+2
local t0=`t'-2
replace cognitive`t' = cognitive`t0' if cognitive`t'==. &  cognitive`t0' !=. & cognitive`t1' !=. & status`t1'!=2
replace adl`t' = adl`t0' if adl`t'==. &  adl`t0' !=. & adl`t1' !=. & status`t1'!=2
replace iadl`t' = iadl`t0' if iadl`t'==. &  iadl`t0' !=. & iadl`t1' !=. & status`t1'!=2

}



 *Correction necessaire pour les gens en institution: gardent rense plus recente
scalar start=1994
scalar stop=2010
forvalue t=`=start'(2)`=stop-2'{
local t1=`t'+2
replace cognitive`t1' = cognitive`t' if cognitive`t1'==. &  cognitive`t' !=. & status`t1'==3
replace adl`t1' = adl`t' if adl`t1'==. &  adl`t' !=. & status`t1'==3
replace iadl`t1' = iadl`t' if iadl`t1'==. &  iadl`t' !=. & status`t1'==3

}

