*Classement des facteurs de risque 


***STATUT DE FUMEUR - 3 NIVEUX:  0 = JAMAIS;  1= FUMEUR REG;  2 = ANCIEN FUMEUR
********************************************************************************



 scalar start=1994
 scalar stop=2010
local t=start

foreach x of var SMC?_2 { // etat du fumeur

gen fumeur`t' = (`x'==1) if  status`t'!=2  // 1 si fume tous les jours, 0 sinon

replace fumeur`t'=. if `x'>=6 & `x'<=9 //les SO, NSP, ND et R 

local `t'=`++t'
local `t'=`++t' //Deux ans entre les cycles
}

*Anciens fumeurs
local t=start

foreach x of var SMC?_5 { // A deja fume tous les jours

replace fumeur`t' = 2 if `x'==1 & status`t'!=2  // 2 si ancien fumeur
local `t'=`++t'
local `t'=`++t' //Deux ans entre les cycles
}

*Correction pour ceux qui passent de jamais fume a ancien fumeur (reponses incoherentes) -On suppose qu'ils ont ete fumeurs reguliers une courte periode juste avant l'enquete
forvalue t=`=start'(2)`=stop-2'{
local t1 = `t'+2
replace fumeur`t1' = 1 if fumeur`t'==0 & fumeur`t1' ==2 & status`t1'!=2
}

*Correction pour ceux qui passent de fumeur a jamais fume: rendus anciens fumeurs
forvalue t=`=start'(2)`=stop-2'{
local t1 = `t'+2
replace fumeur`t1' = 2 if fumeur`t'==1 & fumeur`t1' ==0 & status`t1'!=2
}


*Correction pour ceux qui passent de ancien fumeur a jamais fume: rendus anciens fumeurs
forvalue t=`=start'(2)`=stop-2'{
local t1 = `t'+2
replace fumeur`t1' = 2 if fumeur`t'==2 & fumeur`t1' ==0 & status`t1'!=2
}

*Correction pour ceux qui ont jamais fume a t et missing a t-2
 scalar start=2008
 scalar stop=1996
forvalue t=`=start'(-2)`=stop-2'{
local t1 = `t'+2
local t0=`t'-2
replace fumeur`t'=0 if fumeur`t1'==0 &  fumeur`t'==. & status`t1'!=2
}

***************************************************************************
*************** IMC - 3 niveaux <30 ; entre 30 et 35 ; >35   **************
***************************************************************************


 scalar start=1994
 scalar stop=2010
local t=start

foreach x of var HWC?IBMI { // 

gen IMC`t'		= 0	if `x'< 30 &  status`t'!=2  
replace IMC`t'	= 1 if `x'>=30 & `x'<=35 & status`t'!=2 
replace IMC`t'	= 2 if `x'>35 & `x'<99 & status`t'!=2 

replace IMC`t'=. if `x'>=99.96 //les SO, ND, NSP et R 
*IMC continue
gen IMC_C`t' = `x' if   status`t'!=2 
replace IMC_C`t'=. if `x'>=99 //les SO, ND, NSP et R 
egen IMC_FEM`t' = cut(`x') if  status`t'!=2 , at(0,20(2.5)40,98.5)

local `t'= `++t'					
local `t'= `++t' 

}

 *Correction necessaire pour ceux qui n'ont pas rendu a t, mais qui ont repondu a t-2 et t+2
 *Leur met la reponse de t-2
scalar start=1996
scalar stop=2008
forvalue t=`=start'(2)`=stop-2'{
local t1 = `t'+2
local t0=`t'-2
replace fumeur`t' = fumeur`t0' if fumeur`t'==. &  fumeur`t0' !=. & fumeur`t1' !=. & status`t1'!=2
replace IMC`t' = IMC`t0' if IMC`t'==. &  IMC`t0' !=. & IMC`t1' !=. & status`t1'!=2
replace IMC_C`t' = IMC_C`t0' if IMC_C`t'==. &  IMC_C`t0' !=. & IMC_C`t1' !=. & status`t1'!=2
replace IMC_FEM`t' = IMC_FEM`t0' if IMC_FEM`t'==. &  IMC_FEM`t0' !=. & IMC_FEM`t1' !=. & status`t1'!=2

}


*Correction necessaire pour les gens en institution: gardent leur reponse la plus recente

scalar start=1994
scalar stop=2010
forvalue t=`=start'(2)`=stop-2'{
local t1=`t'+2
replace fumeur`t1' = fumeur`t' if fumeur`t1'==. &  fumeur`t' !=. & status`t1'==3
replace IMC`t1' = IMC`t' if IMC`t1'==. &  IMC`t' !=. & status`t1'==3
replace IMC_C`t1' = IMC_C`t' if IMC_C`t1'==. &  IMC_C`t' !=. & status`t1'==3
replace IMC_C`t1' = IMC_C`t' if IMC_C`t1'==. &  IMC_C`t' !=. & status`t1'==3

}


