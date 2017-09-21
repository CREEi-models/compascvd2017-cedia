**********************************************************
*** indicateur soins/aide a domicile (oui/non) ***
**********************************************************

*Comme la question concernant l'aide non payee par le gouvernement commence seulement en 2002 (cycle5), on commence a cette date
scalar start2 = 2002
scalar stop2  = 2010

 
 local t=start2
foreach x of var HCC2_9  HCCA_9  HCCB_9  HCCC_9  HCCD_9 { 
gen homecare`t' = (`x'==1)	if  status`t'!=2 
replace homecare`t'=. if `x'>=6 & `x' <=9 

local `t'=`++t'
local `t'=`++t'  
}


local t=start2
foreach x of var HCC?_11A  { 
replace homecare`t' = 1	if `x'==1 & status`t'!=2 
replace homecare`t'=0 if `x'==2 & `x'==.

local `t'=`++t'
local `t'=`++t'  
}


*************************************
*** Soins/aide a domicile formels ***
*************************************

/* 
Comme la question concernant l'aide non payee par le gouvernement commence seulement en 2002 (cycle5), on commence a cette date

12 mois
Variable composee de la variable de services a domicile rendus payes en partie ou totalement par le gouvernement et de celles ou le gouvernement n'a rien paye
mais l'aide provient d'agences privees (HCC?_12A pour infirmiere et HCCC?_12B pour entretien menager). Ce n'est pas complet, mais c'est le mieux possible avec l'ENSP
SO(6) sont a missing car en partie les moins de 18 ans (mais j'aurais pu les mettre a 0) et en partie les gens en institution ***/

 local t=start2
foreach x of var HCC2_9  HCCA_9  HCCB_9  HCCC_9  HCCD_9 { 
gen homecare_f`t' = (`x'==1)	if  status`t'!=2 
replace homecare_f`t'=. if `x'>=6 & `x' <=9 

local `t'=`++t'
local `t'=`++t'  
}

local t=start2
foreach x of var HCC?_12A { 
replace homecare_f`t' =1 if  `x'==1 & status`t'!=2 
replace homecare_f`t'=0 if `x'==2 & status`t'!=2  & homecare_f`t'==.


local `t'=`++t'
local `t'=`++t'  
}

local t=start2
foreach x of var HCC?_12B { 
replace homecare_f`t' =1 if  `x'==1 & status`t'!=2 
replace homecare_f`t'=0 if `x'==2 & status`t'!=2  & homecare_f`t'==.


local `t'=`++t'
local `t'=`++t'  
}
 
 


 
 
 
 *************************************
*** Soins/aide a domicile informels ***
*************************************

/* 
12 mois
Commence seulement au cycle 5 (2002). Avant ce cycle, seulement des services a domicile payes en partie par le gouvernement. 
Variable composee des variables de soins/aide a domicile pas paye par le gouvernement (HCC?_12C pour voisin et amis et HCCC?_12D pour membres de la famille et HCC?_12E pour benevole ).
Ce n'est pas complet, mais c'est le mieux possible avec l'ENSP
SO(6) sont a missing car en partie les moins de 18 ans (mais j'aurais pu les mettre a 0) et en partie les gens en institution ***/
 
scalar start2 = 2002
scalar stop2  = 2010


 
local t=start2
foreach x of varlist HCC?_11A { 
gen homecare_i`t' = 0 if `x'==2 & status`t'!=2 
replace homecare_i`t'=. if `x'>=6 & `x' <=9 

local `t'=`++t'
local `t'=`++t'  
}
 
local t=start2
foreach x of varlist HCC?_12C { 
replace homecare_i`t'=1 if `x'==1 & status`t'!=2 

local `t'=`++t'
local `t'=`++t'  
}

local t=start2
foreach x of var HCC?_12D { 
replace homecare_i`t' =1 if  `x'==1 & status`t'!=2 

local `t'=`++t'
local `t'=`++t'  
}

local t=start2
foreach x of var HCC?_12E { 
replace homecare_i`t' =1 if  `x'==1 & status`t'!=2 


local `t'=`++t'
local `t'=`++t'  
}
 
 local t=start2
foreach x of var HCC?_12F { 
replace homecare_f`t' =1 if  `x'==1 & status`t'!=2 
replace homecare_f`t'=0 if `x'==2 & status`t'!=2  & homecare_f`t'==.


local `t'=`++t'
local `t'=`++t'  
}
 
 
********************************************************
***Indicateur de consommation de medicaments oui/non ***
********************************************************
/*
AU COURS D'UN SEUL MOIS
Les SO (6) sont les jeunes de moins de 12 ans (quelques moins de 12 ans dans ND):
ils sont recodes a "missing" comme de toute facon on ne les garde pas (mais j'aurais pu les mettre ‡ 0) - Aurelie */
scalar start=1994
scalar stop=2010
local t=start
foreach x of var DGC?F1 { 
gen drugs`t' = (`x'==1)	if  status`t'!=2  
replace drugs`t'=. if `x'>=6 &`x'<=9


local `t'=`++t'
local `t'=`++t'  
}
***********************************
*** Nombre de nuits ‡ l'hÙpital ***
***********************************
*au cours de 12 mois
*La question a ÈtÈ posÈe ‡ tous, mais la suivante, sur le nombre de nuits, mets le nombre de nuits des jeunes de moins de 12 ans ‡ SO
local t=start
foreach x of var HCC?_1 { // a passÈ la nuit
gen night`t' = (`x'==1)	if  status`t'!=2 
replace night`t'=. if `x'>=6 & `x'<=9
local `t'=`++t'
local `t'=`++t'  
}

*j'ai remplacÈ les SO (996) par des "missing" parce que ce sont les jeunes de moins de 12 ans qui ont rÈpondu oui

local t=start
foreach x of var HCC?_1A { //nombre de nuits
gen nights`t' = `x'	if  status`t'!=2  & `x'<121
replace nights`t'=. if `x'>=996  & `x'<=999 //NSP, R, ND et SO ‡ missing

local `t'=`++t'
local `t'=`++t'  
}

forvalue t=`=start'(2)`=stop-2'{
replace nights`t'	= 0 if night`t'==0 & status`t'!=2 // met seulement ceux qui ont rÈpondu non ‡ une seule nuit ‡ 0

}
*************************************
*** Consultations mÈdecin famille ***
*************************************
*12 derniers mois
*Question posÈe ‡ tous
local t=start
foreach x of var HCC?_2A { 
gen generalist`t' = `x'	if  status`t'!=2  & `x'<26
replace generalist`t'=. if `x'>=996 //NSP, R, ND ‡ missing

local `t'=`++t'
local `t'=`++t'  
}
********************************************************************************************
***Consultations spÈcialiste (chirurgien, allergologue, othopÈdiste, gynÈco, psychiatre) *** 
********************************************************************************************
*12 derniers mois
*question posÈe ‡ tous
local t=start
foreach x of var HCC?_2C { 
gen specialist`t' = `x'	if  status`t'!=2 & `x'<13
replace specialist`t'=. if `x'>=996 //NSP, R, ND ‡ missing

local `t'=`++t'
local `t'=`++t'  
} 


