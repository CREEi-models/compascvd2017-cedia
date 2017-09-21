**********************************************************
*** indicateur soins/aide a domicile (oui/non) ***
**********************************************************

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
*** Nombre de nuits ‡ l'hôpital ***
***********************************

local t=start
foreach x of var HCC?_1 { // a passÈ la nuit
gen night`t' = (`x'==1)	if  status`t'!=2 
replace night`t'=. if `x'>=6 & `x'<=9
local `t'=`++t'
local `t'=`++t'  
}

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

local t=start
foreach x of var HCC?_2C { 
gen specialist`t' = `x'	if  status`t'!=2 & `x'<13
replace specialist`t'=. if `x'>=996 //NSP, R, ND ‡ missing

local `t'=`++t'
local `t'=`++t'  
} 


