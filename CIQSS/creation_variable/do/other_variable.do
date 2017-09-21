*Constructions des autres variables explicatives - Steeve Marchand 

scalar start=1994
scalar stop=2010
*Education
/*
Les SO (96) sont les jeunes de moins de 12 ans:
ils sont recodes a "missing" comme de toute facon on ne les garde pas (mais j'aurais pu les mettre a 0) - Aurelie */

local t=start
foreach x of var EDC?D1{ 
gen scol`t' = 0 if `x'<4 &  status`t'!=2 //moins que secondaire
replace scol`t'=1 if `x'>=4 & `x'<9  & status`t'!=2 //secondaire
replace scol`t'=2 if `x'>=9 & `x'<11 &  status`t'!=2 // cegep
replace scol`t'=3 if  `x'>=11 & `x'<=14 &  status`t'!=2 //universite
replace scol`t'=. if `x'>=96 //Les SO, NSP, R et ND a missing

local `t'=`++t'
local `t'=`++t'  
}

forvalue t = `=start'(2)`=stop-2'{
local t1 = `t'+2
replace scol`t1' = scol`t' if scol`t'!=. & scol`t1'==. &  status`t1'!=2  // Si la personne arrete de repondre, elle garde son niveau le plus rent
}



***age***
forvalue t = `=start'(2)`=stop'{
gen age`t' = `t' - YOB if  status`t'!=2
}


***province of residence***
local t=start
foreach x of var PRC?_CUR  { 
gen quebec`t' = (`x'==24) if `x'<60
gen atlantic`t' = (`x'==10 | `x'==11 | `x'==12 |`x'==13) if `x'<60
gen ontario`t' = (`x'==35) if `x'<60
gen prairies`t' = (`x'==46 | `x'==47 | `x'==48) if `x'<60
gen bc`t' = (`x'==59) if `x'<60
 
local `t'=`++t'
local `t'=`++t'  
}

*revenu total du menage
local t=start
foreach x of var INC?DHH{ 

gen totincome`t' = `x' if `x'<90

local `t'=`++t'
local `t'=`++t' 

}


*Statut marital
local t=start
foreach x of var DHC?_MAR{ 

gen couple`t' = inlist(`x',1,2,3)

local `t'=`++t'
local `t'=`++t' 

}

*Statut enfant
local t=start
foreach x of var DHC?DECF{ 

gen enfant`t' = inlist(`x',5,6,7,8,9,10,11,12,13,14,15,16)

local `t'=`++t'
local `t'=`++t' 

}
global demo "age* scol* quebec* atlantic* ontario* prairies* bc* totincome* couple* enfant*"
