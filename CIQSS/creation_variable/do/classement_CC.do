/* Classement des variables Chronic Condition (CC)*/

***DIABETE***(Une seule variable utile)
scalar start=1994
scalar stop=2010
local t=start
foreach x of var CCC?_1J { // Pour la var du statut de diabete de chaque cycle...
gen diabe`t' = (`x'==1)	if  status`t'!=2
replace diabe`t'=. if `x'>=6 &`x'<=9 //NSP, R, ND et SO a manquantes



local `t'=`++t'  
local `t'=`++t'  
}


***HYPERTENSION***(Une seule variable utile)
local t=start
foreach x of var CCC?_1F { 
gen hibpe`t' = (`x'==1)	if  status`t'!=2
replace hibpe`t'=. if `x'>=6 &`x'<=9 //NSP, R, ND et SO a manquantes




local `t'=`++t'
local `t'=`++t'  
}


***CANCER***(Une seule variable utile)
local t=start
foreach x of var  CCC?_1M{ 
gen cancre`t' = (`x'==1)	if  status`t'!=2
replace cancre`t'=. if `x'>=6 &`x'<=9 //NSP, R, ND et SO a manquantes



local `t'=`++t'
local `t'=`++t'  
}



***MALADIES CARDIAQUES***(Une seule variable utile)
local t=start
foreach x of var CCC?_1L{ 
gen hearte`t' = (`x'==1)	if  status`t'!=2
replace hearte`t'=. if `x'>=6 &`x'<=9 //NSP, R, ND et SO a manquantes


local `t'=`++t'
local `t'=`++t'  
}



***ACV***(une seule variable utile)

local t=start
foreach x of var CCC?_1O { 	
	
gen stroke`t' = (`x'==1)	if  status`t'!=2
replace stroke`t'=. if `x'>=6 &`x'<=9 //NSP, R, ND et SO a manquantes


local `t'=`++t'
local `t'=`++t'  
}


***MALADIES DES POUMONS***(bronchite chronique et emphyseme)

local t=start
foreach x of var  CCC?_1H{ 	//Bronchite et emphyseme
	
gen lunge`t' = (`x'==1)	 if  status`t'!=2
replace lunge`t'=. if `x'>=6 &`x'<=9 //NSP, R, ND et SO a manquantes

local `t'=`++t'
local `t'=`++t'  
}


***TROUBLES MENTAUX***Alzheimer ou autre demence cerebrale(CC)

local t=start
foreach x of var CCC?_1R { 	//(Alzheimer ou autre demence cerebrale)
	
gen mentae`t' = (`x'==1) 	if  status`t'!=2
replace mentae`t'=. if `x'>=6 &`x'<=9 //NSP, R, ND et SO a manquantes


local `t'=`++t'
local `t'=`++t'  
}

 *Correction necessaire pour ceux qui n'ont pas repondu a t, mais qui ont repondu a t-2 et t+2
 *Leur met la reponse de t-2
 scalar start=1996
 scalar stop=2008
forvalue t=`=start'(2)`=stop-2'{
local t1 = `t'+2
local t0=`t'-2
replace diabe`t' = diabe`t0' if diabe`t'==. &  diabe`t0' !=. & diabe`t1' !=. & status`t1'!=2
replace hearte`t' = hearte`t0' if hearte`t'==. &  hearte`t0' !=. & hearte`t1' !=. & status`t1'!=2
replace stroke`t' = stroke`t0' if stroke`t'==. &  stroke`t0' !=. & stroke`t1' !=. & status`t1'!=2
replace cancre`t' = cancre`t0' if cancre`t'==. &  cancre`t0' !=. & cancre`t1' !=. & status`t1'!=2
replace hibpe`t' = hibpe`t0' if hibpe`t'==. &  hibpe`t0' !=. & hibpe`t1' !=. & status`t1'!=2
replace mentae`t' = mentae`t0' if mentae`t'==. &  mentae`t0' !=. & mentae`t1' !=. & status`t1'!=2
replace lunge`t' = lunge`t0' if lunge`t'==. &  lunge`t0' !=. & lunge`t1' !=. & status`t1'!=2

}
*Correction necessaire pour les gens en institution: gardent leur reponse la plus recente

scalar start=1994
scalar stop=2010
forvalue t=`=start'(2)`=stop-2'{
local t1=`t'+2
replace diabe`t1' = diabe`t' if diabe`t1'==. &  diabe`t' !=. & status`t1'==3
replace mentae`t1' = mentae`t' if mentae`t1'==. &  mentae`t' !=. & status`t1'==3
replace hearte`t1' = hearte`t' if hearte`t1'==. &  hearte`t' !=. & status`t1'==3
replace lunge`t1' = lunge`t' if lunge`t1'==. &  lunge`t' !=. & status`t1'==3
replace stroke`t1' = stroke`t' if stroke`t1'==. &  stroke`t' !=. & status`t1'==3
replace cancre`t1' = cancre`t' if cancre`t1'==. &  cancre`t' !=. & status`t1'==3
replace hibpe`t1' = hibpe`t' if hibpe`t1'==. &  hibpe`t' !=. & status`t1'==3

}


*Question de suivi pour les maladies chroniques

scalar start2=1996
scalar stop=2010


local t=start2
foreach x of var  CCC?_O1{ 	//stroke dispute
	
gen stroke_never`t' = (`x'==3)	 if  status`t'!=2
replace stroke_never`t'=. if `x'>=6 &`x'<=9 //NSP, R, ND et SO a manquantes

local `t'=`++t'
local `t'=`++t'  
}



local t=start2
foreach x of var  CCC?_J1{ 	//diabetes dispute
	
gen diabe_never`t' = (`x'==3)	 if  status`t'!=2
replace diabe_never`t'=. if `x'>=6 &`x'<=9 //NSP, R, ND et SO a manquantes

local `t'=`++t'
local `t'=`++t'  
}

local t=start2
foreach x of var  CCC?_F1{ 	//hibpe dispute
	
gen hibpe_never`t' = (`x'==3)	 if  status`t'!=2
replace hibpe_never`t'=. if `x'>=6 &`x'<=9 //NSP, R, ND et SO a manquantes

local `t'=`++t'
local `t'=`++t'  
}

scalar start3=2000
scalar stop=2010

local t=start3
foreach x of var  CCC?_L1A{ 	//hearte : heart attack
	
gen hearte_attack`t' = (`x'==1)	 if  status`t'!=2
replace hearte_attack`t'=. if `x'>=6 &`x'<=9 //NSP, R, ND et SO a manquantes

local `t'=`++t'
local `t'=`++t'  
}
local t=start3

foreach x of var  CCC?_L6{ 	//hearte : angina
	
gen hearte_angina`t' = (`x'==1)	 if  status`t'!=2
replace hearte_angina`t'=. if `x'>=6 &`x'<=9 //NSP, R, ND et SO a manquantes

local `t'=`++t'
local `t'=`++t'  
}
local t=start3

foreach x of var  CCC?_L7{ 	//hearte : congective heart failure
	
gen hearte_chf`t' = (`x'==1)	 if  status`t'!=2
replace hearte_chf`t'=. if `x'>=6 &`x'<=9 //NSP, R, ND et SO a manquantes

local `t'=`++t'
local `t'=`++t'  
}


			
global prevalence "diabe* hibpe* cancre* hearte* stroke* lunge* mentae*"
global never "stroke_never* diabe_never* hibpe_never*"

			
