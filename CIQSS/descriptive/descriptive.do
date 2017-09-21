
clear all
clear matrix
capture log close

cd "T:\Projet 3985-S003\compas\Transitions pour PLOS ONE"
set maxvar 10000
set more off
use "data\clean_CCHS_plos_one.dta", replace

*Prévalence de maladies

table age10 [pw=WT64LS], c(mean cancre mean hearte mean stroke) row


*Incidence de maladies

table age10 [pw=WT64LS] if age5<95, c(mean icancre) row
table age10 [pw=WT64LS], c(mean ihearte) row
table age10 [pw=WT64LS] if age5>=35, c(mean istroke) row

*utilisation par age

table age10 [pw=WT64LS], c(mean generalist mean specialist mean nights) row
table age10 [pw=WT64LS], c(mean drugs mean homecare) row

* utilisation par maladie

global cc "cancre hearte stroke"

foreach var of varlist $cc {
table `var' [pw=WT64LS], c(mean generalist mean specialist mean nights) row
table `var' [pw=WT64LS], c(mean drugs mean homecare) row
}

*Rémissions 
table age10 [pw=WT64LS] if age5<95, c(mean rcancre) row
table age10 [pw=WT64LS], c(mean rhearte) row
table age10 [pw=WT64LS] if age5>=35, c(mean rstroke) row

*Nombre d'obs
foreach var of varlist cancre hearte stroke icancre ihearte istroke drugs homecare rcancre rhearte rstroke{
table age10 `var'
}

tabstat generalist specialist nights, by(age10) stat(n)

foreach var of varlist $cc {
table `var', c(n generalist) row
}
foreach var of varlist $cc {
table `var', c(n specialist) row
}
foreach var of varlist $cc {
table `var', c(n nights) row
}
foreach var of varlist $cc {
table `var', c(n drugs) row
}
foreach var of varlist $cc {
table `var', c(n homecare) row
}
