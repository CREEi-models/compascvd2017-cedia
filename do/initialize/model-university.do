* model estimated on epa for university diploma 
* conditional on having post-secondary ed
preserve 

use "data/lfs/lfs-2010.dta",clear

* selection
drop ed76to89
drop age_6
keep if educ90>=4

* create age variable 
gen ageg =  30 		if age_12==4
forvalues j = 5/12 {
	replace ageg = 30 + 5*(`j'-4) if age_12==`j'
}

* province
recode prov (10/13=1) (24=2) (35=3) (46/48=4) (59=5), gen(province)
label def province 1 "atlantic" 2 "quebec" 3 "ontario" 4 "prairies" 5 "bc"
label val province province

* gender
replace sex = sex-1
label value sex
label def sex 0 "male" 1 "female"  
label values sex sex

* university diploma variable
gen univ= (educ90>=5) if educ90!=.
keep univ fweight ageg sex province
collapse (mean) puniv=univ [aw=fweight], by(ageg sex province)
sort ageg sex province
order ageg sex province puniv

save temp/prob-university.dta, replace

table ageg province sex, content(mean puniv)

restore
