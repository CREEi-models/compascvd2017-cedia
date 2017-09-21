* code to read a scenario from a dictionary file
* code to read a scenario from a dictionary file
clear all
set more off
cd ~/compas
global scenario = "`1'"
global rep = "`2'"
global discount = 0.03
use output/$scenario/simpop-$rep.dta

* add labels to variables
label var year "year in simulation" 
label var id "identifier of simulated case"
label var age "age in years"
label var byear "birth year"
label var educ "education level (4)"
label var sex "sex of case (female=1)"
label var imm "immigration status (immigrant=1)"
label var income "income bracket (5)"
label var ltc "Long term care (3)"
label var province "region of country (5)"
label var diabe "case has diabetes"
label var hibpe "case has hypertension"
label var hearte "case has heart disease"
label var stroke "case has stroke"
label var cancre "case has cancer"
label var lunge "case has lung disease"
label var mentae "case has mental illness"
label var smoke "smoking status (3)"
label var bmi "bmi status (3)"
label var inv "disability status"
label var alive "case is alive"
label var hc_nights "number of nights in hospital"
label var hc_specialist "number of visits to specialist"
label var hc_generalist "number of visits with generalists"
label var hc_homecare_f "Whether receiving formal home care"
label var hc_homecare_i "Whether receiving informal home care"
label var hc_homecare_sp "Whether receiving home care from spouse"
label var hc_drugs "whether taking medication"
label var hc_hui "Health utility index"
label var cost_nights "annual cost of nights in hospital"
label var cost_specialist "annual cost of specialist consultations"
label var cost_generalist "annual cost of generalist consultations"
label var cost_homecare_f "annual cost of formal home care"
label var hours_homecare_i "annual informal home care hours received"
label var hours_homecare_sp "annual home care hours received from spouse"
label var cost_drugs "annual cost of drugs"
label var cost_nhome "annual cost of nursing home stay"
label var wgt "weight in simulation"

* add lifetime variables to data for each ind
sort id year
by id: gen t = _n
tsset id t
foreach var of varlist diabe hibpe hearte stroke cancre lunge mentae {
	* count number of periods
	*qui egen nyrs_`var' = total(`var'), by(id)
	
	by id: gen temp_nyrs_`var'=sum(`var')
	by id: gen nyrs_`var' = temp_nyrs_`var'[_N]-temp_nyrs_`var'[_n-1]
	by id: replace nyrs_`var'=temp_nyrs_`var'[_N] if _n==1	
	* adjust for gap in years
	by id: replace nyrs_`var' = nyrs_`var'*2 - 1*`var'[_N]
	* find first age onset
	qui gen temp_age_`var' = age if `var'==1
	qui egen agefirst_`var' = min(temp_age_`var'), by(id)
	drop temp_age_`var' temp_nyrs_`var'
}
label var nyrs_diabe "number of years with diabetes"
label var nyrs_hibpe "number of years with hypertension"
label var nyrs_hearte "number of years with heart disease"
label var nyrs_stroke "number of years with stroke"
label var nyrs_cancre "number of years with cancer"
label var nyrs_lunge "number of years with lung disease"
label var nyrs_mentae "number of years with mental illness"

label var agefirst_diabe "first age with diabetes"
label var agefirst_hibpe "first age with hypertension"
label var agefirst_hearte "first age with heart disease"
label var agefirst_stroke "first age with stroke"
label var agefirst_cancre "first age with cancre"
label var agefirst_lunge "first age with lung disease"
label var agefirst_mentae "first age with mental illness"

* add age of death
qui egen agedead = max(age), by(id)
replace agedead=agedead+1
label var agedead "age of death"

* smoking

	qui gen temp_age_smoke = age if smoke==2
	qui egen agefirst_smoke = min(temp_age_smoke), by(id)
	drop temp_age_smoke
	label var agefirst_smoke "age first smoke"
	recode smoke (2=1) (else=0), gen(smoker)
	*qui egen nyrs_smoke = total(smoker), by(id) 
	by id: gen temp_nyrs_smoke=sum(smoker)
	by id: gen nyrs_smoke = temp_nyrs_smoke[_N]-temp_nyrs_smoke[_n-1]
	by id: replace nyrs_smoke=temp_nyrs_smoke[_N] if _n==1	
	qui replace nyrs_smoke = 2*nyrs_smoke - 1*smoker[_N]
	label var nyrs_smoke "number of years smoking"
	drop smoker temp_nyrs_smoke
	qui gen temp_age_quit = age if smoke==3
	qui egen agefirst_quit = min(temp_age_quit), by(id)
	label var agefirst_quit "age first quits smoking"
	drop temp_age_quit
	
* BMI
	recode bmi (2/3=1) (1=0), gen(obese)
	*qui egen nyrs_obese = total(obese), by(id)
	by id: gen temp_nyrs_obese=sum(obese)
	by id: gen nyrs_obese = temp_nyrs_obese[_N]-temp_nyrs_obese[_n-1]
	by id: replace nyrs_obese=temp_nyrs_obese[_N] if _n==1
	by id: replace nyrs_obese = 2*nyrs_obese - 1*obese[_N]
	label var nyrs_obese "number of years obese"	
	qui gen temp_age_obese = age if obese==1
	qui egen agefirst_obese = min(temp_age_obese), by(id)
	label var agefirst_obese "age first becomes obese"
	drop obese temp_age_obese temp_nyrs_obese
	
* disability
	recode inv (3/7=1) (1/2=0), gen(disab)
	by id: gen temp_nyrs_disab=sum(disab)
	by id: gen nyrs_disab = temp_nyrs_disab[_N]-temp_nyrs_disab[_n-1]
	by id: replace nyrs_disab=temp_nyrs_disab[_N] if _n==1	
	label var nyrs_disab "number of years remaining disabled "
	by id: replace nyrs_disab = nyrs_disab*2-1*disab[_N]
	gen nyrs = agedead - age 
	gen nyrs_nondisab = nyrs - nyrs_disab
	label var nyrs "number of years remaining (from current age)"
	label var nyrs_nondisab "number of years remaining non disabled"
	drop disab 
	*qui egen nyrs_disab = total(disab), by(id)
	*recode inv (3/4=0) (1/2=1), gen(nondisab)
	*qui egen nyrs_nondisab = total(nondisab), by(id)
	*qui replace nyrs_nondisab = nyrs_nondisab*2

	
* QALYs
	*recode hc_hui (min/0 =0)
	gen nyrs_qaly = hc_hui*nyrs		
	label var nyrs_qaly "number of QALY remaining "	
* institutionalization
	recode ltc (3=1) (else=0), gen(nhome)
	*qui egen nyrs_nhome = total(nhome), by(id)
	by id: gen temp_nyrs_nhome=sum(nhome)
	by id: gen nyrs_nhome = temp_nyrs_nhome[_N]-temp_nyrs_nhome[_n-1]
	by id: replace nyrs_nhome=temp_nyrs_nhome[_N] if _n==1	
	by id: replace nyrs_nhome = 2*nyrs_nhome - 1*nhome[_N]
	label var nyrs_nhome "number of years remaining spent in nursing home"
	qui gen ever_nhome = (nyrs_nhome!=0)
	label var ever_nhome "ever in a nursing home"	
	qui gen temp_age_nhome = age if nhome==1
	qui egen agefirst_nhome = min(temp_age_nhome), by(id)
	label var agefirst_nhome "age first enters a nursing home"
	drop nhome temp_age_nhome nhome temp_nyrs_nhome
	
	
* homecare (total)
	recode ltc (2=1) (else=0), gen(homecare)
	by id: gen temp_nyrs_homecare=sum(homecare)
	by id: gen nyrs_homecare = temp_nyrs_homecare[_N]-temp_nyrs_homecare[_n-1]
	by id: replace nyrs_homecare=temp_nyrs_homecare[_N] if _n==1	
	by id: replace nyrs_homecare = 2*nyrs_homecare - 1*homecare[_N]
	label var nyrs_homecare "number of years remaining spent receiving home care"
	qui gen ever_homecare = (nyrs_homecare!=0)
	label var ever_homecare "ever received home care"	
	qui gen temp_age_homecare = age if homecare==1
	qui egen agefirst_homecare = min(temp_age_homecare), by(id)
	label var agefirst_homecare "age first receives home care"
	drop homecare temp_age_homecare homecare temp_nyrs_homecare
	
* homecare (formal)
	
	by id: gen temp_nyrs_homecare_f=sum(hc_homecare_f)
	by id: gen nyrs_homecare_f = temp_nyrs_homecare_f[_N]-temp_nyrs_homecare_f[_n-1]
	by id: replace nyrs_homecare_f=temp_nyrs_homecare_f[_N] if _n==1	
	by id: replace nyrs_homecare_f = 2*nyrs_homecare_f - 1*hc_homecare_f[_N]
	label var nyrs_homecare_f "number of years remaining spent receiving formal home care"
	qui gen ever_homecare_f = (nyrs_homecare_f!=0)
	label var ever_homecare_f "ever received formal home care"	
	qui gen temp_age_homecare_f = age if hc_homecare_f==1
	qui egen agefirst_homecare_f = min(temp_age_homecare_f), by(id)
	label var agefirst_homecare_f "age first receives formal home care"
	drop temp_age_homecare_f temp_nyrs_homecare_f
	
* homecare (informal)
	
	by id: gen temp_nyrs_homecare_i=sum(hc_homecare_i)
	by id: gen nyrs_homecare_i = temp_nyrs_homecare_i[_N]-temp_nyrs_homecare_i[_n-1]
	by id: replace nyrs_homecare_i=temp_nyrs_homecare_i[_N] if _n==1	
	by id: replace nyrs_homecare_i = 2*nyrs_homecare_i - 1*hc_homecare_i[_N]
	label var nyrs_homecare_i "number of years remaining spent receiving informal home care"
	qui gen ever_homecare_i = (nyrs_homecare_i!=0)
	label var ever_homecare_i "ever received informal home care"	
	qui gen temp_age_homecare_i = age if hc_homecare_i==1
	qui egen agefirst_homecare_i = min(temp_age_homecare_i), by(id)
	label var agefirst_homecare_i "age first receives informal home care"
	drop temp_age_homecare_i temp_nyrs_homecare_i
	
* homecare (informal from spouse)
	
	by id: gen temp_nyrs_homecare_sp=sum(hc_homecare_sp)
	by id: gen nyrs_homecare_sp = temp_nyrs_homecare_sp[_N]-temp_nyrs_homecare_sp[_n-1]
	by id: replace nyrs_homecare_sp=temp_nyrs_homecare_sp[_N] if _n==1	
	by id: replace nyrs_homecare_sp = 2*nyrs_homecare_sp - 1*hc_homecare_sp[_N]
	label var nyrs_homecare_sp "number of years remaining spent receiving home care from spouse"
	qui gen ever_homecare_sp = (nyrs_homecare_sp!=0)
	label var ever_homecare_sp "ever received home care from spouse"	
	qui gen temp_age_homecare_sp = age if hc_homecare_sp==1
	qui egen agefirst_homecare_sp = min(temp_age_homecare_sp), by(id)
	label var agefirst_homecare_sp "age first receives home care from spouse"
	drop temp_age_homecare_sp temp_nyrs_homecare_sp


* health care use
foreach var of varlist hc_nights hc_specialist hc_generalist hc_drugs hours_homecare_i hours_homecare_sp {
	* count number of periods
	*qui egen total_`var' = total(`var'), by(id)
	* adjust for gap in years
	by id: gen temp_total_`var' = sum(`var')
	by id: gen total_`var' = temp_total_`var'[_N]-temp_total_`var'[_n-1]
	by id: replace total_`var'=temp_total_`var'[_N] if _n==1	
	by id: replace total_`var' = 2*total_`var'  - 1*total_`var'[_N]
}
label var total_hc_nights "total number of nights ever spent"
label var total_hc_specialist "total number of specialist visits ever"
label var total_hc_generalist "total number of generalist visits ever"
label var total_hours_homecare_i "total number of informal homecare hours ever"

rename total_hc_drugs nyrs_hc_drugs
label var nyrs_hc_drugs "total number of years taking drugs"

* cost
	sort id year
	by id: gen fyear = year[1] 
foreach var of varlist cost_nights cost_specialist cost_generalist cost_drugs cost_nhome cost_homecare_f {
	* npv discounted
	gen temp_`var' = `var'*2
	qui gen temp_`var'_d = temp_`var' * exp(-$discount*(year-fyear))
	by id: gen temp_totdis_`var'=sum(temp_`var'_d)
	by id: gen totaldis_`var' = temp_totdis_`var'[_N]-temp_totdis_`var'[_n-1]
	by id: replace totaldis_`var'=temp_totdis_`var'[_N] if _n==1	
	* adjust for gap in years and last year
	by id: replace totaldis_`var' = totaldis_`var' - .5*totaldis_`var'[_N]
	
	*Undiscounte
	by id: gen temp_totundis_`var'=sum(temp_`var')
	by id: gen totalundis_`var' = temp_totundis_`var'[_N]-temp_totundis_`var'[_n-1]
	by id: replace totalundis_`var'=temp_totundis_`var'[_N] if _n==1	
	* adjust for gap in years and last year
	by id: replace totalundis_`var' = totalundis_`var' - .5*totalundis_`var'[_N]
	drop temp_*
}
	
label var totaldis_cost_nights "discounted NPV of hospitalisation costs"
label var totaldis_cost_generalist "discounted NPV of generalist consultation costs"
label var totaldis_cost_specialist "discounted NPV of specialist consultation costs"
label var totaldis_cost_homecare_f "discounted NPV of homecare formal costs"
label var totaldis_cost_drugs "discounted NPV of drug costs"
label var totaldis_cost_nhome "discounted NPV of nhome costs"
label var totalundis_cost_nights "undiscounted NPV of hospitalisation costs"
label var totalundis_cost_specialist "undiscounted NPV of consultation costs"
label var totalundis_cost_generalist "undiscounted NPV of consultation costs"
label var totalundis_cost_homecare_f "undiscounted NPV of homecare formal costs"
label var totalundis_cost_drugs "undiscounted NPV of drug costs"
label var totalundis_cost_nhome "undiscounted NPV of nhome costs"

label var fyear "year cohort enters COMPAS"

capture drop temp_* 
capture drop t
label data "simulated population from scenario $scenario"
compress
save output/$scenario/simpop-$rep.dta, replace







