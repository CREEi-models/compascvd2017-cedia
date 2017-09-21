* account for mortality at older ages underpredicted relative to census

clear all
capture log close
set more off

*log using logs/calibrate-mortality.txt, replace text


    * read parmaters
  forvalue i = 1/100 {
    #d ;
        import delimited "params/transit/alive`i'.csv",
            delimiter(comma) varnames(nonames) rowrange(2) clear;
    #d cr
    mkmat v2, matrix(par_trans)
    matrix par_trans = par_trans'
    global n = _N
    global names
    forvalues j = 1/$n {
        global this = v1[`j']
        if ("$this"=="_cons") {
            global this = "cons"
        }
        di "$this"
        global names = "$names  $this"
        di "$names"
    }
    di "$names"
    matrix list par_trans
    matrix colnames par_trans = $names
    matrix par_trans`i' = par_trans
 }

    use temp/startpop-all.dta, clear
    * map to vars of transition model
    gen age50m = min(age,50) if age!=.
    gen age50p = max(age-50,0) if age!=.
    gen smoker = smoke==2 if smoke!=.
    gen former = smoke==3 if smoke!=.
    gen obese = bmi==2 if bmi!=.
    gen veryobese = bmi==3 if bmi!=.
    gen hs = educ4==2 if educ4!=.
    gen college = educ4==3 if educ4!=.
    gen univ = educ4==4 if educ4!=.
    gen quebec = province==2 if province!=.
    gen ontario=province==3 if province!=.
    gen prairies=province==4 if province!=.
    gen bc=province==5 if province!=.
    gen adl = (inv==4 | inv==6  | inv==7) if inv!=.
    gen iadl = (inv==3 | inv==5  | inv==6| inv==7) if inv!=.
    gen cognitive = (inv==2 | inv==5  | inv==7) if inv!=.
    gen inv1= (inv==1) if inv!=.
    gen inv2= (inv==2) if inv!=.
    gen inv3= (inv==3) if inv!=.
    gen inv4= (inv==4) if inv!=.
    gen inv5= (inv==5) if inv!=.
    gen inv6= (inv==6) if inv!=.
    gen inv7= (inv==7) if inv!=.
    gen nhome = ltc==3 if ltc!=.
    gen constant = 1
    forvalue i = 1/100 {
      preserve
      matrix score xb = par_trans`i'
      gen prob = 1 - exp(-exp(xb))
      rename prob mx_compas
      merge n:1 age sex using data/census/census-mx.dta
      drop _merge
      rename mx mx_census
      gen mx = mx_census
      replace mx_census =1/(1/mx-.5) //Convert from probability to mortality rate
      replace mx_census = mx_census*(2-mx_census)
      collapse mx_census mx_compas mx [fw=wgt], by(sex age)
      *Smooth mx compas
      *lowess mx_compas age,by(sex) gen(mx_compas_smooth) bwidth(.3)

      gen factor = mx_census/mx_compas
      *table age sex, content(mean mx_compas)

      *tabstat mx_compas mx_census factor, by(age)
      keep sex age factor
      order sex age factor
      sort sex age
      #d ;
  	   outsheet using params/transit/factor`i'.csv,
  	    delimiter(",")  nonames nolabel replace;
  	  #d cr
      restore
    }


log close
