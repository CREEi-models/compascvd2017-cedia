* do-file to prepare data for new cohort imputation model
    global opt "replace novarnames nolabel"
    forvalues i = 1/50 {
    use data/input/startpop`i'.dta, clear
    keep if inlist(age,30,31)
      
    table province, content(sum wgt) row
   * drop adl    
    drop if id==.
    *expand 5
    *drop id  
    *replace wgt= wgt/5
    *gen id =_n
    order id age byear educ4 income sex imm province diabe hibpe hearte stroke cancre lunge mentae smoke bmi inv ltc wgt
    des
    * save file for estimation
	save  data/input/newcohorts`i'.dta, replace 
	
	* export to csv number of obs
	    count 
	    gen n = r(N)
	    *keep if _n==1
	    local j = `i'
	    export delimited n using params/newcohorts/nobs`i'.csv if _n==1, $opt 
	    drop n
 
	* export to data file to csv
	preserve
		order id age byear educ4 income sex imm province diabe hibpe hearte stroke cancre lunge mentae smoke bmi inv ltc wgt
		sum age, d
	    export delimited using data/input/newcohorts`i'.csv, $opt 
    restore    
   
    
}
