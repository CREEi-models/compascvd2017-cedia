* do-file to estimate multivariate model for initial conditions
* save to file parameters
    cd ~/compas
    global opt "replace comma nonames nolabel noquote"
     capture ssc uninstall cmp
     ssc install cmp

forvalues i = 1/100 {
   di "newcohort : `i'"
     use data/input/newcohorts`i'.dta,clear
   
  
    tab province, gen(prov_)
        gen prov_1_female = prov_1*sex

    gen prov_2_female = prov_2*sex
        gen prov_3_female = prov_3*sex
    gen prov_4_female = prov_4*sex
    gen prov_5_female = prov_5*sex
    

    global x = "sex  prov_2-prov_5 prov_2_female prov_3_female prov_4_female prov_5_female"
    tab educ4
    tab smoke
    tab bmi

    cmp setup
    #d ;
    cmp
	    (educ4 = $x, noconstant)
	    (smoke = $x, noconstant)
	    (bmi = $x, noconstant)
	    [aw = wgt]
	    ,
	    ghkd(23)
	    ind(
	    $cmp_oprobit
	    $cmp_oprobit
	    $cmp_oprobit
	    );
    #d cr
    matrix covar = e(Sigma)
    matrix choleski = cholesky(covar)
    matrix list covar
    matrix list choleski
    predict educ , pr
    
   * save temp/pr3_educ`i'.dta,replace
   * exit

    preserve
        svmat choleski
        keep choleski*
        keep if choleski1!=.
        outsheet using params/newcohorts/choleski`i'.csv, $opt
    restore

    global varx = 9
    global nvar = 3
    matrix par = e(b)'
    matrix list par
    matrix beta = J($varx,$nvar,0)
    local s = 1
    forvalues j = 1/$nvar {
            local kk = $varx
            forvalues k = 1/`kk' {
                matrix beta[`k',`j'] = par[`s',1]
                local s = `s' + 1
            }


    }
    matrix list beta
       *exit

    preserve
        matrix beta = (1,1,1)\beta //Write the number of line
	svmat beta
        keep beta1-beta3
        keep if beta1!=.
	tostring(beta1-beta3), replace force usedisplayformat

	foreach var of varlist beta1-beta3 {
	replace `var' = "9" in 1
	replace `var' = "sex,"+`var' in 2
	replace `var' = "quebec,"+`var' in 3
	replace `var' = "ontario,"+`var' in 4
	replace `var' = "prairies,"+`var' in 5
	replace `var' = "bc,"+`var' in 6
	replace `var' = "s_quebec,"+`var' in 7
	replace `var' = "s_ontario,"+`var' in 8
	replace `var' = "s_prairies,"+`var' in 9
	replace `var' = "s_bc,"+`var' in 10
	}

	outsheet beta1 using params/newcohorts/mean_educ`i'.csv, $opt
        outsheet beta2 using params/newcohorts/mean_smoke`i'.csv, $opt
        outsheet beta3 using params/newcohorts/mean_bmi`i'.csv, $opt
    restore

    * threshold for education
    matrix tau_educ4 = J(3+2,1,.)
    matrix tau_educ4[1,1] = -100
    matrix tau_educ4[5,1] = 100
    forvalues j = 2/4 {
        matrix tau_educ4[`j',1] = par[`s',1]
        local s = `s' + 1
    }
    matrix list tau_educ4
    preserve
        svmat tau_educ4
        keep tau_educ41
        keep if tau_educ41!=.
        outsheet tau_educ41 using params/newcohorts/thres_educ`i'.csv, $opt
    restore

    * threshold for smoking
    matrix tau_smoke = J(2+2,1,.)
    matrix tau_smoke[1,1] = -1e4
    matrix tau_smoke[4,1] = 1e4
    forvalues j = 2/3 {
        matrix tau_smoke[`j',1] = par[`s',1]
        local s = `s' + 1
    }
    matrix list tau_smoke
    preserve
        svmat tau_smoke
        keep tau_smoke1
        keep if tau_smoke1!=.
        outsheet tau_smoke1 using params/newcohorts/thres_smoke`i'.csv, $opt
    restore
    * threshold for bmi
    matrix tau_bmi = J(2+2,1,.)
    matrix tau_bmi[1,1] = -1e4
    matrix tau_bmi[4,1] = 1e4
    forvalues j = 2/3 {
        matrix tau_bmi[`j',1] = par[`s',1]
        local s = `s' + 1
    }
    matrix list tau_bmi
    preserve
        svmat tau_bmi
        keep tau_bmi1
        keep if tau_bmi1!=.
        outsheet tau_bmi1 using params/newcohorts/thres_bmi`i'.csv, $opt
    restore
    tab educ4, gen(educ_cchs_)
	
    collapse (mean) educ_cchs_1-educ_cchs_4 educ_1-educ_4 [w=wgt], by(province sex)
    gen educ_1_factor =   educ_cchs_1/educ_1
    gen educ_2_factor =   educ_cchs_2/educ_2
    gen educ_3_factor =   educ_cchs_3/educ_3
    gen educ_4_factor =   educ_cchs_4/educ_4

    keep sex province  educ_1_factor educ_2_factor educ_3_factor educ_4_factor
    order sex province educ_1_factor educ_2_factor educ_3_factor educ_4_factor
    sort sex  province
    *tabstat educ_1_factor educ_2_factor educ_3_factor educ_4_factor, by(sex province)

    #d ;
	outsheet using params/newcohorts/factor_educ`i'.csv,
	    delimiter(",")  nonames nolabel replace;
	#d cr  
}
