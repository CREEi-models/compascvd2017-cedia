* do-file to caibrate the age (5 year) x institutional status census dist

* do cchs pop counts
preserve
    use temp/clean-cchs.dta, clear
    gen nhome = ltc==3 if ltc!=.
    egen ageg5 = cut(age), at(30 75(5)85 115)
    gen one = 1
    save temp/calib-cchs.dta, replace
    collapse (sum) pop_cchs = one [fw=wgt], by(province sex ageg5 nhome) 
    table ageg5 province if nhome==1, content(sum pop_cchs) row col 
    sort province sex ageg5 nhome
    save temp/cchs-pop-counts.dta, replace
restore


* do census pop counts  in nh
preserve
    use data/census/census-pop-nh.dta, clear
    egen ageg5temp = cut(ageg5), at(30 75(5)85 110)
    drop ageg5
    rename ageg5temp ageg5
    collapse (sum) pop_nh_census = pop, by(province sex ageg5) 
    sort province sex ageg5
    table ageg5 province, content(sum pop_nh_census) row col
    table ageg5 province, content(sum pop_nh_census) col row
    save temp/census-pop-nh-counts.dta, replace
restore


* do census pop counts total 
preserve
    use data/census/census-year-age-sex-province.dta, clear 
    keep if year==2010
    keep if age>=30
    egen ageg5 = cut(age), at(30 75(5)85 115)
    tab ageg5
    collapse (sum) pop_census = pop, by(province sex ageg5) 
    sort province sex ageg5
    save temp/census-pop-total-counts.dta, replace
restore

* do census pop counts not in nh
preserve
    use  temp/census-pop-total-counts.dta, clear 
    merge 1:1 province sex ageg5 using temp/census-pop-nh-counts.dta
    drop _merge
    gen frac = pop_nh_census / pop_census
    label def prov2 1 "atlantic" 2 "quebec" 3 "ontario" 4 "prairies" 5 "bc"
    label val province prov2
    table ageg5 province, content(mean frac) format(%6.3f)
    gen pop_nonnh_census = pop_census - pop_nh_census
    sum pop_nonnh_census pop_nh_census
    rename pop_nonnh_census pop0
    rename pop_nh_census pop1
    keep province sex ageg5 pop0 pop1
    egen id = group(province ageg5 sex)
    reshape long pop, i(id) j(nhome)
    drop id
    rename pop pop_census
    sort province sex ageg5 nhome
    save temp/census-pop-counts.dta, replace
restore

* compute adjustment factors
preserve
    use temp/census-pop-counts.dta
    merge 1:1 province sex ageg5 nhome using temp/cchs-pop-counts.dta
    gen adjust = pop_census / pop_cchs
    sum adjust
    label var adjust "ratio of census pop to cchs pop"
    keep province sex ageg nhome adjust
    save temp/adjustments-census.dta, replace
restore

* calibrate the weights and clean-up
preserve
    use temp/calib-cchs.dta, clear
    merge n:1 province sex ageg5 nhome using temp/adjustments-census.dta
    drop _merge 
    replace wgt = round(wgt*adjust)
    drop adjust inv3 ageg
    table ageg5 province [fw=wgt], content(sum nhome) col row
    drop nhome one ageg5
    label data "Initialization sample for COMPAS based on CCHS 2010"   
    save data/input/startpop.dta, replace
restore



