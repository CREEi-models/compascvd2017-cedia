* do-file to caibrate the age (5 year) x institutional status census dist

* do cchs pop counts
preserve
    use temp/clean-cchs-first.dta, clear
    gen one = 1
    recode ageg (70/80=70)
    save temp/calib-cchs-educ.dta, replace
    collapse (sum) pop_cchs = one [fw=wgt], by(province ageg educ4)
    *table ageg province if nhome==1, content(sum pop_cchs) row col
    sort province  ageg educ4
    save temp/cchs-pop-counts-educ.dta, replace
restore


* do epa pop counts  by education

    use data/other/educ_epa.dta, clear
    rename age_12 ageg
    drop if ageg<4
    replace ageg = 30 + 5*(ageg-4)
    rename prov province
    recode province (10/13=1) (24=2) (35=3) (46/48=4) (59=5) (else=.)
    gen one = 1

    preserve
    rename educ90 educ4
    recode educ4 (0/1=1) (2/3=2) (4/6=3)
    label def educ4 1 "less than HS" 2 "HS" 3 "College+"
    label val educ4 educ4
    label var educ4 "Education level (3 cat)"
    collapse (sum) pop_educ_epa = one [fw=fweight], by(province ageg educ4)
    sort province ageg educ4
    replace pop_educ_epa = pop_educ_epa/12
    *table ageg5 province, content(sum pop_nh_census) row col
    *table ageg5 province, content(sum pop_nh_census) col row
    save temp/epa-pop-educ-counts.dta, replace
    restore
    collapse (sum) pop_epa = one [fw=fweight], by(province ageg)
    replace pop_epa = pop_epa/12
    save temp/epa-pop-counts.dta, replace


* compute adjustment factors
    use temp/epa-pop-educ-counts.dta,clear
    merge 1:1 province ageg educ4 using temp/cchs-pop-counts-educ.dta
    gen adjust = pop_educ_epa / pop_cchs
    sum adjust
    label var adjust "ratio of census pop to cchs pop"
    keep province ageg educ4 adjust
    save temp/adjustments-educ-epa.dta, replace


* calibrate the weights and clean-up

    use temp/clean-cchs-first.dta, clear
    merge n:1 province  ageg educ4 using temp/adjustments-educ-epa.dta
    sort province educ4 ageg
    by province educ4 : replace adjust = adjust[_n-1] if adjust ==.
    drop _merge
    replace wgt = round(wgt*adjust)
    drop adjust
    save temp/clean-cchs-first-calib.dta, replace
