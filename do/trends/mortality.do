
set more off
clear

global opt "replace comma nonames nolabel"

* use file gotten from Stats Can on mortality rates
import delimited "data/census/DTH_Tx_Scenario_M-Table 1.csv", clear

rename anne yr
gen year = substr(yr,1,4)
destring year, replace
label var year "year"
drop yr
drop if year>2050

rename sexe sex
label var sex "sex (female=1)"
recode sex (1=0) (2=1)

recode cgt (10/13=1) (24=2) (35=3) (46/48=4) (59=5) (else=.), gen(province)
label def province 1 "atlantic" 2 "quebec" 3 "ontario" 4 "prairies" 5 "bc"
label values province province

drop if province==.
drop cgt

* rename variables
forvalues j=4/84 {
	local k = 26 + `j'
	rename v`j' qx`k'
	replace qx`k' = 1/(1/qx`k' - 1/2)
}

collapse qx30-qx110, by(year sex province)

save "temp/census-improve.dta", replace

forvalues p = 1/5 {
	forvalues s = 0/1 {
		preserve
			keep if sex==`s'&province==`p'
			drop sex province
			keep year qx*
			global N = _N + 3
			set obs $N
			replace year = 2010 if _n==($N-2)
			replace year = 2011 if _n==($N-1)
			replace year = 2012 if _n==$N
			sort year
			forvalues j =30/110 {
				replace qx`j' = qx`j'[4] if year<2013
			}
			sort year
			egen year2 = cut(year), at(2010(2)2050 2100)
			keep if year==year2
			drop year2
			sort year
			forvalues j = 30/110 {
				gen rx`j' = qx`j'/qx`j'[1]
			}
			replace rx110 = 1
			drop qx*
			save "params/trends/trend-mortality-`p'-`s'.dta", replace
			outsheet using "params/trends/trend-mortality-`p'-`s'.csv", $opt
		restore
	}
}

exit
