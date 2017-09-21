*COMPAS output profile - table
cd "`3'"
capture log close 
log using log_table, replace

global actual = "`1'"
global master = "`2'"

#delimit ;
	global variable "lesshs hs college university diabe hibpe hearte stroke 
					 cancre lunge mentae nvrsmoke former smoker nonobese obese veryobese
					 adl iadl cognitive hc_nights hc_specialist hc_generalist 
					 hc_hui hc_drugs hc_homecare_f hc_homecare_i hc_homecare_sp 
					 hours_homecare_i hours_homecare_sp nhome" ;
#delimit cr
foreach run in $actual $master { 
	foreach var of global variable {
	use "./`run'/`var'_mean_year_value.dta", clear
	gen variable = "`var'"
	rename `var' proportion
	rename `var'_sd proportion_sd
	tempfile `run'_`var'
	save "``run'_`var''"
	}
}
local first = 1

foreach run in $actual $master { 
	foreach var of global variable {
		if `first'==1 {
		use "``run'_`var''", clear
		local first = 2
		continue
		}	
		append  using "``run'_`var''"
	}
	if "`run'" == "$actual" {
			rename proportion proportion_a
			rename proportion_sd proportion_sd_a
		}
	else  {
			rename proportion proportion_m
			rename proportion_sd proportion_sd_m
		}
   tempfile `run'
    save "``run''"
   local first = 1
}
merge 1:1 year variable using "`$actual'"
gen diff = proportion_a-proportion_m
gen diff_pct = diff/proportion_m*100
gen flag = (diff > 1.96*proportion_sd_m)
sort variable year
list variable year diff_pct if flag ==1

keep if year <=2050
#delimit ;
listtab variable year proportion_m proportion_a diff_pct flag  using ./rapport/list1.tex,replace
rstyle(tabular) head("\begin{longtable}{lrrrrr}""
\textit{Variable} & \multicolumn{1}{l}{\textit{Year}} &\multicolumn{1}{l}{\textit{Benchmark}}&\multicolumn{1}{l}{\textit{Current}}&\multicolumn{1}{l}{\textit{Diff (\%)}}&\multicolumn{1}{l}{\textit{Flag}}\\\\""
\endhead")
foot("\end{longtable}");
#delimit cr
!sed 's/_/ /g' <./rapport/list1.tex >./rapport/list.tex
!rm ./rapport/list.text

#delimit ;
listtab variable year proportion_m proportion_a diff_pct flag  if flag==1 using ./rapport/list.dat,replace
rstyle(tabdelim);
#delimit cr


log close 
