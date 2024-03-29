*! cmp 6.8.0 5 March 2015
*! Copyright David Roodman 2007-15. May be distributed free.

cap program drop cmp_p
program define cmp_p
	version 10.0
	cap version 11.0
	syntax anything [if] [in], [EQuation(string) Outcome(string) RELevel(string) xb FIXEDonly NOOFFset e pr REsiduals lnl SCores Ystar(string) REDucedform CONDition(string) *]
	local _pr `pr'
	local _e `e'
	local 0, `options'
	syntax, [pr(string) e(string) *]
	if "`pr'"!="" & `"`outcome'"'!="" {
		di as error "{cmd:pr(`pr')} incompatible with {cmd:outcome(`outcome')}."
		exit 198
	}

	if `: word count `xb' `_pr' `_e' `residuals' `scores' `lnl'' + (`"`pr'"'!="") + (`"`ystar'"'!="") + (`"`e'"'!="") > 1 {
		di as err "Only one statistic allowed per {cmd:predict} call."
		exit 198
	}
	local options `options' `scores'

	if "`pr'"=="" & "`_pr'"!="" local pr 0 .
	if "`e'" =="" & "`_e'" !="" local e  . .
	
	tempvar xb
	local _options `options'
	marksample touse, novarlist
	
	if "`scores'"=="" {
		tempname b
		mat `b' = e(b)
		mat `b' = `b'[1,1..`=e(k)-e(k_aux)']
	}
	else qui replace `touse' = `touse' & e(sample)

	_score_spec `anything', equation(`equation') b(`b')
	if "`s(eqspec)'"=="#1" & `"`equation'`lnl'"'=="" & e(k_dv)>1 di as txt "(equation #1 assumed)"
	local vartype: word 1 of `s(typlist)'
	local _varlist `s(varlist)'
	local _eqspec `s(eqspec)'

	if `"`outcome'"'!="" local pr 0 .

	if `"`pr'`e'`ystar'"'!="" {
		if `: word count `pr'`e'`ystar'' != 2 {
			di as err "{cmd:pr}, {cmd:e}, and {cmd:ystar} require two arguments, without commas."
			exit 198
		}
		tempname Sigma sig rho
		mat `Sigma' = e(Sigma)
		local ll: word 1 of `pr'`e'`ystar'
		local ul: word 2 of `pr'`e'`ystar'
		cap confirm var `ll'
		local lmissing = _rc & (`ll')>=.
		cap confirm var `ul'
		local umissing = _rc & (`ul')>=.
		if `lmissing' & `umissing' & `"`ystar'"'!="" {
			local e `ystar'
			local ystar
		}
	}
	else if `"`condition'"'!="" {
		di as err "{cmdab:cond:ition} option only compatible with {cmd:pr} and {cmdab:y:star} statistic options."
		exit 198
	}
	if `"`condition'"'!="" {
		tempvar condxb
		tempname condsig
		local 0 `condition'
		syntax anything, EQuation(string)
		local condll: word 1 of `anything'
		local condul: word 2 of `anything'
		if "`condll'`condul'"==".." local condition
		else {
			local condeq `equation'
			_score_spec `condxb', equation(`condeq') b(`b')
			local condeq = substr("`s(eqspec)'", 2, .)
			Predict double `condxb' if `touse', eq(`s(eqspec)') `reducedform'
			scalar `condsig' = sqrt(`Sigma'[`condeq',`condeq'])
		}
	}
	
	if "`lnl'" != "" {
		if e(L) > 1 {
			di as error "Observation-level likelihoods not defined for random effects/coefficient models. e(ll) holds the overall log-likelihood (`e(ll)')."
			exit 111
		}

		_stubstar2names `anything', nvars(1)
		local vartype `s(typlist)'
		local varname `s(varlist)'
		quietly {
			gen `vartype' `varname' = .
			`e(cmdline)' predict // run cmp front end to reconstruct necessary _cmp_* variables
			tempname hold
			_est hold `hold', copy
			local model `e(model)'
			ml model `:subinstr local model ": . =" ": _cmp_ind1 =", all' if e(sample) & `touse', collinear missing
			_est unhold `hold'
			if e(user) == "cmp_lf" {
				tempvar t
				_stubstar2names `t'*, nvars(`e(k_eq)')
				tokenize `s(varlist)'
				forvalues i=1/`e(k_eq)' {
					Predict double ``i'' if e(sample) & `touse', eq(#`i') `reducedform'
				}
				cmp_lf `varname' `*'
			}
			else {
				tempname b
				mat `b' = e(b)
				if e(user) == "cmp_lf1" {
					cmp_lf1 0 `b' `varname'
				}
				else {
					tempname t
					cmp_d1 0 `b' `t'
					replace `varname' = _cmp_lnfi if e(sample) & `touse'
				}
			}
			cmp_clear
		}
		exit
	}
	
	if `"`_options'`pr'`residuals'`ystar'`e'"' == "" di as txt "(option xb assumed; fitted values)"

	tempname num_cuts cat
	mat `cat' = e(cat)
	mat `num_cuts' = e(num_cuts)

	tempvar L U phiL phiU PhiL PhiU condU condL xbinormalL_condL xbinormalL_condU xbinormalU_condL xbinormalU_condU binormalL_condL binormalL_condU binormalU_condL binormalU_condU denom
	tokenize `_varlist'
	for`=cond("`_eqspec'"=="", "values eq=1/`e(k_`=cond("`scores'"=="", "dv", "eq")')'", "each eq in `=subinstr("`_eqspec'","#","",.)'")' {
		local depvar: word `eq' of `e(depvar)'
		if `"`pr'`residuals'`ystar'`e'"' == "" {
			quietly `e(cmdline)' predict // run cmp front end to reconstruct necessary _cmp_* variables
			Predict `vartype' `1' if `touse', `_options' eq(#`eq') `offset' `reducedform'
			cmp_clear
		}
		else {
			Predict double `xb' if `touse', `_options' eq(#`eq') `offset' `reducedform'
			if "`residuals'" != "" {
				gen `vartype' `1' = `depvar' - `xb' if `touse'
				label var `1' Residuals
			}
			else {
				scalar `sig' = sqrt(`Sigma'[`eq',`eq'])
				if `"`condition'"'!="" {
					scalar `rho' = `Sigma'[`eq',`condeq'] / `condsig' / `sig'
					if `rho'==0 & !inlist("`condeq'", "", "`eq'") {
						di _n as res "Warning: conditioning equation #`condeq' for {cmd:e(`e')} option uncorrelated without outcome equation #`eq'."
						di "Conditioning information ignored."
					}
				}
				else scalar `rho' = 1
				if `"`ystar'`e'"' != "" {
					if `"`condition'"'=="" | `"`ll'`ul'"'==".." { // use simpler formula if conditioning or dependent varunbounded or the two are uncorrelated, or conditioning variable undeclared
						local cond = cond(`"`condition'"'!="", "cond", "")
						qui gen double `L' = ((``cond'll')-``cond'xb')/``cond'sig' if `touse'
						qui gen double `U' = ((``cond'ul')-``cond'xb')/``cond'sig' if `touse'
						qui gen double `phiL' = cond(`L'>=., 0, normalden(`L'))    if `touse'
						qui gen double `phiU' = cond(`U'>=., 0, normalden(`U'))    if `touse'
						qui gen double `PhiL' = cond(`L'>=., 0, normal(   `L'))    if `touse'
						qui gen double `PhiU' = cond(`U'>=., 1, normal(   `U'))    if `touse'
						if `"`e'"'!="" gen `vartype' `1' = `xb' - cond(`"`condition'"'=="", `sig', `rho'*`sig') * (`phiU'-`phiL')/(`PhiU'-`PhiL') if `touse'
						else           gen `vartype' `1' = (`PhiU'-`PhiL')*`xb'-`sig'*(`phiU'-`phiL')+cond((`ll')>=.,0,`PhiL'*(`ll'))+cond((`ul')>=.,0,(1-`PhiU')*(`ul')) if `touse'
						drop `L' `U' `phiL' `phiU' `PhiL' `PhiU'
					}
					else {
						qui gen double `L' = ((`ll')-`xb')/`sig' if `touse'
						qui gen double `U' = ((`ul')-`xb')/`sig' if `touse'
						qui gen double `condU' = ((`condul')-`condxb')/`condsig' if `touse'
						qui gen double `condL' = ((`condll')-`condxb')/`condsig' if `touse'
						xbinormal `U' `condU' + + `rho' `xbinormalU_condU'       if `touse'
						xbinormal `U' `condL' + - `rho' `xbinormalU_condL'       if `touse'
						xbinormal `L' `condU' - + `rho' `xbinormalL_condU'       if `touse'
						xbinormal `L' `condL' - - `rho' `xbinormalL_condL'       if `touse'
						 binormal `U' `condU' + + `rho'  `binormalU_condU'       if `touse'
						 binormal `U' `condL' + - `rho'  `binormalU_condL'       if `touse'
						 binormal `L' `condU' - + `rho'  `binormalL_condU'       if `touse'
						 binormal `L' `condL' - - `rho'  `binormalL_condL'       if `touse'

						if `"`e'"'!="" {
							gen `vartype' `1' = `xb' - `sig'*(`xbinormalU_condU' - `xbinormalU_condL' - `xbinormalL_condU' + `xbinormalL_condL') ///
																			                   /  ( `binormalU_condU' - `binormalU_condL' - `binormalL_condU' + `binormalL_condL') if `touse'
						}
						else {
							qui gen double `denom' = cond(`condU'>=., 1, normal(`condU')) - cond(`condL'>=., 0, normal(`condL')) if `touse'
							gen `vartype' `1' = `xb' + `sig'*(`xbinormalU_condL' + `xbinormalL_condU' -`xbinormalU_condU' - `xbinormalL_condL' ///
						                                    + `L' * (`binormalL_condU' - `binormalL_condL') ///
																								+ `U' * (`denom' - `binormalU_condU' + `binormalU_condL')) ///
																									/ `denom' if `touse'
							drop `denom'
						}
						drop `L' `U' `condU' `condL' `xbinormalL_condL' `xbinormalL_condU' `xbinormalU_condL' `xbinormalU_condU' `binormalL_condL' `binormalL_condU' `binormalU_condL' `binormalU_condU'
					}
					label var `1' "E(`depvar'`=cond(`"`e'"'=="","*","")'`=cond(`lmissing' & `umissing', "", "|`=cond(`lmissing', "", "`ll'<")'`depvar'`=cond(`umissing', "", "<`ul'")'")')"
				}
				else if "`pr'" != "" {
					local num_cats = `num_cuts'[`eq',1] + 1
					if `num_cats' > 1 {
						if `"`outcome'"' == "" {
							_stubstar2names `1'_*, nvars(`num_cats') outcome
							forvalues outno=1/`num_cats' {
								condpr `xb' `rho' `vartype' `1'_`outno' if `touse', condll(`condll') condul(`condul') condxb(`condxb') condsig(`condsig') ///
									ll(`=cond(`outno'>1, "[cut_`eq'_`=`outno'-1']_cons", ".")') ///
									ul(`=cond(`outno'<=`num_cuts'[`eq',1], "[cut_`eq'_`outno']_cons", ".")') 
								label var `1'_`outno' "Pr(`depvar'=`=`cat'[`eq', `outno']')"
							}
						}
						else {
							if substr(`"`outcome'"', 1, 1) == "#" {
								local outcome = substr(`"`outcome'"', 2, .)
								if `outcome' > `num_cats' {
									di as err `"There is no outcome #`outcome'. There are only `num_cats' outcomes for equation #`eq'."'
									exit 111
								}
							}
							else {
								local i 1
								while `i' <= `num_cats' & `cat'[`eq', `i']!=`outcome' {
									local ++i
								}
								if `i' > `num_cats' {
									di as error `"Outcome `outcome' not found in equation `eq'. outcome() must either be a value of `depvar' or #1, #2, ..."'
									exit 111
								}
								local outcome `i'
							}
							condpr `xb' `rho' `vartype' `1' if `touse', condll(`condll') condul(`condul') condxb(`condxb') condsig(`condsig') ///
								ll(`=cond(`outcome'>1, "[cut_`eq'_`=`outcome'-1']_cons", ".")') ///
								ul(`=cond(`outcome'<=`num_cuts'[`eq',1], "[cut_`eq'_`outcome']_cons", ".")')
							label var `1' "Pr(`depvar'=`=`cat'[`eq', `outcome']')"
						}
					}
					else if `"`outcome'"' == "" {
						if `"`condition'"'=="" & `"`pr'"'=="0 ." {
							gen `vartype' `1' = normal(`xb')
							label var `1' "Pr(`depvar')"
						}
						else condpr `xb' `rho' `vartype' `1' if `touse', ll(`ll') ul(`ul') condll(`condll') condul(`condul') condxb(`condxb') condsig(`condsig')
					}
					else {
						di as err "Equation #`eq' is not ordered probit. outcome() is not allowed."
						exit 197
					}
				}
			}
			drop `xb'
		}
		macro shift
	}
end

cap program drop Predict
program Predict, eclass
	version 10.0
	cap version 11.0
	
	syntax anything [if], eq(string) [reducedform *]
	local hasGamma = e(k_gamma) & "`options'"!="scores"
	local reducedform = "`reducedform'"!=""
	if `hasGamma' {
		tempname b V N hold hold2 _p
		if `reducedform'==0 {
			mat `b' = e(b)
			mat colnames `b' = `e(params)'
			mat `b' = `b'[1,`"`:word `=substr("`eq'", 2, .)' of `e(eqnames)'':"']
			local reducedform = strpos("`:colnames `b''", "#")
		}
		if `reducedform' {
			mat `b' = e(br)
			mat `V' = e(Vr)
			local colnamesr: colnames e(br)
			forvalues i=1/`e(k)' { // if margins has hacked e(b) colnames to point to temporary vars, copy substitutions to e(br)
				local colnamesr: subinstr local colnamesr "`:word `i' of `:colnames e(bs)''" "`:word `i' of `:colnames e(b)''", all word
			}
			scalar `N' = e(N)
			_estimates hold `hold'
			ereturn post `b' `V', obs(`=`N'') esample(`hold') // pass around sample marker without duplicating it
			mat `b' = e(b)
			mat colnames `b' = `colnamesr'
			ereturn repost b=`b', rename
		}
		else {
			_estimates hold `hold', copy
			mat `b' = e(b)
			mat `V' = e(V)
			mata `_p' = st_matrix("e(_p)"); st_matrix("`b'", st_matrix("`b'")[`_p']); st_matrix("`V'", st_matrix("`V'")[`_p',`_p'])
			mata mata drop `_p'
			ereturn repost b=`b' V=`V'
			mat `b' = e(b)
			mat colnames `b' = `e(params)'
			ereturn repost b=`b', rename
		}
	}
	
	qui ml_p `anything' `if', `options' equation(`eq') `=cond("`options'"=="scores", "missing", "")'

	if `hasGamma'`reducedform' {
		_estimates hold `hold2'
		_estimates unhold `hold'
		ereturn repost, esample(`hold2')
	}
end

// Call binormal, dealing with missing coordinates
// Arguments: x y xsign ysign rho newvarname [if]
// xsign, ysign = +/-, indicating whether to interpret . as +/-infinity
cap program drop binormal
program define binormal
	args x y xsign ysign rho newvarname
	syntax anything [if]
	qui gen double `newvarname' = cond(`x'>=., ///
	                                     cond("`xsign'"=="-", ///
                                              0, ///
                                              cond(`y'>=.,  ///
                                                "`ysign'"=="+", ///
																						 normal(`y') ///
																					     ) ///
																		    ), ///
                                        cond(`y'>=., ///
																		       cond("`ysign'"=="-", ///
																					        0, ///
																		              normal(`x') ///
																					     ), ///
																					 binormal(`x',`y',`rho') ///
																		    ) ///
                                    ) `if'
end

// Integral of xPr[x,y] over quarter plane, corresponding to binormal()'s integral of Pr[x,y] over quarter plane
// For efficiency, returns the negative of the integral
// Arguments: x y xsign ysign rho newvarname [if]
// xsign, ysign = +/-, indicating whether to interpret . as +/-infinity
// Equation can be found in Rosenbaum (1961), JRSS B, eq 1.
cap program drop xbinormal
program define xbinormal
	args x y xsign ysign rho newvarname
	syntax anything [if]
	tempname c
	scalar `c' = 1/sqrt(1-`rho'*`rho')
	qui gen double `newvarname' = cond(`x'>=., ///
	                                     cond(-("`xsign'"=="-"), ///
                                              0, ///
													                 cond(`y'>=.,  ///
																					        0, ///
																									`rho'*normalden(`y') ///
																					     ) ///
																		    ), ///
                                        cond(`y'>=., ///
																		       cond(-("`ysign'"=="-"), ///
																					        0, ///
																		              normalden(`x') ///
																					     ), ///
																					 normalden(`x')*normal((`y'-`rho'*`x')*`c') + `rho'*normalden(`y')*normal((`x'-`rho'*`y')*`c') ///
																		    ) ///
                                    ) `if'
end

// compute Pr[a<x<b | c<z<d]
cap program drop condpr
program define condpr
	args xb rho newvartype newvarname
	syntax anything [if], ll(string) ul(string) [condll(string) condul(string) condxb(string) condsig(string)]
	tempvar L U condL condU binormalL_condL binormalL_condU binormalU_condL binormalU_condU
	qui gen double `L' = (`ll')-`xb' `if'
	qui gen double `U' = (`ul')-`xb' `if'
	if `"`condll'"' == "" {
		gen `newvartype' `newvarname' = cond(`U'>=., 1, normal(`U')) - cond(`L'>=., 0, normal(`L')) `if'
	}
	else {
		qui gen double `condL' = ((`condll')-`condxb')/`condsig' `if'
		qui gen double `condU' = ((`condul')-`condxb')/`condsig' `if'
		binormal `U' `condU' + + `rho' `binormalU_condU' `if'
		binormal `U' `condL' + - `rho' `binormalU_condL' `if'
		binormal `L' `condU' - + `rho' `binormalL_condU' `if'
		binormal `L' `condL' - - `rho' `binormalL_condL' `if'
		gen `newvartype' `newvarname'  = (`binormalU_condU' - `binormalU_condL' - `binormalL_condU' + `binormalL_condL') ///
											                  / (cond(`condU'>=., 1, normal(`condU')) - cond(`condL'>=., 0, normal(`condL'))) `if'
	}
end
