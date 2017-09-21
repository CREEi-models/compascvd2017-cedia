* imputing mental illness from NHPS logit regression
preserve
use temp/clean-cchs.dta, clear
    * coefficients from NHPS
    scalar coef_mental_age0 = 0.0732084		    // splines d'ages
    scalar coef_mental_age75= 0.0241762
    scalar coef_mental_imm 	= -0.2701174	        // 0 : non-immig , 1:immigrant
    scalar coef_mental_sex	= -0.3175567  	    // sexe: 0:homme, 1: femme
    scalar coef_mental_adl		= 0.9106223		// 1 adl +
    scalar coef_mental_iadl		= 1.144055		// 1+ iadl
    scalar coef_mental_cogn		= 2.803349		// cognitive
    scalar coef_mental_inv3		= 1.524059		// inst
    scalar coef_mental_const= -9.31361

    * covariates
    gen adl = (inv==4 | inv==6  | inv==7) if inv!=.
    gen iadl = (inv==3 | inv==5  | inv==6| inv==7) if inv!=.
    gen cognitive = (inv==2 | inv==5  | inv==7) if inv!=.
    gen splineage0 = min(age,75)
    gen splineage75= max(age-75,0)

    * score
    gen xbeta_mental =	coef_mental_age0 	* splineage0  + ///
                        coef_mental_age75   * splineage75 + ///
                        coef_mental_imm	 	* imm 	      +	///
                        coef_mental_sex		* sex 		  +	///
			coef_mental_adl		* adl		+ ///
			coef_mental_iadl		* iadl		+ ///
			coef_mental_cogn		* cognitive		+ ///
                        coef_mental_inv3	* inv3	+ ///
                        coef_mental_const

    gen probmental = exp(xbeta_mental)/(1+exp(xbeta_mental))
    gen u = runiform()
    gen mentae =(u<=probmental)
    replace mentae = 0 if age <55
    replace stroke = 0 if age <40

    drop u probmental splineage* xbeta_* adl iadl cognitive
    label values mentae yesno
    label var mentae "Suffers from Alzheimer's Disease or other Dementia"
    save temp/clean-cchs.dta, replace
restore
