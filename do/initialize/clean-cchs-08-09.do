*- cleaning cchs 2008 2009
use data/cchs/data-cchs-08-09, clear

set more off
* create id
rename adm_rno id
label var id "id of record"

* recode provinces
recode geo_prv (10/13=1) (24=2) (35=3) (46/48=4) (59=5) (else=.), gen(province)
label def province 1 "atlantic" 2 "quebec" 3 "ontario" 4 "prairies" 5 "bc"
label val province province

rename dhhgage ageg
* keep those age 30+
replace ageg = 40 + 5*(ageg)
recode ageg (85=80)

rename dhh_sex sex
replace sex = sex-1
label def sex 0 "male" 1 "female"
label values sex sex
tab sex

rename hwtgbmi bmi
recode bmi (min/30=1) (30/35=2) (35/60=3) (60/max=.)
label def bmi 1 "less 30" 2 "30-35" 3 "35-60"
label values bmi bmi
tab bmi

label def yesno 0 "no" 1 "yes"

*ccc_071         byte   %11.0g      CCC_071    Has high blood pressure
*ccc_072         byte   %11.0g      CCC_072    Ever diagnosed with high blood pressure
rename ccc_072 hibpe
replace hibpe = 1 if ccc_071 == 1
recode hibpe (6/9=.) (2=0)
label values hibpe yesno
tab hibpe

*ccc_091         byte   %11.0g      CCC_091    Has chronic bronchitis, emphysema or chronic obstructive pulmonary
*                                                disease (COPD)
rename ccc_91a lungea
recode lungea (2=0) (7/9=.)
label values lungea yesno
tab lungea, m

rename ccc_91e lungee
recode lungee (2=0) (7/9=.)
label values lungee yesno
tab lungee, m

rename ccc_91f lungef
recode lungef (2=0) (7/9=.)
label values lungef yesno
tab lungef, m

gen lunge=.
replace lunge = 1 if [lungea == 1 | lungee == 1 | lungef == 1]
replace lunge = 0 if [lungea == 0 & lungee == 0 & lungef == 0]
label values lunge yesno
tab lunge, m



*ccc_101         byte   %11.0g      CCC_101    Has diabetes
rename ccc_101 diabe
recode diabe (2=0) (6/9=.)
label values diabe yesno
tab diabe

*ccc_121         byte   %11.0g      CCC_121    Has heart disease
rename ccc_121 hearte
recode hearte (2=0) (6=0) (7/9=.)
label values hearte yesno
tab hearte
*ccc_131         byte   %11.0g      CCC_131    Has cancer
*ccc_31a         byte   %11.0g      CCC_31A    Ever had cancer

rename ccc_131 cancre
recode cancre (2=0) (6=0) (7/9=.)
label values cancre yesno
tab cancre
* ccc_151         byte   %11.0g      CCC_151    Suffers from the effects of a stroke
rename ccc_151 stroke
recode stroke (2=0) (6=0) (7/9=.)
label values stroke yesno
tab stroke
/*
doadl           byte   %8.0g       DOADL      Flag: module ADL (activities of daily living)
adl_01          byte   %11.0g      ADL_01     Needs help preparing meals
adl_02          byte   %11.0g      ADL_02     Needs help getting to appointments or running errands
adl_03          byte   %11.0g      ADL_03     Needs help doing housework
adl_04          byte   %11.0g      ADL_04     Needs help with personal care
adl_05          byte   %11.0g      ADL_05     Needs help moving about inside house
adl_06          byte   %11.0g      ADL_06     Needs help looking after personal finances
*/
tab ial_04a
rename ial_04a iadl_01
recode iadl_01 (2=1) (1=0) (7/9=.)
label values iadl_01 yesno
tab iadl_01

tab ial_03a
rename ial_03a iadl_02
recode iadl_02 (2=1) (1=0) (7/9=.)
label values iadl_02 yesno
tab iadl_02

tab ial_05a
rename ial_05a iadl_03
recode iadl_03 (2=1) (1=0) (7/9=.)
label values iadl_03 yesno
tab iadl_03

tab adl_06a
rename adl_06a adl_01
recode adl_01 (2=1) (1=0) (7/9=.)
label values adl_01 yesno
tab adl_01

tab adl_04a
rename adl_04a adl_02
recode adl_02 (2=1) (1=0) (7/9=.)
label values adl_02 yesno
tab adl_02, m

egen adl = anycount(adl_01 adl_02), values(1)
replace adl=. if adl_01==. & adl_02==.
tab adl
recode adl (0=0) (1/2=1)
label def adl 0 "none" 1 "one+"
label values adl adl
label var adl "adl count (type of adl inconsitent with ENSP)"
tab adl

egen iadl = anycount(iadl_01 iadl_02 iadl_03), values(1)
replace iadl=. if iadl_01==. & iadl_02==.& iadl_03==.
tab iadl
recode iadl (0=0) (1/3=1)
label def iadl 0 "none" 1 "one+"
label values iadl iadl
label var iadl "iadl count (type of iadl inconsitent with ENSP)"
tab iadl

gen cognitive = (huidcog==5 | huidcog==6) if huidcog!=99
label values cognitive yesno
label var cognitive "cognitive problem: Very forgetful, great deal of difficulty thinking, or unable to remember or think"

*Disability
gen inv=.

replace inv=1 if cognitive==0 & adl==0 &iadl==0
replace inv=2 if cognitive==1 & adl==0 &iadl==0
replace inv=3 if cognitive==0 & adl==0 &iadl==1
replace inv=4 if cognitive==0 & adl==1 &iadl==0
replace inv=5 if cognitive==1 & adl==0 &iadl==1
replace inv=7 if cognitive==1 & adl==1 &iadl==0
replace inv=6 if cognitive==0 & adl==1 &iadl==1
replace inv=7 if cognitive==1 & adl==1 &iadl==1

label def inv 1 "cognitive=0 & adl=0 & iadl=0" 2 "cognitive=1 & adl=0 & iadl=0" ///
3 "cognitive=0 & adl=0 & iadl=1" 4 " cognitive=0 & adl=1 &iadl =0" 5 "cognitive=1 & adl=0 & iadl=1" ///
6 " cognitive=0 & adl=1 & iadl=1" 7 "cognitive=1 & adl=1 &iadl =1"
label values inv inv
label var inv "invalidity status"


/*
smk_01a         byte   %11.0g      SMK_01A    Ever smoked 100 or more cigarettes in lifetime
smk_01b         byte   %11.0g      SMK_01B    Ever smoked a whole cigarette
smkg01c         byte   %17.0g      SMKG01C    Grouped: age when smoked first whole cigarette
smk_202         byte   %14.0g      SMK_202    Type of smoker
smkg203         byte   %17.0g      SMKG203    Grouped: daily smoker: age when started smoking daily
smk_204         int    %13.0g      SMK_204    Daily smoker: number of cigarettes smoked per day
smk_05b         int    %13.0g      SMK_05B    Occasional smoker: number of cigarettes smoked per day
smk_05c         byte   %11.0g      SMK_05C    Occasional smoker: number of days when smoke 1 cigarette or more
smk_05d         byte   %11.0g      SMK_05D    Ever smoked cigarettes daily
smk_06a         byte   %32.0g      SMK_06A    Never daily smoker: how long ago stopped smoking
smkg06c         byte   %16.0g      SMKG06C    Grouped: number of years since stopped smoking
smkg207         byte   %17.0g      SMKG207    Grouped: former daily smoker: age started smoking daily
smk_208         int    %13.0g      SMK_208    Former daily smoker: number of cigarettes smoked per day
smk_09a         byte   %32.0g      SMK_09A    Former daily smoker: how long ago stopped smoking daily
smkg09c         byte   %16.0g      SMKG09C    Grouped: years since stopped smoking daily
smk_10          byte   %11.0g      SMK_10     Former daily smoker: was when quit smoking completely
smk_10a         byte   %32.0g      SMK_10A    Former daily smoker: how long ago stopped smoking completely
smkg10c         byte   %16.0g      SMKG10C    Grouped: number of years since stopped smoking (daily)
smkdsty         byte   %39.0g      SMKDSTY    Derived: type of smoker
smkgstp         byte   %16.0g      SMKGSTP    Derived, grouped: number of years stop smoking completely
smkdycs         int    %18.0g      SMKDYCS    Derived: current daily smoker: number of years smoked
*/
rename smkdsty smoke
recode smoke (99=.) (6=1) (4/5=3) (1/3=2)
label def smoke 1 "never smoked" 2 "current smoker"3 "former smoker"
label values smoke smoke
tab smoke

*sdcfimm         byte   %11.0g      SDCFIMM    Flag: immigrant
rename sdcfimm imm
recode imm (2=0) (9=.)
label values imm yesno
tab imm

*edudr04         byte   %37.0g      EDUDR04    Derived: highest level of education of respondent (4 levels)
rename edudr04 educ4
tab educ4, m
recode educ4 (1=1) (2/3=2) (4=3) (9=.)
drop if educ4==.
label def educ4 1 "less HS" 2 "HS" 3 "College" 4 "University"
label values educ4 educ4
tab educ4

* master weight
rename wts_m wgt
label var wgt "weight"
replace wgt = round(wgt)

* in2ghh: revenu

rename in2ghh income
mvdecode income, mv(9=.)

#d ;
mlogit income sex ageg imm province educ4 dhhgms dhhghsz dhhgdwe
gend08 in2g13 lbfg31 lbf_37a lbfgocg in2g02  ownften [fweight = wgt];
#d cr
predict p0 p1 p2 p3 p4
gen cp1 = p0+p1
gen cp2 = cp1+p2
gen cp3 = cp2+p3
set seed 1234
gen rand=runiform()

gen i_income=.


bysort id: replace i_income = 1 if rand<=p0
bysort id: replace i_income = 2 if rand>p0 & rand<=cp1
bysort id: replace i_income = 3 if rand>cp1 & rand<=cp2
bysort id: replace i_income = 4 if rand>cp2 & rand<=cp3
bysort id: replace i_income = 5 if rand>cp3 & rand<=1

replace income = i_income if income==.


*Long-term care

gen care=.
foreach var of varlist cr1_01a cr1_01b cr1_01c cr1_01d cr1_01e cr1_01f cr1_01h cr2_01aa  cr2_01ab  cr2_01ac  cr2_01ad  cr2_01ae  cr2_01af  cr2_01ah {

 replace care=1 if `var'==1

 }
replace care =0 if cr1_01g==1 & cr2_01ag==1
gen ltc =care
recode ltc (0=1) (1=2)


#d ;
keep id ageg province sex bmi hibpe lunge diabe hearte cancre stroke
 inv smoke imm educ4 income ltc wgt;
order id ageg province sex bmi hibpe lunge diabe hearte cancre stroke
 inv smoke imm educ4 income ltc wgt;

 qui reg id ageg province sex bmi hibpe lunge diabe hearte cancre stroke
 inv smoke imm educ4 ltc wgt;

keep if e(sample)==1;
#d cr

sum


save temp/clean-cchs-08-09, replace
