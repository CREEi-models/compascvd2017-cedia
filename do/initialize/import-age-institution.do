*********************************************************
*							*							*
* Nombre de personnes en institution rescencement 2011  *  
*							*							*
* Importation du fichier csv provenant de StatCan	*
* Source : Statistics Canada, 2011 Census of Population,*
* Statistics Canada Catalogue no. 98-313-XCB2011024.    *
*							*							*
*********************************************************

clear all
cd ~/compas
import delimited "data/census/census_age_institution.csv", delimiter(";")

encode province, gen(prov)
drop province
recode prov (4=46) (3=59) (2=48) (9=47) (10=11) (7=35) (6=13) (1=10) (5=13) (8=24)
label define prov ///
		   10  "Terre-Neuve-et-Labrador" ///
		   11 "ele-du-Prince-edouard" ///
		   12 "Nouvelle-ecosse" ///
       13 "Nouveau-Brunswick" ///
		   14 "Atlantic" ///
		   24 "Quebec" ///
		   35 "Ontario" ///
		   46 "Manitoba" ///
		   47 "Saskatchewan" ///
		   48 "Alberta" ///
		   49 "Prairie" ///
                   59 "Colombie-Britanique" , modify

gen other = other7 + correctionalandpenalinstitutions + shelters4+ ///
		 grouphomesforchildrenandyouth + servicecollectivedwellings5 + lodgingandroominghouses + ///
		 hotelsmotelsandotherestablishmen + otherservicecollectivedwellings6 +religiousestablishments+ ///
		 hutteritecolonies

drop other7 correctionalandpenalinstitutions shelters4 ///
		 grouphomesforchildrenandyouth servicecollectivedwellings5 lodgingandroominghouses ///
		 hotelsmotelsandotherestablishmen otherservicecollectivedwellings6 religiousestablishments ///
		 hutteritecolonies

rename  (totaltypeofcollectivedwelling healthcareandrelatedfacilities2 generalandspecialityhospitals ///
		 nursinghomeschroniccareandlongte residencesforseniorcitizens grouphomesorinstitutionsfortheph ///
		 grouphomesorinstitutionsforpeopl )  ///
		 ///
		 (alltype h_facility gen_spec_hosp nursing_chronic_ltc res_senior inst_physical inst_psychiatric)


encode age, gen(group_age)
drop age
recode group_age (17 = 100) (16=14) (1=15) (2=20) (3=25) (4=30) (5=35) (6=40) (7=45) (8=50) (9=55) (10=60) ///
                 (11=65) (12=70) (13=75) (14=80) (15=85)
label define group_age ///
		   15  "15 to 19 years" ///
           20  "20 to 24 years" ///
           25  "25 to 29 years" ///
           30  "30 to 34 years" ///
           35  "35 to 39 years" ///
           40  "40 to 44 years" ///
           45  "45 to 49 years" ///
           50  "50 to 54 years" ///
           55  "55 to 59 years" ///
          60  "60 to 64 years" ///
          65  "65 to 69 years" ///
          70  "70 to 74 years" ///
          75  "75 to 79 years" ///
          80  "80 to 84 years" ///
          85  "85 years and over" ///
          14  "under 15 years" ///
          100 "All years" , modify


order   prov sexe group_age alltype h_facility gen_spec_hosp nursing_chronic_ltc res_senior inst_physical inst_psychiatric other

tempfile temp1
preserve
keep if prov>=10 & prov <=13
collapse (sum) alltype h_facility gen_spec_hosp nursing_chronic_ltc res_senior inst_physical inst_psychiatric other, by(sex group_age)
gen prov = 14
save "`temp1'", replace
restore

tempfile temp2
preserve
keep if prov>=46 & prov <=48
collapse (sum) alltype h_facility gen_spec_hosp nursing_chronic_ltc res_senior inst_physical inst_psychiatric other, by(sex group_age)
gen prov = 49
save "`temp2'", replace
restore

append using "`temp1'"
append using "`temp2'"
save "data/census/census-pop-institution.dta",replace
