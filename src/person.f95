! COMPAS: module to define a person and a cohort

! defines a person
type person
    integer init
    integer id
    integer age
    integer year
    integer byear
    integer educ
    integer sex
    integer imm
    integer province
    integer diabe
    integer hibpe
    integer hearte
    integer stroke
    integer cancre
    integer lunge
    integer mentae
    integer idiabe
    integer ihibpe
    integer ihearte
    integer istroke
    integer icancre
    integer ilunge
    integer imentae
    integer smoke	
    integer bmi
    integer inv
    integer ltc
    integer income
    integer alive
    integer hc_nights
    integer hc_specialist
    integer hc_generalist
    integer hc_homecare_f
    integer hc_homecare_i
    integer hc_homecare_sp
    integer hc_drugs
    integer nyears
    integer nyears_disable
    integer nyears_disease
    integer nyears_nhome
    integer ydisable
    integer ydisease
    double precision hc_hui
    double precision cost_nights
    double precision cost_specialist
    double precision cost_generalist
    double precision hours_homecare_f
    double precision hours_homecare_i
    double precision hours_homecare_sp
    double precision cost_homecare_f
    double precision cost_drugs
    double precision cost_nhome
    double precision life_cost_nhome
    double precision life_disc_cost_nhome
    double precision wgt
end type person

! defines a cohort
type cohort
	type (person) , allocatable :: pop(:)
end type cohort

! Define total population
type totpop
	type (person), allocatable :: pop(:)
end type totpop

integer, allocatable :: ntotpop(:)
