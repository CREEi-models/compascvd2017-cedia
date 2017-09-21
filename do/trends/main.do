capture log close
set more off
clear all


log using logs/trends.txt, replace text

* create trend for education (lfs)
do "do/trends/education.do"

* create trend for smoking (own)
do "do/trends/smoking.do"

* create trend for bmi (own)
do "do/trends/bmi.do"

* create trend in mortality (census)
do "do/trends/mortality.do"

* create series for population entering at age 30, projections (census)
do "do/trends/pop30.do"

* create series with size of population under 30, projections (census)
do "do/trends/popunder30.do"

* create series with trends in health care costs (icis)
do "do/trends/hcare_costs.do"

log close

*exit
