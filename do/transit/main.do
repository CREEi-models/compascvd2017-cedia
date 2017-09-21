clear all
capture log close
set more off

*log using logs/transit.txt, replace text

* calibrate mortality
do do/transit/calibrate-mortality.do
