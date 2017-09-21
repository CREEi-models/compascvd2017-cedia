* main do-file to generate new cohorts model

clear all
capture log close
set more off
adopath + ado/plus

log using logs/newcohort.txt, replace text

* prepare data
do do/newcohorts/prepare.do

* estimate covariance matrix
do do/newcohorts/covariance.do


exit
