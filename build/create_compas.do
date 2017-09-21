* main program to generate all inputs for COMPAS

clear all
capture log close
set more off

* place yourself at the root of compas
cd ~/compas

* create initial population

do do/initialize/main.do

* create new cohorts
do do/newcohorts/main.do



* adjust transition models
do do/transit/main.do

* construct trends
do do/trends/main.do

* clean up temp directory
erase temp/*
*Create archive for git
*!tar -jcvf  startpop.tbz2 startpop*.csv sizeofstartpop*.csv
*!tar -jcvf  newcohort.tbz2 newcohort*.csv
