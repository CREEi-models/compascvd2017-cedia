#!/bin/bash
for (( c=1; c<=$2; c++ ))
do
time stata-mp -b do readpop $1 $c &
done



