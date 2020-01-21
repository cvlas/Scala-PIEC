#!/bin/bash
#
# Fixes timepoint difference between OSLa and DN results.
#
# author: Christos Vlassopoulos

for csvfile in $( find . -type f -name "*.marginal.csv" )
do
	video=$( echo $csvfile | cut -d"." -f1-2 | cut -d"/" -f2 | rev | cut -d"_" -f3- | rev )
	id_pair=$( echo $csvfile | cut -d"." -f1-2 | cut -d"/" -f2 | rev | cut -d"_" -f-2 | rev )
	dn_filename=${video}.${id_pair}.csv
	printf "Successfully built respective DN video filename: %s\n" ${dn_filename}

	cut -d"," -f2- $csvfile > $csvfile.part2
	cut -d"," -f1 ../../../DN_results+figures/move/results/${dn_filename} > $csvfile.part1
	paste -d"," $csvfile.part1 $csvfile.part2 > $csvfile.cv
done

