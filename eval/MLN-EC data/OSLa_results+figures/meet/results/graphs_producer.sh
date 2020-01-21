#!/bin/bash
#
# Produces graphs that compare the result of the MLN-EC to the Ground Truth.
#
# Graphs are produced using ScalaTIKZ. ScalaTIKZ is an open-source library for PGF/TIKZ
# vector graphics, developed by Evangelos Michelioudakis (https://github.com/vagmcs/ScalaTIKZ)
#
# author: Christos Vlassopoulos

for outfile in $( find . -type f -name "*cv.out" )
do
	cat ${outfile} > aux.csv
	scalatikz	-i aux.csv -P -c "black" -x "T" -y "P"\
			-i aux.csv -P -c "blue" -x "T" -y "GT"\
			-i aux.csv -P -c "black" -b "dashed" -x "T" -y "Th"\
			-t "Comparison between predicted and ground truth probabilistic intervals"\
			-X "Time"\
			-Y "LTA probability"\
			-n ${outfile}.graph
done

rm -f aux.csv

