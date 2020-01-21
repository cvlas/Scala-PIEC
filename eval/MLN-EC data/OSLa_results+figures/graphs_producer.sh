#!/bin/bash
#
# Produces Micro and Macro Precision/Recall/F-measure comparison graphs between
# PIEC and MLN-EC.
#
# Graphs are produced using ScalaTIKZ. ScalaTIKZ is an open-source library for PGF/TIKZ
# vector graphics, developed by Evangelos Michelioudakis ( https://github.com/vagmcs/ScalaTIKZ ).
#
# author: Christos Vlassopoulos

for hle in "meet" "move"
do
	for measure in "precision" "recall" "f-measure"
	do
		scalatikz	-i ./${hle}/piec_mln_comparison_${measure}.csv -d $'\t' -P -c "blue" -m square* -x Thr -y "PIEC1-Micro"\
				-i ./${hle}/piec_mln_comparison_${measure}.csv -d $'\t' -P -c "red" -m square* -x Thr -y "MLN-EC-Micro"\
				-t "${measure} for ${hle} LTA"\
				-X "Probability threshold"\
				-Y ${measure}\
				-a 0.0,1.0,0.0,1.0\
				-g "PIEC","MLN-EC (OSLa)"\
				-n osla_${hle}_${measure}_micro

		scalatikz       -i ./${hle}/piec_mln_comparison_${measure}.csv -d $'\t' -P -c "blue" -m square* -x Thr -y "PIEC1-Macro"\
        	                -i ./${hle}/piec_mln_comparison_${measure}.csv -d $'\t' -P -c "red" -m square* -x Thr -y "MLN-EC-Macro"\
				-t "${measure} for ${hle} LTA"\
        	                -X "Probability threshold"\
        	                -Y ${measure}\
				-a 0.0,1.0,0.0,1.0\
        	                -g "PIEC","MLN-EC (OSLa)"\
        	                -n osla_${hle}_${measure}_macro
	done
done

