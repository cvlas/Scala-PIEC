#! /usr/bin/python
#title           :piec.py
#description     :PIEC(Probabilistic Interval-based Event calculus) computes probabilistic maximal intervals of a long-term activity(LTA).
#author          :Evangelos Makris
#date            :19/07/2018
#version         :0.1
#usage           :./piec.py
#===============================================================================================================================================

import numpy as np
import sys


def getCredible(tuples,prefixInput):

	print(prefixInput)			# tin peiraksa
	if not tuples:
		return(tuples)

	if len(tuples) == 1:
		return(tuples)

	overlap = list()
	currentValue = tuples[0][1]
	currentInterval = tuples[0]
	max_cred = prefixInput[tuples[0][1]] - prefixInput[tuples[0][0]-1]
	print("Tora imaste sto ")		# tin peiraksa
	print(tuples[0])			# tin peiraksa
	print(", me krentimpiliti ")		# tin peiraksa
	print(max_cred)				# tin peiraksa
	print(" logo epidi ")			# tin peiraksa
	print(prefixInput[tuples[0][1]])	# tin peiraksa
	print(" mion ")				# tin peiraksa
	print(prefixInput[tuples[0][0]-1])	# tin peiraksa

	for i in range(1, len(tuples)):
		print(tuples[i])		# tin peiraksa
		if (tuples[i][0] < currentValue):
			print("Iparxi epikalipsi. Tora tha dume an ")	# tin peiraksa
			print((prefixInput[tuples[i][1]] - prefixInput[tuples[i][0]-1]))		# tin peiraksa
			print(" diladi: ")			# tin peiraksa
			print(prefixInput[tuples[i][1]])	# tin peiraksa
			print(" mion ")				# tin peiraksa
			print(prefixInput[tuples[i][0]-1])	# tin peiraksa
			print(" >= ")		# tin peiraksa
			print(max_cred)		# tin peiraksa
			if ((prefixInput[tuples[i][1]] - prefixInput[tuples[i][0]-1]) >= max_cred): 	# tin peiraksa
				print("Orea. To allaksame.")		# tin peiraksa
				max_cred = prefixInput[tuples[i][1]] - prefixInput[tuples[i][0]-1]
				currentInterval = tuples[i]
			print("Pamparakato")				# tin peiraksa	
			currentValue = tuples[i][1]
								
		else:
			overlap.append(currentInterval)
			currentInterval = tuples[i]
			currentValue = tuples[i][1]
			max_cred = prefixInput[tuples[i][1]] - prefixInput[tuples[i][0]-1]
			
			
			
	overlap.append(currentInterval)

	return(overlap)


def PIEC(threshold):
	(''' PIEC(Probabilistic Interval-based Event calculus) computes probabilistic maximal intervals of a long-term activity(LTA) of interest with respect to an user-defined 	  probability threshold given a list of instantaneous probabilities ''')

	#inputArray list initially contains the instantaneous probabilitites of the LTA.	
	inputArray = [0.0, 0.3, 0.3, 0.7, 0.7, 0.5, 0.1, 0.0, 0.0, 0.0]

	#prefixInput contains the prefix sums of inputArray list. 
	prefixInput = [None]*len(inputArray)
	prefixInput[0] = inputArray[0]
	for i in range(1,len(inputArray)):
		prefixInput[i] = prefixInput[i-1] + inputArray[i]
	
	
	#prefix list contains the prefix sums of (inputArray subtracted by the given threshold) list.
	prefix = [None]*len(inputArray)
	#dp[i] is the maximum prefix sum that can be reached from i to n-1, where n the lenght of inputArray.
	dp = [None]*len(inputArray)
	#result contains a list of tuples which represent closed intervals with the property that they are probabilistic maximal intervals before the selection with respect to 	credibility rate.
	result = list()
	#we subtract the user-defined threshold from the instantaneous probabilities.  
	inputArray[:] = [x-threshold for x in inputArray]


	prefix[0] = inputArray[0]
	for i in range(1,len(inputArray)):
		prefix[i] = prefix[i-1] + inputArray[i]


	dp[len(inputArray)-1] = prefix[len(inputArray)-1]
	for i in range(len(inputArray)-2,-1,-1):
		dp[i] = max(dp[i+1], prefix[i])


	dprange = 0
	start = 0
	end = 0
	flag1 = False


	while(end < len(inputArray) and start < len(inputArray)):
		if(start == 0):
			dprange = dp[end]
		else:
			dprange = dp[end] - prefix[start-1]
	

		if(round(dprange,6) >= 0):
			if (end == len(inputArray) -1 and start<end):
				result.append((start, end))
			if (end == len(inputArray) -1 and start==end and inputArray[start]>0):
				result.append((start, end))
		
			flag1 = True
			end += 1

		else:
			if (start < end and flag1):
				result.append((start, end-1))
			if (start == end and inputArray[start]>0):
				result.append((start, end))

			flag1 = False		
			start += 1
	#print(result)
	print(getCredible(result,prefixInput))
	




def main():
        threshold = float(raw_input("Insert a probability threshold in the range [0,1]: "))
        if(threshold < 0 or threshold > 1):
        	print("Oops! That was no valid number. Probability threshold should be in the range [0,1].")
	else:
		PIEC(threshold)
	



if __name__ == "__main__":
	main()
	

		


  
	
	

