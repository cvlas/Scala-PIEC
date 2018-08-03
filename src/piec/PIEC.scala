package piec

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * @author Christos G. Vlassopoulos (cvlas@iit.demokritos.gr)
  *
  * 2018-07-27
  */

object PIEC extends App
{
    // TODO: Is there a more efficient implementation?
    def getCredible(probabilities: Array[Double], intervals: ListBuffer[(Int, Int)]) : ((Int, Int), Double) = {

        var intProb = new mutable.HashMap[(Int, Int), Double]()
        var credibility = 0.0

        var maxCred = -1.0
        var maxInt = (-1, -1)

        for ((start, end) <- intervals)             // For every maximal interval
        {
            println(s"Working on: ${(start, end)}...")
            credibility = 0.0                       // Reset credibility
            println(s"Currently, credibility is set to $credibility...")

            for (i <- start to end)                 // For every timepoint of that interval
            {
                credibility += probabilities(i)     // Calculate credibility as the sum of point probabilities
                println(s"Adding ${probabilities(i)}...")
            }

            // TODO: Check for possibly better rounding options
            intProb += (((start, end), BigDecimal(credibility).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble))
        }

        for (((s, e), cred) <- intProb)             // Find and return the most credible interval, along with its credibility
        {
            println(s"Fetching: ${((s, e), cred)}...")
            if (cred > maxCred)
            {
                println(s"cred > maxCred: $cred > $maxCred...")
                maxInt = (s, e)
                maxCred = cred
                println(s"Maximum credibility so far: ${((s, e), cred)}...")
            }
        }

        (maxInt, maxCred)
    }

    def piec(a: Array[Double], t: Double) : Unit = {

        val n = a.length
		val l = new Array[Double](n)
		val prefix = new Array[Double](n)
		val dp = new Array[Double](n)

        var (start, end) = (0, 0)
        var flag = false
        var output = new ListBuffer[(Int, Int)]()

        val dprange = Array.ofDim[Double](n, n)

		for (i <- a.indices)
        {
			// Construct l array, based on a
			// TODO: Check for possibly better rounding options
			l(i) = BigDecimal(a(i) - t).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

			// Construct prefix array, based on l
			prefix(i) = l(i)
			for (j <- 0 until i) prefix(i) += l(j)
			// TODO: Check for possibly better rounding options
			prefix(i) = BigDecimal(prefix(i)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
		}

        // Construct dp array, backwards, based on prefix
		for (k <- a.indices.reverse)
        {
			if (k == a.indices.last)
            {
                dp(k) = prefix(k)
            }
			else
            {
				if (prefix(k) > dp(k+1))
                {
                    dp(k) = prefix(k)
                }
				else dp(k) = dp(k+1)
			}
		}

        var stepCounter = 0

        while (start < n && end < n)
        {
            if (start > 0)
            {
                dprange(start)(end) = BigDecimal(dp(end) - prefix(start-1)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
            }
            else
            {
                dprange(start)(end) = dp(end)
            }

            if (dprange(start)(end) >= 0)
            {
                if ((end == n-1 && start < end) || (end == start && end == n-1 && a(start) >= t))
                {
                    output += ((start, end))

                    println(s"1:Added interval ${(start, end)}, now output is $output\n")
                }
				
				flag = true
				end += 1
            }
			else
			{
				if (start < end && flag)
				{
                    output += ((start, end-1))

                    println(s"2:Added interval ${(start, end-1)}, now output is $output\n")
				}
				if (start == end && a(start) == t)
				{
                    output += ((start, end))

                    println(s"3:Added interval ${(start, end)}, now output is $output\n")
                }
				flag = false
				start += 1
			}
        }

        val result = getCredible(a, output)

        println(s"The most credible maximal interval is ${result._1}, with total credibility ${result._2}")
	}

	val aa = Array(0.0, 0.3, 0.3, 0.7, 0.7, 0.5, 0.1, 0.0, 0.0, 0.0)
	piec(aa, 0.5)
}
