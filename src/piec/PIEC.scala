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
    def getCredible(prefix: Array[Double], intervals: ListBuffer[(Int, Int)]) : ListBuffer[(Int, Int)] =
    {
        if (intervals.isEmpty)
        {
            intervals
        }
        else
        {
            if (intervals.length == 1)
            {
                intervals
            }
            else
            {
                var credible = new ListBuffer[(Int, Int)]()

                var currentInterval = intervals(0)
                var currentStart = currentInterval._1
                var currentEnd = currentInterval._2
                var maxCredibility = 0.0

                // If interval begins at timepoint 0
                if (currentStart == 0)
                {
                    // Credibility should be equal to the prefix at the end
                    // TODO: Check for possibly better rounding options
                    maxCredibility = BigDecimal(prefix(currentEnd)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
                }
                else
                {
                    // Calculate credibility as a difference of prefixes
                    // TODO: Check for possibly better rounding options
                    maxCredibility = BigDecimal(prefix(currentEnd) - prefix(currentStart - 1)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
                }

                for (i <- 1 until intervals.length)
                {
                    if (intervals(i)._1 < currentEnd)
                    {
                        if ((prefix(intervals(i)._2) - prefix(intervals(i)._1 - 1)) > maxCredibility)
                        {
                            maxCredibility = prefix(intervals(i)._2) - prefix(intervals(i)._1 - 1)
                            currentInterval = intervals(i)
                        }

                        currentEnd = intervals(i)._2
                    }
                    else
                    {
                        credible += currentInterval
                        currentInterval = intervals(i)
                        currentEnd = intervals(i)._2
                        maxCredibility = prefix(intervals(i)._2) - prefix(intervals(i)._1 - 1)
                    }
                }

                credible += currentInterval

                credible
            }
        }
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

        //var stepCounter = 0

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

        val result = getCredible(prefix, output)

        println(s"The most credible maximal intervals are ${result.mkString("[", ",", "]")}.")
	}

	val aa = Array(0.0, 0.3, 0.3, 0.7, 0.7, 0.5, 0.1, 0.0, 0.0, 0.0)
	piec(aa, 0.5)
}
