package piec

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

                var currentInterval = intervals.head
                val currentStart = currentInterval._1
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
        val prefixInput = new Array[Double](n)
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

            // Construct prefixInput array, based on a
            prefixInput(i) = a(i)

            // Construct prefix array, based on l
            prefix(i) = l(i)
			for (j <- 0 until i)
            {
                prefixInput(i) += a(j)
                prefix(i) += l(j)
            }
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

                    //println(s"1:Added interval ${(start, end)}, now output is $output\n")
                }
				
				flag = true
				end += 1
            }
			else
			{
				if (start < end && flag)
				{
                    output += ((start, end-1))

                    //println(s"2:Added interval ${(start, end-1)}, now output is $output\n")
				}
				if (start == end && a(start) == t)
				{
                    output += ((start, end))

                    //println(s"3:Added interval ${(start, end)}, now output is $output\n")
                }
				flag = false
				start += 1
			}
        }

        val result = getCredible(prefixInput, output)

        println(s"The most credible maximal intervals are ${result.mkString("[", ",", "]")}.")
	}

    val aa01 = Array(0.8, 0.0, 0.8, 0.0, 0.8, 0.0, 0.8, 0.0, 0.8, 0.0)
    val aa02 = Array(0.0, 0.9, 0.0, 0.9, 0.0, 0.9, 0.0, 0.9, 0.0, 0.9)
    val aa03 = Array(0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0)
    val aa04 = Array(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
    val aa05 = Array(1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
    val aa06 = Array(1.0, 0.8, 0.6, 0.4, 0.2, 0.2, 0.4, 0.6, 0.8, 1.0)
    val aa07 = Array(0.0, 0.3, 0.6, 0.9, 1.0, 0.0, 0.9, 0.6, 0.3, 0.0)
    val aa08 = Array(1.0, 0.7, 0.4, 0.1, 0.0, 1.0, 0.1, 0.4, 0.7, 1.0)
    val aa09 = Array(0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    val aa10 = Array(0.4, 0.4, 0.4, 0.4, 0.9, 0.4, 0.4, 0.4, 0.4, 0.4)
    val aa11 = Array(0.9, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.9)
    val aa12 = Array(1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0)
    val aa13 = Array(1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    val aa14 = Array(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.4, 0.9)
    val aa15 = Array(0.9, 0.4, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    val aa16 = Array(0.3, 0.6, 0.7, 0.7, 0.2, 0.4, 0.0, 0.1, 0.4, 0.9)
    val aa17 = Array(0.0, 0.3, 0.3, 0.7, 0.7, 0.5, 0.1, 0.0, 0.0, 0.0)
    val aa18 = Array(0.5, 0.3, 0.1, 0.0, 0.0, 0.0, 0.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.4, 0.5, 0.6, 0.7, 0.8 ,0.9, 0.9, 0.9, 0.9, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.7, 0.0)
    val aa19 = Array(0.5, 0.3, 0.1, 0.0, 0.0, 0.0, 0.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.4, 0.5, 0.6, 0.7, 0.8 ,0.9, 0.9, 0.9, 0.9, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.7)
    val aa20 = Array(0, 0.5, 0.7, 0.9, 0.4, 0.1, 0, 0, 0.5, 1)
    val aa21 = Array(0, 0.5, 0.5, 0.9, 0.4, 0.1, 0, 0, 0.5, 1)
    val aa22 = Array(0, 0.5, 0, 0.9, 0.4, 0.1, 0, 0, 0.5, 1)
    piec(aa01, 0.5)
    piec(aa02, 0.5)
    piec(aa03, 0.5)
    piec(aa04, 0.5)
    piec(aa05, 0.5)
    piec(aa06, 0.5)
    piec(aa07, 0.5)
    piec(aa08, 0.5)
    piec(aa09, 0.5)
    piec(aa10, 0.5)
    piec(aa11, 0.5)
    piec(aa12, 0.5)
    piec(aa13, 0.5)
    piec(aa14, 0.5)
    piec(aa15, 0.5)
    piec(aa16, 0.5)
    piec(aa17, 0.5)
    piec(aa18, 0.5)
    piec(aa19, 0.5)
    piec(aa20, 0.5)
    piec(aa21, 0.5)
    piec(aa22, 0.5)
}
