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
    def getCredible(probabilities: Array[Double], intervals: ListBuffer[(Int, Int)]) : ((Int, Int), Double) = {

        var intProb = new mutable.HashMap[(Int, Int), Double]()
        var totalProb = 0.0

        var maxProb = -1.0
        var maxInt = (-1, -1)

        for ((start, end) <- intervals)
        {
            totalProb = 0.0     // reset

            for (i <- start to end)
            {
                totalProb += probabilities(i)
            }

            intProb += (((start, end), BigDecimal(totalProb).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble))
        }

        for (((s, e), pr) <- intProb)
        {
            if (pr > maxProb)
            {
                maxInt = (s, e)
                maxProb = pr
            }
        }

        (maxInt, maxProb)
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
                if ((end == n && start < end) || (end == start && end == n && a(start) >= t))
                {
                    output += ((start, end))
                }
				
				flag = true
				end += 1
            }
			else
			{
				if (start < end && flag)
				{
                    output += ((start, end - 1))
				}
				if (start == end && a(start) == t)
				{
                    output += ((start, end))
                }
				flag = false
				start += 1
			}
        }

        val result = getCredible(a, output)

		println(a.deep.mkString(", "))
		println(l.deep.mkString(", "))
		println(prefix.deep.mkString(", "))
		println(dp.deep.mkString(", "))
        println(start)
        println(end)
        println(output)

        for (dpr <- dprange)
        {
            println(dpr.deep)
        }

        println(s"\n\nThe most credible maximal interval is ${result._1}, with total credibility ${result._2}\n\n")
	}

	val aa = Array(0.0, 0.5, 0.7, 0.9, 0.4, 0.1, 0.0, 0.0, 0.5, 1.0)
	piec(aa, 0.5)
}
