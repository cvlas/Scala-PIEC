package piec

import scala.collection.mutable.ListBuffer

/**
  * @author Christos G. Vlassopoulos (cvlas@iit.demokritos.gr)
  *
  * 2018-07-27
  */

object PIEC extends App
{
    /**
      * Takes a list of probabilistic maximal intervals and filters out all but
      * the credible ones.
      *
      * Credibility = the sum of the instantaneous probabilities
      *
      * @param prefix Array containing the progressive instantaneous probability sums
      * @param intervals the complete List of probabilistic maximal intervals
      * @return a List of credible probabilistic maximal intervals
      */
    def getCredible(prefix: Array[Double], intervals: ListBuffer[(Int, Int)]) : List[(Int, Int)] =
    {
        if (intervals.isEmpty)
        {
            intervals.toList
        }
        else
        {
            if (intervals.length == 1)
            {
                intervals.toList
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
                    maxCredibility = BigDecimal(prefix(currentEnd)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                }
                else
                {
                    // Calculate credibility as a difference of prefixes
                    // TODO: Check for possibly better rounding options
                    maxCredibility = BigDecimal(prefix(currentEnd) - prefix(currentStart - 1)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
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

                credible.toList
            }
        }
    }

    /**
      * The PIEC algorithm. It originally appears in "Artikis A., Makris E. and Paliouras G.,
      * A Probabilistic Interval-based Event Calculus for Activity Recognition.
      * In Annals of Mathematics and Artificial Intelligence, 2019".
      *
      * Implementation in Scala by Christos Vlassopoulos.
      *
      * @param a the array that contains all of the input instantaneous probabilities
      * @param t the desired probability threshold
      * @return credible probabilistic maximal intervals
      */
    def piec(a: Array[Double], t: Double) : List[(Int, Int)] =
    {
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
            l(i) = BigDecimal(a(i) - t).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

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
            prefix(i) = BigDecimal(prefix(i)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
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
                dprange(start)(end) = BigDecimal(dp(end) - prefix(start-1)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
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
                }

                flag = true
                end += 1
            }
            else
            {
                if (start < end && flag)
                {
                    output += ((start, end-1))
                }
                if (start == end && a(start) >= t)
                {
                    output += ((start, end))
                }
                flag = false
                start += 1
            }
        }

        println(output)

        getCredible(prefixInput, output)
    }

    /**
      * A sample run
      */
    println(piec(Array(1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0), 0.5))
}
