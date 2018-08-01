package piec

import scala.collection.mutable.ListBuffer

/**
  * @author Christos G. Vlassopoulos (cvlas@iit.demokritos.gr)
  *
  * 2018-07-27
  */

object PIEC extends App
{

	def piec(a: Array[Double], t: Double): Unit = {

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
	
		for (k <- a.indices.reverse)                        // Construct dp array, backwards, based on prefix
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
                dprange(start)(end) = dp(end) - prefix(start-1)
            }
            else
            {
                dprange(start)(end) = dp(end)
            }

            if (dprange(start)(end) >= 0)
            {
                if ((end == n && start < end) || (end == start && end == n && a(start) >= t))
                {
                    //output += (start, end) // ERROR?
                }
				
				flag = true
				end += 1
            }
			else
			{
				if (start < end && flag == true)
				{
                    //output += (start, end - 1) // ERROR?
				}
				if (start == end && a(start) == t)
				{
                    //output += (start, end) // ERROR?
				}
				flag = false
				start += 1
			}
        }
		
		// return getCredible(output)

		println(a.deep.mkString(", "))
		println(l.deep.mkString(", "))
		println(prefix.deep.mkString(", "))
		println(dp.deep.mkString(", "))
        println(start)
        println(end)
        println(output)
        println(dprange.deep)
	}

	val aa = Array(0.0, 0.5, 0.7, 0.9, 0.4, 0.1, 0.0, 0.0, 0.5, 1.0)
	piec(aa, 0.5)
}
