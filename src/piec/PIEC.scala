package piec

/**
  * @author Christos G. Vlassopoulos (cvlas@iit.demokritos.gr)
  *
  * 2018-07-27
  */

object PIEC extends App
{

	def piec(a: Array[Double], t: Double): Unit = {

		val l = new Array[Double](a.length)
		val prefix = new Array[Double](a.length)
		val dp = new Array[Double](a.length)

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

		println(a.deep.mkString(", "))
		println(l.deep.mkString(", "))
		println(prefix.deep.mkString(", "))
		println(dp.deep.mkString(", "))
	}

	val aa = Array(0.0, 0.5, 0.7, 0.9, 0.4, 0.1, 0.0, 0.0, 0.5, 1.0)
	piec(aa, 0.5)
}
