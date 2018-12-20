package piec

import java.io.{File, FileWriter, PrintWriter}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

/**
  * @author Christos G. Vlassopoulos (cvlas@iit.demokritos.gr)
  *
  * 2018-07-27
  */

object PIEC_test extends App
{
    def formatGround(x: List[(Int, Int)]): Array[Int] =
    {
        var start = Array.fill[Int](25170)(0)

        for (i <- x)
        {
            val y = i._1
            start(y) = 1
        }

        for ((a,b) <- x)
        {
            var aa = a

            while (aa <= b)
            {
                val t = aa

                if (aa <= b)
                {
                    start(t) = 1
                }

                aa = aa + 1
            }
        }

        start
    }

    def probec_intervals(z: Array[Double], threshold: Double): Array[Int] =
    {
        // z.map(x => if (x >= threshold) 1 else 0)

        var allZeros = true

        var tmpArray = Array[Int]()

        for (x <- z)
        {
            if (x > 0.0)
            {
                allZeros = false
            }

            if (x >= threshold)
            {
                tmpArray = tmpArray :+ 1
            }
            else
            {
                tmpArray = tmpArray :+ 0
            }
        }

        if (allZeros)
        {
            //println(s"NO USE... ALL ZEROS!")
            //Thread.sleep(5000l)
        }

        tmpArray
    }

    def getCredible(tuples: List[(Int, Int)], prefix: Array[Double]) : Array[Int] =
    {
        if (tuples.isEmpty)
        {
            Array.fill[Int](25170)(0)
        }
        else
        {
            if (tuples.length == 1)
            {
                formatGround(tuples)
            }
            else
            {
                var overlap = new ListBuffer[(Int, Int)]()

                var tmp = 0
                var currentValue = tuples(0)._2
                var currentStart = tuples(0)._1
                var currentInterval = tuples(0)
                var maxCredibility = 0.0

                // If interval begins at timepoint 0
                if (currentStart == 0)
                {
                    // Credibility should be equal to the prefix at the end
                    // TODO: Check for possibly better rounding options
                    maxCredibility = BigDecimal(prefix(currentValue)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                }
                else
                {
                    // Calculate credibility as a difference of prefixes
                    // TODO: Check for possibly better rounding options
                    maxCredibility = BigDecimal(prefix(currentValue) - prefix(currentStart - 1)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                }

                for (i <- 1 until tuples.size)
                {
                    if (tuples(i)._1 < currentValue)
                    {
                        if (BigDecimal(prefix(tuples(i)._2) - prefix(tuples(i)._1 - 1)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble >= maxCredibility)
                        {
                            maxCredibility = BigDecimal(prefix(tuples(i)._2) - prefix(tuples(i)._1 - 1)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                            currentInterval = tuples(i)
                        }

                        currentValue = tuples(i)._2
                    }
                    else
                    {
                        overlap += currentInterval
                        currentInterval = tuples(i)
                        currentValue = tuples(i)._2
                        maxCredibility = BigDecimal(prefix(tuples(i)._2) - prefix(tuples(i)._1 - 1)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                    }
                }

                overlap += currentInterval

                formatGround(overlap.toList)
            }
        }
    }

    def piec(inputArray: Array[Double], threshold: Double) : Array[Int] =
    {
        val prefixInput = new Array[Double](inputArray.length)
        prefixInput(0) = inputArray(0)

        for (i <- 1 until inputArray.length)
        {
            prefixInput(i) = BigDecimal(prefixInput(i-1) + inputArray(i)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
        }

        val prefix = new Array[Double](inputArray.length)
        val dp = new Array[Double](inputArray.length)
        var result = ListBuffer[(Int, Int)]()

        for (x <- inputArray)
        {
            inputArray(inputArray.indexOf(x)) = BigDecimal(x - threshold).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
        }

        prefix(0) = BigDecimal(inputArray(0)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

        for (i <- 1 until inputArray.length)
        {
            prefix(i) = BigDecimal(prefix(i-1) + inputArray(i)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
        }

        dp(inputArray.length - 1) = prefix(inputArray.length - 1)

        for (i <- inputArray.length - 2 until -1 by -1)
        {
            dp(i) = BigDecimal(Seq(dp(i+1), prefix(i)).max).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
        }

        var dprange = 0.0
        var start = 0
        var end = 0
        var flag1 = false

        while (end < inputArray.length && start < inputArray.length)
        {
            if (start == 0)
                dprange = dp(end)
            else
                dprange = BigDecimal(dp(end) - prefix(start - 1)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

            if (dprange >= 0)
            {
                if (end == inputArray.length - 1 && start < end)
                    result += ((start, end))
                if (end == inputArray.length - 1 && start == end && inputArray(start) > 0)
                    result += ((start, end))

                flag1 = true
                end = end + 1
            }
            else
            {
                if (start < end && flag1)
                    result += ((start, end - 1))
                if (start == end && inputArray(start) > 0)
                    result += ((start, end))

                flag1 = false
                start = start + 1
            }
        }

        getCredible(result.toList, prefixInput)
    }

    def evaluateResults(g: Array[Int], p: Array[Int]): (Int, Int, Int, Int) =
    {
        var (tn, fp, fn, tp) = (0, 0, 0, 0)

        if (g.length == p.length)
        {
            //println("OKIE DOKIE1")
            val n = g.length

            if (n > 0)
            {
                //println("OKIE DOKIE2")
                for (i <- 0 until n)
                {
                    if (g(i) == 0 && p(i) == 0) tn += 1
                    if (g(i) == 0 && p(i) == 1) fp += 1
                    if (g(i) == 1 && p(i) == 0) fn += 1
                    if (g(i) == 1 && p(i) == 1) tp += 1
                    //println(s"${p(i)}")
                }
            }
            else
            {
                println(s"WARNING! g.length = p.length = ${n}!!!")
            }
        }
        else
        {
            println(s"WARNING! g.length = ${g.length}, but p.length = ${p.length}!!!")
        }

        (tn, fp, fn, tp)
    }

    val toyDir = new File("/home/cgvlas/Demokritos/TPLP-toy")
    val vaggelisHomePath = "/home/cgvlas/Demokritos/PIEC-paper/code-data-Vagelis/CAVIAR_experiments"

    val probEC_result_file_paths = Array[String](
        s"$vaggelisHomePath/enhanced_noisy_data/0.0/outputfile_noise_free",
        s"$vaggelisHomePath/enhanced_noisy_data/0.5/enh_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/0.5/enh_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/0.5/enh_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/0.5/enh_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/0.5/enh_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/0.5/enh_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/0.5/enh_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/0.5/enh_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/0.5/enh_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/0.5/enh_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/1.0/enh_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/1.0/enh_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/1.0/enh_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/1.0/enh_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/1.0/enh_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/1.0/enh_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/1.0/enh_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/1.0/enh_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/1.0/enh_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/1.0/enh_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/1.5/enh_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/1.5/enh_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/1.5/enh_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/1.5/enh_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/1.5/enh_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/1.5/enh_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/1.5/enh_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/1.5/enh_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/1.5/enh_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/1.5/enh_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/2.0/enh_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/2.0/enh_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/2.0/enh_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/2.0/enh_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/2.0/enh_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/2.0/enh_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/2.0/enh_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/2.0/enh_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/2.0/enh_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/2.0/enh_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/2.5/enh_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/2.5/enh_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/2.5/enh_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/2.5/enh_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/2.5/enh_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/2.5/enh_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/2.5/enh_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/2.5/enh_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/2.5/enh_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/2.5/enh_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/3.0/enh_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/3.0/enh_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/3.0/enh_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/3.0/enh_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/3.0/enh_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/3.0/enh_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/3.0/enh_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/3.0/enh_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/3.0/enh_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/3.0/enh_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/3.5/enh_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/3.5/enh_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/3.5/enh_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/3.5/enh_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/3.5/enh_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/3.5/enh_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/3.5/enh_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/3.5/enh_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/3.5/enh_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/3.5/enh_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/4.0/enh_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/4.0/enh_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/4.0/enh_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/4.0/enh_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/4.0/enh_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/4.0/enh_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/4.0/enh_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/4.0/enh_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/4.0/enh_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/4.0/enh_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/4.5/enh_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/4.5/enh_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/4.5/enh_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/4.5/enh_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/4.5/enh_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/4.5/enh_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/4.5/enh_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/4.5/enh_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/4.5/enh_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/4.5/enh_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/5.0/enh_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/5.0/enh_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/5.0/enh_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/5.0/enh_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/5.0/enh_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/5.0/enh_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/5.0/enh_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/5.0/enh_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/5.0/enh_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/5.0/enh_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/5.5/enh_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/5.5/enh_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/5.5/enh_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/5.5/enh_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/5.5/enh_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/5.5/enh_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/5.5/enh_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/5.5/enh_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/5.5/enh_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/5.5/enh_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/6.0/enh_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/6.0/enh_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/6.0/enh_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/6.0/enh_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/6.0/enh_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/6.0/enh_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/6.0/enh_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/6.0/enh_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/6.0/enh_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/6.0/enh_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/6.5/enh_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/6.5/enh_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/6.5/enh_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/6.5/enh_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/6.5/enh_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/6.5/enh_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/6.5/enh_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/6.5/enh_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/6.5/enh_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/6.5/enh_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/7.0/enh_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/7.0/enh_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/7.0/enh_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/7.0/enh_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/7.0/enh_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/7.0/enh_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/7.0/enh_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/7.0/enh_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/7.0/enh_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/7.0/enh_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/7.5/enh_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/7.5/enh_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/7.5/enh_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/7.5/enh_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/7.5/enh_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/7.5/enh_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/7.5/enh_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/7.5/enh_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/7.5/enh_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/7.5/enh_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/8.0/enh_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/8.0/enh_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/8.0/enh_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/8.0/enh_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/8.0/enh_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/8.0/enh_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/8.0/enh_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/8.0/enh_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/enhanced_noisy_data/8.0/enh_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/enhanced_noisy_data/8.0/enh_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/0.0/outputfile_noise_free",
        s"$vaggelisHomePath/original_noisy_data/0.5/orig_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/0.5/orig_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/0.5/orig_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/0.5/orig_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/0.5/orig_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/0.5/orig_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/0.5/orig_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/0.5/orig_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/0.5/orig_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/0.5/orig_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/1.0/orig_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/1.0/orig_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/1.0/orig_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/1.0/orig_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/1.0/orig_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/1.0/orig_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/1.0/orig_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/1.0/orig_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/1.0/orig_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/1.0/orig_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/1.5/orig_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/1.5/orig_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/1.5/orig_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/1.5/orig_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/1.5/orig_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/1.5/orig_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/1.5/orig_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/1.5/orig_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/1.5/orig_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/1.5/orig_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/2.0/orig_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/2.0/orig_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/2.0/orig_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/2.0/orig_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/2.0/orig_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/2.0/orig_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/2.0/orig_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/2.0/orig_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/2.0/orig_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/2.0/orig_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/2.5/orig_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/2.5/orig_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/2.5/orig_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/2.5/orig_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/2.5/orig_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/2.5/orig_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/2.5/orig_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/2.5/orig_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/2.5/orig_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/2.5/orig_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/3.0/orig_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/3.0/orig_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/3.0/orig_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/3.0/orig_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/3.0/orig_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/3.0/orig_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/3.0/orig_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/3.0/orig_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/3.0/orig_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/3.0/orig_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/3.5/orig_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/3.5/orig_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/3.5/orig_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/3.5/orig_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/3.5/orig_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/3.5/orig_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/3.5/orig_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/3.5/orig_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/3.5/orig_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/3.5/orig_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/4.0/orig_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/4.0/orig_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/4.0/orig_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/4.0/orig_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/4.0/orig_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/4.0/orig_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/4.0/orig_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/4.0/orig_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/4.0/orig_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/4.0/orig_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/4.5/orig_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/4.5/orig_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/4.5/orig_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/4.5/orig_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/4.5/orig_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/4.5/orig_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/4.5/orig_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/4.5/orig_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/4.5/orig_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/4.5/orig_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/5.0/orig_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/5.0/orig_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/5.0/orig_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/5.0/orig_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/5.0/orig_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/5.0/orig_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/5.0/orig_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/5.0/orig_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/5.0/orig_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/5.0/orig_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/5.5/orig_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/5.5/orig_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/5.5/orig_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/5.5/orig_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/5.5/orig_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/5.5/orig_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/5.5/orig_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/5.5/orig_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/5.5/orig_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/5.5/orig_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/6.0/orig_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/6.0/orig_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/6.0/orig_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/6.0/orig_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/6.0/orig_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/6.0/orig_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/6.0/orig_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/6.0/orig_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/6.0/orig_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/6.0/orig_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/6.5/orig_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/6.5/orig_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/6.5/orig_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/6.5/orig_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/6.5/orig_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/6.5/orig_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/6.5/orig_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/6.5/orig_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/6.5/orig_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/6.5/orig_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/7.0/orig_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/7.0/orig_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/7.0/orig_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/7.0/orig_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/7.0/orig_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/7.0/orig_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/7.0/orig_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/7.0/orig_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/7.0/orig_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/7.0/orig_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/7.5/orig_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/7.5/orig_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/7.5/orig_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/7.5/orig_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/7.5/orig_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/7.5/orig_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/7.5/orig_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/7.5/orig_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/7.5/orig_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/7.5/orig_all_run_5/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/8.0/orig_all_run_1/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/8.0/orig_all_run_1/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/8.0/orig_all_run_2/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/8.0/orig_all_run_2/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/8.0/orig_all_run_3/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/8.0/orig_all_run_3/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/8.0/orig_all_run_4/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/8.0/orig_all_run_4/outputfile_intermediate",
        s"$vaggelisHomePath/original_noisy_data/8.0/orig_all_run_5/outputfile_smooth",
        s"$vaggelisHomePath/original_noisy_data/8.0/orig_all_run_5/outputfile_intermediate"
    )

    var map_of_f = new mutable.HashMap[String, Array[Double]]()
    var ground_truth_of_f = new mutable.HashMap[String, Array[Int]]()

    ground_truth_of_f += (("meeting(id0,id1)", formatGround(List[(Int, Int)]((11154,11218),(19634,19655),(20797,20856)))))
    ground_truth_of_f += (("meeting(id1,id2)", formatGround(List[(Int, Int)]((682,1536),(12456,12578),(17820,17844),(18272,18316),(18680,18781)))))
    ground_truth_of_f += (("meeting(id1,id3)", formatGround(List[(Int, Int)]((18950,18975)))))
    ground_truth_of_f += (("meeting(id2,id6)", formatGround(List[(Int, Int)]((23453,23472)))))
    //ground_truth_of_f += (("meeting(id3,id4)", formatGround(List[(Int, Int)]((18188,18197)))))
    ground_truth_of_f += (("meeting(id4,id5)", formatGround(List[(Int, Int)]((143,610)))))
    ground_truth_of_f += (("meeting(id1,id0)", formatGround(List[(Int, Int)]((11154,11218),(19634,19655),(20797,20856)))))
    ground_truth_of_f += (("meeting(id2,id1)", formatGround(List[(Int, Int)]((682,1536),(12456,12578),(17820,17844),(18272,18316),(20528,20629)))))
    ground_truth_of_f += (("meeting(id3,id1)", formatGround(List[(Int, Int)]((18950,18975)))))
    //ground_truth_of_f += (("meeting(id4,id3)", formatGround(List[(Int, Int)]((18188,18197)))))
    ground_truth_of_f += (("meeting(id5,id4)", formatGround(List[(Int, Int)]((143,610)))))
    ground_truth_of_f += (("meeting(id6,id2)", formatGround(List[(Int, Int)]((23453,23472)))))

    //ground_truth_of_f += (("leaving_object(id2,id4)", formatGround(List[(Int, Int)]((18449,18460),(18936,18961)))))
    //ground_truth_of_f += (("leaving_object(id3,id4)", formatGround(List[(Int, Int)]((14618,14669)))))
    //ground_truth_of_f += (("leaving_object(id4,id3)", formatGround(List[(Int, Int)]((17655,17686)))))
    //ground_truth_of_f += (("leaving_object(id4,id6)", formatGround(List[(Int, Int)]((15583,15606)))))
    //ground_truth_of_f += (("leaving_object(id5,id6)", formatGround(List[(Int, Int)]((15967,16018)))))

    ground_truth_of_f += (("fighting(id1,id2)", formatGround(List[(Int, Int)]((25036,25105)))))
    ground_truth_of_f += (("fighting(id2,id1)", formatGround(List[(Int, Int)]((25036,25105)))))
    ground_truth_of_f += (("fighting(id2,id6)", formatGround(List[(Int, Int)]((24160,24298)))))
    ground_truth_of_f += (("fighting(id6,id2)", formatGround(List[(Int, Int)]((24160,24298)))))
    ground_truth_of_f += (("fighting(id3,id4)", formatGround(List[(Int, Int)]((21879,22007)))))
    ground_truth_of_f += (("fighting(id4,id3)", formatGround(List[(Int, Int)]((21879,22007)))))
    ground_truth_of_f += (("fighting(id4,id5)", formatGround(List[(Int, Int)]((22387,22500),(23197,23313)))))
    ground_truth_of_f += (("fighting(id5,id4)", formatGround(List[(Int, Int)]((22387,22500),(23197,23313)))))
    ground_truth_of_f += (("fighting(id6,id7)", formatGround(List[(Int, Int)]((21371,21430)))))
    ground_truth_of_f += (("fighting(id7,id6)", formatGround(List[(Int, Int)]((21371,21430)))))

    ground_truth_of_f += (("moving(id0,id1)", formatGround(List[(Int, Int)]((11506,11592),(19656,19800),(20317,20607),(20623,20796)))))
    ground_truth_of_f += (("moving(id0,id2)", formatGround(List[(Int, Int)]((20346,20629)))))
    ground_truth_of_f += (("moving(id0,id3)", formatGround(List[(Int, Int)]((20385,20627)))))
    ground_truth_of_f += (("moving(id1,id2)", formatGround(List[(Int, Int)]((611,681),(12181,12455),(14133,14189),(17845,17984),(18317,18679),(20346,20607)))))
    ground_truth_of_f += (("moving(id1,id3)", formatGround(List[(Int, Int)]((18976,19061),(20385,20607)))))
    ground_truth_of_f += (("moving(id2,id3)", formatGround(List[(Int, Int)]((20385,20627)))))
    ground_truth_of_f += (("moving(id3,id4)", formatGround(List[(Int, Int)]((18189,18198)))))
    ground_truth_of_f += (("moving(id4,id5)", formatGround(List[(Int, Int)]((63,142)))))
    ground_truth_of_f += (("moving(id5,id6)", formatGround(List[(Int, Int)]((18117,18198)))))
    ground_truth_of_f += (("moving(id1,id0)", formatGround(List[(Int, Int)]((11506,11592),(19656,19800),(20317,20607),(20623,20796)))))
    ground_truth_of_f += (("moving(id2,id0)", formatGround(List[(Int, Int)]((20346,20629)))))
    ground_truth_of_f += (("moving(id3,id0)", formatGround(List[(Int, Int)]((20385,20627)))))
    ground_truth_of_f += (("moving(id2,id1)", formatGround(List[(Int, Int)]((611,681),(12181,12455),(14133,14189),(17845,17984),(18317,18679),(20346,20607)))))
    ground_truth_of_f += (("moving(id3,id1)", formatGround(List[(Int, Int)]((18976,19061),(20385,20607)))))
    ground_truth_of_f += (("moving(id3,id2)", formatGround(List[(Int, Int)]((20385,20627)))))
    ground_truth_of_f += (("moving(id4,id3)", formatGround(List[(Int, Int)]((18189,18198)))))
    ground_truth_of_f += (("moving(id5,id4)", formatGround(List[(Int, Int)]((63,142)))))
    ground_truth_of_f += (("moving(id6,id5)", formatGround(List[(Int, Int)]((18117,18198)))))

    for (piecInputFilePath <- probEC_result_file_paths)
    {
        val piecInputFile = new File(piecInputFilePath)

        for (th <- 0.5 to 0.91 by 0.2)
        {
            val thround = BigDecimal(th).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            println(s"Working on ${piecInputFilePath} Prob-EC result file, threshold: ${thround}...")

            //var fpl = 0
            var fpf = 0
            var fpmt = 0
            var fpmv = 0

            //var noiseType = piecInputFile.getName.split("_")(0)
            val dataSetType = piecInputFilePath.split("/")(7).split("_")(0)
            val dst = if (dataSetType == "enhanced") "enh" else "orig"
            val gammaValue = piecInputFilePath.split("/")(8)
            val runNo = if (gammaValue == "0.0") "0" else piecInputFilePath.split("/")(9).split("_")(3)
            val noiseAmount = if (gammaValue == "0.0") "none" else piecInputFilePath.split("/")(10).split("_")(1)

            map_of_f.clear()

            if (dst == "enh")
            {
                map_of_f += (("fighting(id1,id2)", Array.fill(25170)(0.0)))
                map_of_f += (("fighting(id2,id6)", Array.fill(25170)(0.0)))
                map_of_f += (("fighting(id3,id4)", Array.fill(25170)(0.0)))
                map_of_f += (("fighting(id4,id5)", Array.fill(25170)(0.0)))
                map_of_f += (("fighting(id5,id6)", Array.fill(25170)(0.0)))
                map_of_f += (("fighting(id6,id7)", Array.fill(25170)(0.0)))
                map_of_f += (("fighting(id2,id1)", Array.fill(25170)(0.0)))
                map_of_f += (("fighting(id6,id2)", Array.fill(25170)(0.0)))
                map_of_f += (("fighting(id4,id3)", Array.fill(25170)(0.0)))
                map_of_f += (("fighting(id5,id4)", Array.fill(25170)(0.0)))
                map_of_f += (("fighting(id6,id5)", Array.fill(25170)(0.0)))
                map_of_f += (("fighting(id7,id6)", Array.fill(25170)(0.0)))
            }

            if (dst == "orig")
            {
                map_of_f += (("meeting(id0,id1)", Array.fill(25170)(0.0)))
                map_of_f += (("meeting(id1,id2)", Array.fill(25170)(0.0)))
                map_of_f += (("meeting(id1,id3)", Array.fill(25170)(0.0)))
                //map_of_f += (("meeting(id3,id4)", Array.fill(25170)(0.0)))
                map_of_f += (("meeting(id4,id5)", Array.fill(25170)(0.0)))
                map_of_f += (("meeting(id5,id6)", Array.fill(25170)(0.0)))
                map_of_f += (("meeting(id2,id6)", Array.fill(25170)(0.0)))
                map_of_f += (("meeting(id1,id0)", Array.fill(25170)(0.0)))
                map_of_f += (("meeting(id2,id1)", Array.fill(25170)(0.0)))
                map_of_f += (("meeting(id3,id1)", Array.fill(25170)(0.0)))
                //map_of_f += (("meeting(id4,id3)", Array.fill(25170)(0.0)))
                map_of_f += (("meeting(id5,id4)", Array.fill(25170)(0.0)))
                map_of_f += (("meeting(id6,id5)", Array.fill(25170)(0.0)))
                map_of_f += (("meeting(id6,id2)", Array.fill(25170)(0.0)))

                //map_of_f += (("leaving_object(id2,id4)", Array.fill(25170)(0.0)))
                //map_of_f += (("leaving_object(id3,id4)", Array.fill(25170)(0.0)))
                //map_of_f += (("leaving_object(id4,id3)", Array.fill(25170)(0.0)))
                //map_of_f += (("leaving_object(id4,id6)", Array.fill(25170)(0.0)))
                //map_of_f += (("leaving_object(id5,id6)", Array.fill(25170)(0.0)))

                map_of_f += (("moving(id0,id1)", Array.fill(25170)(0.0)))
                map_of_f += (("moving(id0,id2)", Array.fill(25170)(0.0)))
                map_of_f += (("moving(id0,id3)", Array.fill(25170)(0.0)))
                map_of_f += (("moving(id1,id2)", Array.fill(25170)(0.0)))
                map_of_f += (("moving(id1,id3)", Array.fill(25170)(0.0)))
                map_of_f += (("moving(id2,id3)", Array.fill(25170)(0.0)))
                map_of_f += (("moving(id3,id4)", Array.fill(25170)(0.0)))
                map_of_f += (("moving(id4,id5)", Array.fill(25170)(0.0)))
                map_of_f += (("moving(id5,id6)", Array.fill(25170)(0.0)))
                map_of_f += (("moving(id1,id0)", Array.fill(25170)(0.0)))
                map_of_f += (("moving(id2,id0)", Array.fill(25170)(0.0)))
                map_of_f += (("moving(id3,id0)", Array.fill(25170)(0.0)))
                map_of_f += (("moving(id2,id1)", Array.fill(25170)(0.0)))
                map_of_f += (("moving(id3,id1)", Array.fill(25170)(0.0)))
                map_of_f += (("moving(id3,id2)", Array.fill(25170)(0.0)))
                map_of_f += (("moving(id4,id3)", Array.fill(25170)(0.0)))
                map_of_f += (("moving(id5,id4)", Array.fill(25170)(0.0)))
                map_of_f += (("moving(id6,id5)", Array.fill(25170)(0.0)))
            }

            for (line <- Source.fromFile(piecInputFile).getLines())
            {
                val lineReady = if (line.endsWith("o") || line.endsWith("ok")) line.substring(0, line.lastIndexOf("o")) else line

                val value = lineReady.split("::")(0)
                val rest = lineReady.split("::")(1)

                val hle = rest.substring(rest.indexOf("(") + 1, rest.indexOf("="))
                val pred = hle.substring(0, hle.indexOf("("))
                val prob = value.toDouble
                val time = rest.substring(rest.indexOf("%") + 1).toInt

                if (!map_of_f.contains(hle))
                {
                    if (pred == "meeting" && prob >= thround)
                    {
                        fpmt = fpmt + 1
                    }
                    if (pred == "moving" && prob >= thround)
                    {
                        fpmv = fpmv + 1
                    }
                    if (pred == "fighting" && prob >= thround)
                    {
                        fpf = fpf + 1
                    }
                }
                else
                {
                    map_of_f(hle)(time) = prob
                }
            }

            //println("Now, onto the PIEC stuff...")

            if (dst == "orig")
            {
                var (wmt, xmt, ymt, zmt) = (0, fpmt, 0, 0)
                var (dmt, amt, bmt, cmt) = (0, fpmt, 0, 0)

                val results_mt_01_r = evaluateResults(ground_truth_of_f("meeting(id0,id1)"), probec_intervals(map_of_f("meeting(id0,id1)"), thround))
                val results_mt_01_p = evaluateResults(ground_truth_of_f("meeting(id0,id1)"), piec(map_of_f("meeting(id0,id1)"), thround))

                wmt = wmt + results_mt_01_r._1
                xmt = xmt + results_mt_01_r._2
                ymt = ymt + results_mt_01_r._3
                zmt = zmt + results_mt_01_r._4

                dmt = dmt + results_mt_01_p._1
                amt = amt + results_mt_01_p._2
                bmt = bmt + results_mt_01_p._3
                cmt = cmt + results_mt_01_p._4

                val results_mt_12_r = evaluateResults(ground_truth_of_f("meeting(id1,id2)"), probec_intervals(map_of_f("meeting(id1,id2)"), thround))
                val results_mt_12_p = evaluateResults(ground_truth_of_f("meeting(id1,id2)"), piec(map_of_f("meeting(id1,id2)"), thround))

                wmt = wmt + results_mt_12_r._1
                xmt = xmt + results_mt_12_r._2
                ymt = ymt + results_mt_12_r._3
                zmt = zmt + results_mt_12_r._4

                dmt = dmt + results_mt_12_p._1
                amt = amt + results_mt_12_p._2
                bmt = bmt + results_mt_12_p._3
                cmt = cmt + results_mt_12_p._4

                val results_mt_13_r = evaluateResults(ground_truth_of_f("meeting(id1,id3)"), probec_intervals(map_of_f("meeting(id1,id3)"), thround))
                val results_mt_13_p = evaluateResults(ground_truth_of_f("meeting(id1,id3)"), piec(map_of_f("meeting(id1,id3)"), thround))

                wmt = wmt + results_mt_13_r._1
                xmt = xmt + results_mt_13_r._2
                ymt = ymt + results_mt_13_r._3
                zmt = zmt + results_mt_13_r._4

                dmt = dmt + results_mt_13_p._1
                amt = amt + results_mt_13_p._2
                bmt = bmt + results_mt_13_p._3
                cmt = cmt + results_mt_13_p._4

                val results_mt_45_r = evaluateResults(ground_truth_of_f("meeting(id4,id5)"), probec_intervals(map_of_f("meeting(id4,id5)"), thround))
                val results_mt_45_p = evaluateResults(ground_truth_of_f("meeting(id4,id5)"), piec(map_of_f("meeting(id4,id5)"), thround))

                wmt = wmt + results_mt_45_r._1
                xmt = xmt + results_mt_45_r._2
                ymt = ymt + results_mt_45_r._3
                zmt = zmt + results_mt_45_r._4

                dmt = dmt + results_mt_45_p._1
                amt = amt + results_mt_45_p._2
                bmt = bmt + results_mt_45_p._3
                cmt = cmt + results_mt_45_p._4

                val results_mt_26_r = evaluateResults(ground_truth_of_f("meeting(id2,id6)"), probec_intervals(map_of_f("meeting(id2,id6)"), thround))
                val results_mt_26_p = evaluateResults(ground_truth_of_f("meeting(id2,id6)"), piec(map_of_f("meeting(id2,id6)"), thround))

                wmt = wmt + results_mt_26_r._1
                xmt = xmt + results_mt_26_r._2
                ymt = ymt + results_mt_26_r._3
                zmt = zmt + results_mt_26_r._4

                dmt = dmt + results_mt_26_p._1
                amt = amt + results_mt_26_p._2
                bmt = bmt + results_mt_26_p._3
                cmt = cmt + results_mt_26_p._4

                val results_mt_10_r = evaluateResults(ground_truth_of_f("meeting(id1,id0)"), probec_intervals(map_of_f("meeting(id1,id0)"), thround))
                val results_mt_10_p = evaluateResults(ground_truth_of_f("meeting(id1,id0)"), piec(map_of_f("meeting(id1,id0)"), thround))

                wmt = wmt + results_mt_10_r._1
                xmt = xmt + results_mt_10_r._2
                ymt = ymt + results_mt_10_r._3
                zmt = zmt + results_mt_10_r._4

                dmt = dmt + results_mt_10_p._1
                amt = amt + results_mt_10_p._2
                bmt = bmt + results_mt_10_p._3
                cmt = cmt + results_mt_10_p._4

                val results_mt_21_r = evaluateResults(ground_truth_of_f("meeting(id2,id1)"), probec_intervals(map_of_f("meeting(id2,id1)"), thround))
                val results_mt_21_p = evaluateResults(ground_truth_of_f("meeting(id2,id1)"), piec(map_of_f("meeting(id2,id1)"), thround))

                wmt = wmt + results_mt_21_r._1
                xmt = xmt + results_mt_21_r._2
                ymt = ymt + results_mt_21_r._3
                zmt = zmt + results_mt_21_r._4

                dmt = dmt + results_mt_21_p._1
                amt = amt + results_mt_21_p._2
                bmt = bmt + results_mt_21_p._3
                cmt = cmt + results_mt_21_p._4

                val results_mt_31_r = evaluateResults(ground_truth_of_f("meeting(id3,id1)"), probec_intervals(map_of_f("meeting(id3,id1)"), thround))
                val results_mt_31_p = evaluateResults(ground_truth_of_f("meeting(id3,id1)"), piec(map_of_f("meeting(id3,id1)"), thround))

                wmt = wmt + results_mt_31_r._1
                xmt = xmt + results_mt_31_r._2
                ymt = ymt + results_mt_31_r._3
                zmt = zmt + results_mt_31_r._4

                dmt = dmt + results_mt_31_p._1
                amt = amt + results_mt_31_p._2
                bmt = bmt + results_mt_31_p._3
                cmt = cmt + results_mt_31_p._4

                val results_mt_54_r = evaluateResults(ground_truth_of_f("meeting(id5,id4)"), probec_intervals(map_of_f("meeting(id5,id4)"), thround))
                val results_mt_54_p = evaluateResults(ground_truth_of_f("meeting(id5,id4)"), piec(map_of_f("meeting(id5,id4)"), thround))

                wmt = wmt + results_mt_54_r._1
                xmt = xmt + results_mt_54_r._2
                ymt = ymt + results_mt_54_r._3
                zmt = zmt + results_mt_54_r._4

                dmt = dmt + results_mt_54_p._1
                amt = amt + results_mt_54_p._2
                bmt = bmt + results_mt_54_p._3
                cmt = cmt + results_mt_54_p._4

                val results_mt_62_r = evaluateResults(ground_truth_of_f("meeting(id6,id2)"), probec_intervals(map_of_f("meeting(id6,id2)"), thround))
                val results_mt_62_p = evaluateResults(ground_truth_of_f("meeting(id6,id2)"), piec(map_of_f("meeting(id6,id2)"), thround))

                wmt = wmt + results_mt_62_r._1
                xmt = xmt + results_mt_62_r._2
                ymt = ymt + results_mt_62_r._3
                zmt = zmt + results_mt_62_r._4

                dmt = dmt + results_mt_62_p._1
                amt = amt + results_mt_62_p._2
                bmt = bmt + results_mt_62_p._3
                cmt = cmt + results_mt_62_p._4

                val prec_mt_probec = if ((zmt + xmt) == 0) 0
                                     else BigDecimal(zmt / (zmt + xmt).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val rec_mt_probec = if ((zmt + ymt) == 0) 0
                                    else BigDecimal(zmt / (zmt + ymt).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val f1_mt_probec = if ((prec_mt_probec + rec_mt_probec) == 0.0) 0
                                   else BigDecimal((2 * prec_mt_probec * rec_mt_probec) / (prec_mt_probec + rec_mt_probec)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                println(s"precision_meeting_prob-ec : ${prec_mt_probec}")
                println(s"recall_meeting_prob-ec : ${rec_mt_probec}")
                println(s"f1-score_meeting_prob-ec : ${f1_mt_probec}")

                val prec_mt_piec = if ((cmt + amt) == 0) 0
                                   else BigDecimal(cmt / (cmt + amt).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val rec_mt_piec = if ((cmt + bmt) == 0) 0
                                  else BigDecimal(cmt / (cmt + bmt).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val f1_mt_piec = if ((prec_mt_piec + rec_mt_piec) == 0.0) 0
                                 else BigDecimal((2 * prec_mt_piec * rec_mt_piec) / (prec_mt_piec + rec_mt_piec)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                println(s"precision_meeting_piec : ${prec_mt_piec}")
                println(s"recall_meeting_piec : ${rec_mt_piec}")
                println(s"f1-score_meeting_piec : ${f1_mt_piec}\n\n")

                val prout1_mt = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PROBEC/$noiseAmount/meeting_${noiseAmount}-${dst}-t${thround}.meeting-precision.data")
                val recout1_mt = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PROBEC/$noiseAmount/meeting_${noiseAmount}-${dst}-t${thround}.meeting-recall.data")
                val f1out1_mt = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PROBEC/$noiseAmount/meeting_${noiseAmount}-${dst}-t${thround}.meeting-fmeasure.data")

                if (!prout1_mt.getParentFile.exists()) prout1_mt.getParentFile.mkdirs()
                if (!prout1_mt.exists()) prout1_mt.createNewFile()
                if (!recout1_mt.exists()) recout1_mt.createNewFile()
                if (!f1out1_mt.exists()) f1out1_mt.createNewFile()

                val prout2_mt = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PIEC/$noiseAmount/meeting_${noiseAmount}-${dst}-t${thround}.meeting-precision.data")
                val recout2_mt = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PIEC/$noiseAmount/meeting_${noiseAmount}-${dst}-t${thround}.meeting-recall.data")
                val f1out2_mt = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PIEC/$noiseAmount/meeting_${noiseAmount}-${dst}-t${thround}.meeting-fmeasure.data")

                if (!prout2_mt.getParentFile.exists()) prout2_mt.getParentFile.mkdirs()
                if (!prout2_mt.exists()) prout2_mt.createNewFile()
                if (!recout2_mt.exists()) recout2_mt.createNewFile()
                if (!f1out2_mt.exists()) f1out2_mt.createNewFile()

                val fw11_mt = new FileWriter(prout1_mt, true)
                val fw12_mt = new FileWriter(recout1_mt, true)
                val fw13_mt = new FileWriter(f1out1_mt, true)

                val fw21_mt = new FileWriter(prout2_mt, true)
                val fw22_mt = new FileWriter(recout2_mt, true)
                val fw23_mt = new FileWriter(f1out2_mt, true)

                fw11_mt.write(s"${gammaValue}:${runNo}:${prec_mt_probec}\n")
                fw12_mt.write(s"${gammaValue}:${runNo}:${rec_mt_probec}\n")
                fw13_mt.write(s"${gammaValue}:${runNo}:${f1_mt_probec}\n")

                fw21_mt.write(s"${gammaValue}:${runNo}:${prec_mt_piec}\n")
                fw22_mt.write(s"${gammaValue}:${runNo}:${rec_mt_piec}\n")
                fw23_mt.write(s"${gammaValue}:${runNo}:${f1_mt_piec}\n")

                fw11_mt.close()
                fw12_mt.close()
                fw13_mt.close()

                fw21_mt.close()
                fw22_mt.close()
                fw23_mt.close()

                /****************************************************2*************************************************/

                var (wmv, xmv, ymv, zmv) = (0, fpmv, 0, 0)
                var (dmv, amv, bmv, cmv) = (0, fpmv, 0, 0)

                val results_mv_01_r = evaluateResults(ground_truth_of_f("moving(id0,id1)"), probec_intervals(map_of_f("moving(id0,id1)"), thround))
                val results_mv_01_p = evaluateResults(ground_truth_of_f("moving(id0,id1)"), piec(map_of_f("moving(id0,id1)"), thround))

                wmv = wmv + results_mv_01_r._1
                xmv = xmv + results_mv_01_r._2
                ymv = ymv + results_mv_01_r._3
                zmv = zmv + results_mv_01_r._4

                dmv = dmv + results_mv_01_p._1
                amv = amv + results_mv_01_p._2
                bmv = bmv + results_mv_01_p._3
                cmv = cmv + results_mv_01_p._4

                val results_mv_02_r = evaluateResults(ground_truth_of_f("moving(id0,id2)"), probec_intervals(map_of_f("moving(id0,id2)"), thround))
                val results_mv_02_p = evaluateResults(ground_truth_of_f("moving(id0,id2)"), piec(map_of_f("moving(id0,id2)"), thround))

                wmv = wmv + results_mv_02_r._1
                xmv = xmv + results_mv_02_r._2
                ymv = ymv + results_mv_02_r._3
                zmv = zmv + results_mv_02_r._4

                dmv = dmv + results_mv_02_p._1
                amv = amv + results_mv_02_p._2
                bmv = bmv + results_mv_02_p._3
                cmv = cmv + results_mv_02_p._4

                val results_mv_03_r = evaluateResults(ground_truth_of_f("moving(id0,id3)"), probec_intervals(map_of_f("moving(id0,id3)"), thround))
                val results_mv_03_p = evaluateResults(ground_truth_of_f("moving(id0,id3)"), piec(map_of_f("moving(id0,id3)"), thround))

                wmv = wmv + results_mv_03_r._1
                xmv = xmv + results_mv_03_r._2
                ymv = ymv + results_mv_03_r._3
                zmv = zmv + results_mv_03_r._4

                dmv = dmv + results_mv_03_p._1
                amv = amv + results_mv_03_p._2
                bmv = bmv + results_mv_03_p._3
                cmv = cmv + results_mv_03_p._4

                val results_mv_12_r = evaluateResults(ground_truth_of_f("moving(id1,id2)"), probec_intervals(map_of_f("moving(id1,id2)"), thround))
                val results_mv_12_p = evaluateResults(ground_truth_of_f("moving(id1,id2)"), piec(map_of_f("moving(id1,id2)"), thround))

                wmv = wmv + results_mv_12_r._1
                xmv = xmv + results_mv_12_r._2
                ymv = ymv + results_mv_12_r._3
                zmv = zmv + results_mv_12_r._4

                dmv = dmv + results_mv_12_p._1
                amv = amv + results_mv_12_p._2
                bmv = bmv + results_mv_12_p._3
                cmv = cmv + results_mv_12_p._4

                val results_mv_13_r = evaluateResults(ground_truth_of_f("moving(id1,id3)"), probec_intervals(map_of_f("moving(id1,id3)"), thround))
                val results_mv_13_p = evaluateResults(ground_truth_of_f("moving(id1,id3)"), piec(map_of_f("moving(id1,id3)"), thround))

                wmv = wmv + results_mv_13_r._1
                xmv = xmv + results_mv_13_r._2
                ymv = ymv + results_mv_13_r._3
                zmv = zmv + results_mv_13_r._4

                dmv = dmv + results_mv_13_p._1
                amv = amv + results_mv_13_p._2
                bmv = bmv + results_mv_13_p._3
                cmv = cmv + results_mv_13_p._4

                val results_mv_23_r = evaluateResults(ground_truth_of_f("moving(id2,id3)"), probec_intervals(map_of_f("moving(id2,id3)"), thround))
                val results_mv_23_p = evaluateResults(ground_truth_of_f("moving(id2,id3)"), piec(map_of_f("moving(id2,id3)"), thround))

                wmv = wmv + results_mv_23_r._1
                xmv = xmv + results_mv_23_r._2
                ymv = ymv + results_mv_23_r._3
                zmv = zmv + results_mv_23_r._4

                dmv = dmv + results_mv_23_p._1
                amv = amv + results_mv_23_p._2
                bmv = bmv + results_mv_23_p._3
                cmv = cmv + results_mv_23_p._4

                val results_mv_34_r = evaluateResults(ground_truth_of_f("moving(id3,id4)"), probec_intervals(map_of_f("moving(id3,id4)"), thround))
                val results_mv_34_p = evaluateResults(ground_truth_of_f("moving(id3,id4)"), piec(map_of_f("moving(id3,id4)"), thround))

                wmv = wmv + results_mv_34_r._1
                xmv = xmv + results_mv_34_r._2
                ymv = ymv + results_mv_34_r._3
                zmv = zmv + results_mv_34_r._4

                dmv = dmv + results_mv_34_p._1
                amv = amv + results_mv_34_p._2
                bmv = bmv + results_mv_34_p._3
                cmv = cmv + results_mv_34_p._4

                val results_mv_45_r = evaluateResults(ground_truth_of_f("moving(id4,id5)"), probec_intervals(map_of_f("moving(id4,id5)"), thround))
                val results_mv_45_p = evaluateResults(ground_truth_of_f("moving(id4,id5)"), piec(map_of_f("moving(id4,id5)"), thround))

                wmv = wmv + results_mv_45_r._1
                xmv = xmv + results_mv_45_r._2
                ymv = ymv + results_mv_45_r._3
                zmv = zmv + results_mv_45_r._4

                dmv = dmv + results_mv_45_p._1
                amv = amv + results_mv_45_p._2
                bmv = bmv + results_mv_45_p._3
                cmv = cmv + results_mv_45_p._4

                val results_mv_56_r = evaluateResults(ground_truth_of_f("moving(id5,id6)"), probec_intervals(map_of_f("moving(id5,id6)"), thround))
                val results_mv_56_p = evaluateResults(ground_truth_of_f("moving(id5,id6)"), piec(map_of_f("moving(id5,id6)"), thround))

                wmv = wmv + results_mv_56_r._1
                xmv = xmv + results_mv_56_r._2
                ymv = ymv + results_mv_56_r._3
                zmv = zmv + results_mv_56_r._4

                dmv = dmv + results_mv_56_p._1
                amv = amv + results_mv_56_p._2
                bmv = bmv + results_mv_56_p._3
                cmv = cmv + results_mv_56_p._4

                val results_mv_10_r = evaluateResults(ground_truth_of_f("moving(id1,id0)"), probec_intervals(map_of_f("moving(id1,id0)"), thround))
                val results_mv_10_p = evaluateResults(ground_truth_of_f("moving(id1,id0)"), piec(map_of_f("moving(id1,id0)"), thround))

                wmv = wmv + results_mv_10_r._1
                xmv = xmv + results_mv_10_r._2
                ymv = ymv + results_mv_10_r._3
                zmv = zmv + results_mv_10_r._4

                dmv = dmv + results_mv_10_p._1
                amv = amv + results_mv_10_p._2
                bmv = bmv + results_mv_10_p._3
                cmv = cmv + results_mv_10_p._4

                val results_mv_20_r = evaluateResults(ground_truth_of_f("moving(id2,id0)"), probec_intervals(map_of_f("moving(id2,id0)"), thround))
                val results_mv_20_p = evaluateResults(ground_truth_of_f("moving(id2,id0)"), piec(map_of_f("moving(id2,id0)"), thround))

                wmv = wmv + results_mv_20_r._1
                xmv = xmv + results_mv_20_r._2
                ymv = ymv + results_mv_20_r._3
                zmv = zmv + results_mv_20_r._4

                dmv = dmv + results_mv_20_p._1
                amv = amv + results_mv_20_p._2
                bmv = bmv + results_mv_20_p._3
                cmv = cmv + results_mv_20_p._4

                val results_mv_30_r = evaluateResults(ground_truth_of_f("moving(id3,id0)"), probec_intervals(map_of_f("moving(id3,id0)"), thround))
                val results_mv_30_p = evaluateResults(ground_truth_of_f("moving(id3,id0)"), piec(map_of_f("moving(id3,id0)"), thround))

                wmv = wmv + results_mv_30_r._1
                xmv = xmv + results_mv_30_r._2
                ymv = ymv + results_mv_30_r._3
                zmv = zmv + results_mv_30_r._4

                dmv = dmv + results_mv_30_p._1
                amv = amv + results_mv_30_p._2
                bmv = bmv + results_mv_30_p._3
                cmv = cmv + results_mv_30_p._4

                val results_mv_21_r = evaluateResults(ground_truth_of_f("moving(id2,id1)"), probec_intervals(map_of_f("moving(id2,id1)"), thround))
                val results_mv_21_p = evaluateResults(ground_truth_of_f("moving(id2,id1)"), piec(map_of_f("moving(id2,id1)"), thround))

                wmv = wmv + results_mv_21_r._1
                xmv = xmv + results_mv_21_r._2
                ymv = ymv + results_mv_21_r._3
                zmv = zmv + results_mv_21_r._4

                dmv = dmv + results_mv_21_p._1
                amv = amv + results_mv_21_p._2
                bmv = bmv + results_mv_21_p._3
                cmv = cmv + results_mv_21_p._4

                val results_mv_31_r = evaluateResults(ground_truth_of_f("moving(id3,id1)"), probec_intervals(map_of_f("moving(id3,id1)"), thround))
                val results_mv_31_p = evaluateResults(ground_truth_of_f("moving(id3,id1)"), piec(map_of_f("moving(id3,id1)"), thround))

                wmv = wmv + results_mv_31_r._1
                xmv = xmv + results_mv_31_r._2
                ymv = ymv + results_mv_31_r._3
                zmv = zmv + results_mv_31_r._4

                dmv = dmv + results_mv_31_p._1
                amv = amv + results_mv_31_p._2
                bmv = bmv + results_mv_31_p._3
                cmv = cmv + results_mv_31_p._4

                val results_mv_32_r = evaluateResults(ground_truth_of_f("moving(id3,id2)"), probec_intervals(map_of_f("moving(id3,id2)"), thround))
                val results_mv_32_p = evaluateResults(ground_truth_of_f("moving(id3,id2)"), piec(map_of_f("moving(id3,id2)"), thround))

                wmv = wmv + results_mv_32_r._1
                xmv = xmv + results_mv_32_r._2
                ymv = ymv + results_mv_32_r._3
                zmv = zmv + results_mv_32_r._4

                dmv = dmv + results_mv_32_p._1
                amv = amv + results_mv_32_p._2
                bmv = bmv + results_mv_32_p._3
                cmv = cmv + results_mv_32_p._4

                val results_mv_43_r = evaluateResults(ground_truth_of_f("moving(id4,id3)"), probec_intervals(map_of_f("moving(id4,id3)"), thround))
                val results_mv_43_p = evaluateResults(ground_truth_of_f("moving(id4,id3)"), piec(map_of_f("moving(id4,id3)"), thround))

                wmv = wmv + results_mv_43_r._1
                xmv = xmv + results_mv_43_r._2
                ymv = ymv + results_mv_43_r._3
                zmv = zmv + results_mv_43_r._4

                dmv = dmv + results_mv_43_p._1
                amv = amv + results_mv_43_p._2
                bmv = bmv + results_mv_43_p._3
                cmv = cmv + results_mv_43_p._4

                val results_mv_54_r = evaluateResults(ground_truth_of_f("moving(id5,id4)"), probec_intervals(map_of_f("moving(id5,id4)"), thround))
                val results_mv_54_p = evaluateResults(ground_truth_of_f("moving(id5,id4)"), piec(map_of_f("moving(id5,id4)"), thround))

                wmv = wmv + results_mv_54_r._1
                xmv = xmv + results_mv_54_r._2
                ymv = ymv + results_mv_54_r._3
                zmv = zmv + results_mv_54_r._4

                dmv = dmv + results_mv_54_p._1
                amv = amv + results_mv_54_p._2
                bmv = bmv + results_mv_54_p._3
                cmv = cmv + results_mv_54_p._4

                val results_mv_65_r = evaluateResults(ground_truth_of_f("moving(id6,id5)"), probec_intervals(map_of_f("moving(id6,id5)"), thround))
                val results_mv_65_p = evaluateResults(ground_truth_of_f("moving(id6,id5)"), piec(map_of_f("moving(id6,id5)"), thround))

                wmv = wmv + results_mv_65_r._1
                xmv = xmv + results_mv_65_r._2
                ymv = ymv + results_mv_65_r._3
                zmv = zmv + results_mv_65_r._4

                dmv = dmv + results_mv_65_p._1
                amv = amv + results_mv_65_p._2
                bmv = bmv + results_mv_65_p._3
                cmv = cmv + results_mv_65_p._4

                val prec_mv_probec = if ((zmv + xmv) == 0) 0
                                     else BigDecimal(zmv / (zmv + xmv).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val rec_mv_probec = if ((zmv + ymv) == 0) 0
                                    else BigDecimal(zmv / (zmv + ymv).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val f1_mv_probec = if ((prec_mv_probec + rec_mv_probec) == 0.0) 0
                                   else BigDecimal((2 * prec_mv_probec * rec_mv_probec) / (prec_mv_probec + rec_mv_probec)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                println(s"precision_moving_prob-ec : ${prec_mv_probec}")
                println(s"recall_moving_prob-ec : ${rec_mv_probec}")
                println(s"f1-score_moving_prob-ec : ${f1_mv_probec}")

                val prec_mv_piec = if ((cmv + amv) == 0) 0
                                   else BigDecimal(cmv / (cmv + amv).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val rec_mv_piec = if ((cmv + bmv) == 0) 0
                                  else BigDecimal(cmv / (cmv + bmv).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val f1_mv_piec = if ((prec_mv_piec + rec_mv_piec) == 0.0) 0
                                 else BigDecimal((2 * prec_mv_piec * rec_mv_piec) / (prec_mv_piec + rec_mv_piec)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                println(s"precision_moving_piec : ${prec_mv_piec}")
                println(s"recall_moving_piec : ${rec_mv_piec}")
                println(s"f1-score_moving_piec : ${f1_mv_piec}\n\n")

                val prout1_mv = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PROBEC/$noiseAmount/moving_${noiseAmount}-${dst}-t${thround}.moving-precision.data")
                val recout1_mv = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PROBEC/$noiseAmount/moving_${noiseAmount}-${dst}-t${thround}.moving-recall.data")
                val f1out1_mv = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PROBEC/$noiseAmount/moving_${noiseAmount}-${dst}-t${thround}.moving-fmeasure.data")

                if (!prout1_mv.getParentFile.exists()) prout1_mv.getParentFile.mkdirs()
                if (!prout1_mv.exists()) prout1_mv.createNewFile()
                if (!recout1_mv.exists()) recout1_mv.createNewFile()
                if (!f1out1_mv.exists()) f1out1_mv.createNewFile()

                val prout2_mv = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PIEC/$noiseAmount/moving_${noiseAmount}-${dst}-t${thround}.moving-precision.data")
                val recout2_mv = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PIEC/$noiseAmount/moving_${noiseAmount}-${dst}-t${thround}.moving-recall.data")
                val f1out2_mv = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PIEC/$noiseAmount/moving_${noiseAmount}-${dst}-t${thround}.moving-fmeasure.data")

                if (!prout2_mv.getParentFile.exists()) prout2_mv.getParentFile.mkdirs()
                if (!prout2_mv.exists()) prout2_mv.createNewFile()
                if (!recout2_mv.exists()) recout2_mv.createNewFile()
                if (!f1out2_mv.exists()) f1out2_mv.createNewFile()

                val fw11_mv = new FileWriter(prout1_mv, true)
                val fw12_mv = new FileWriter(recout1_mv, true)
                val fw13_mv = new FileWriter(f1out1_mv, true)

                val fw21_mv = new FileWriter(prout2_mv, true)
                val fw22_mv = new FileWriter(recout2_mv, true)
                val fw23_mv = new FileWriter(f1out2_mv, true)

                fw11_mv.write(s"${gammaValue}:${runNo}:${prec_mv_probec}\n")
                fw12_mv.write(s"${gammaValue}:${runNo}:${rec_mv_probec}\n")
                fw13_mv.write(s"${gammaValue}:${runNo}:${f1_mv_probec}\n")

                fw21_mv.write(s"${gammaValue}:${runNo}:${prec_mv_piec}\n")
                fw22_mv.write(s"${gammaValue}:${runNo}:${rec_mv_piec}\n")
                fw23_mv.write(s"${gammaValue}:${runNo}:${f1_mv_piec}\n")

                fw11_mv.close()
                fw12_mv.close()
                fw13_mv.close()

                fw21_mv.close()
                fw22_mv.close()
                fw23_mv.close()
            }

            /****************************************************3*************************************************/

            if (dst == "enh")
            {
                var (wf, xf, yf, zf) = (0, fpf, 0, 0)
                var (df, af, bf, cf) = (0, fpf, 0, 0)

                val results_f_12_r = evaluateResults(ground_truth_of_f("fighting(id1,id2)"), probec_intervals(map_of_f("fighting(id1,id2)"), thround))
                val results_f_12_p = evaluateResults(ground_truth_of_f("fighting(id1,id2)"), piec(map_of_f("fighting(id1,id2)"), thround))

                wf = wf + results_f_12_r._1
                xf = xf + results_f_12_r._2
                yf = yf + results_f_12_r._3
                zf = zf + results_f_12_r._4

                df = df + results_f_12_p._1
                af = af + results_f_12_p._2
                bf = bf + results_f_12_p._3
                cf = cf + results_f_12_p._4

                val results_f_21_r = evaluateResults(ground_truth_of_f("fighting(id2,id1)"), probec_intervals(map_of_f("fighting(id2,id1)"), thround))
                val results_f_21_p = evaluateResults(ground_truth_of_f("fighting(id2,id1)"), piec(map_of_f("fighting(id2,id1)"), thround))

                wf = wf + results_f_21_r._1
                xf = xf + results_f_21_r._2
                yf = yf + results_f_21_r._3
                zf = zf + results_f_21_r._4

                df = df + results_f_21_p._1
                af = af + results_f_21_p._2
                bf = bf + results_f_21_p._3
                cf = cf + results_f_21_p._4

                val results_f_26_r = evaluateResults(ground_truth_of_f("fighting(id2,id6)"), probec_intervals(map_of_f("fighting(id2,id6)"), thround))
                val results_f_26_p = evaluateResults(ground_truth_of_f("fighting(id2,id6)"), piec(map_of_f("fighting(id2,id6)"), thround))

                wf = wf + results_f_26_r._1
                xf = xf + results_f_26_r._2
                yf = yf + results_f_26_r._3
                zf = zf + results_f_26_r._4

                df = df + results_f_26_p._1
                af = af + results_f_26_p._2
                bf = bf + results_f_26_p._3
                cf = cf + results_f_26_p._4

                val results_f_62_r = evaluateResults(ground_truth_of_f("fighting(id6,id2)"), probec_intervals(map_of_f("fighting(id6,id2)"), thround))
                val results_f_62_p = evaluateResults(ground_truth_of_f("fighting(id6,id2)"), piec(map_of_f("fighting(id6,id2)"), thround))

                wf = wf + results_f_62_r._1
                xf = xf + results_f_62_r._2
                yf = yf + results_f_62_r._3
                zf = zf + results_f_62_r._4

                df = df + results_f_62_p._1
                af = af + results_f_62_p._2
                bf = bf + results_f_62_p._3
                cf = cf + results_f_62_p._4

                val results_f_34_r = evaluateResults(ground_truth_of_f("fighting(id3,id4)"), probec_intervals(map_of_f("fighting(id3,id4)"), thround))
                val results_f_34_p = evaluateResults(ground_truth_of_f("fighting(id3,id4)"), piec(map_of_f("fighting(id3,id4)"), thround))

                wf = wf + results_f_34_r._1
                xf = xf + results_f_34_r._2
                yf = yf + results_f_34_r._3
                zf = zf + results_f_34_r._4

                df = df + results_f_34_p._1
                af = af + results_f_34_p._2
                bf = bf + results_f_34_p._3
                cf = cf + results_f_34_p._4

                val results_f_43_r = evaluateResults(ground_truth_of_f("fighting(id4,id3)"), probec_intervals(map_of_f("fighting(id4,id3)"), thround))
                val results_f_43_p = evaluateResults(ground_truth_of_f("fighting(id4,id3)"), piec(map_of_f("fighting(id4,id3)"), thround))

                wf = wf + results_f_43_r._1
                xf = xf + results_f_43_r._2
                yf = yf + results_f_43_r._3
                zf = zf + results_f_43_r._4

                df = df + results_f_43_p._1
                af = af + results_f_43_p._2
                bf = bf + results_f_43_p._3
                cf = cf + results_f_43_p._4

                val results_f_45_r = evaluateResults(ground_truth_of_f("fighting(id4,id5)"), probec_intervals(map_of_f("fighting(id4,id5)"), thround))
                val results_f_45_p = evaluateResults(ground_truth_of_f("fighting(id4,id5)"), piec(map_of_f("fighting(id4,id5)"), thround))

                wf = wf + results_f_45_r._1
                xf = xf + results_f_45_r._2
                yf = yf + results_f_45_r._3
                zf = zf + results_f_45_r._4

                df = df + results_f_45_p._1
                af = af + results_f_45_p._2
                bf = bf + results_f_45_p._3
                cf = cf + results_f_45_p._4

                val results_f_54_r = evaluateResults(ground_truth_of_f("fighting(id5,id4)"), probec_intervals(map_of_f("fighting(id5,id4)"), thround))
                val results_f_54_p = evaluateResults(ground_truth_of_f("fighting(id5,id4)"), piec(map_of_f("fighting(id5,id4)"), thround))

                wf = wf + results_f_54_r._1
                xf = xf + results_f_54_r._2
                yf = yf + results_f_54_r._3
                zf = zf + results_f_54_r._4

                df = df + results_f_54_p._1
                af = af + results_f_54_p._2
                bf = bf + results_f_54_p._3
                cf = cf + results_f_54_p._4

                val results_f_67_r = evaluateResults(ground_truth_of_f("fighting(id6,id7)"), probec_intervals(map_of_f("fighting(id6,id7)"), thround))
                val results_f_67_p = evaluateResults(ground_truth_of_f("fighting(id6,id7)"), piec(map_of_f("fighting(id6,id7)"), thround))

                wf = wf + results_f_67_r._1
                xf = xf + results_f_67_r._2
                yf = yf + results_f_67_r._3
                zf = zf + results_f_67_r._4

                df = df + results_f_67_p._1
                af = af + results_f_67_p._2
                bf = bf + results_f_67_p._3
                cf = cf + results_f_67_p._4

                val results_f_76_r = evaluateResults(ground_truth_of_f("fighting(id7,id6)"), probec_intervals(map_of_f("fighting(id7,id6)"), thround))
                val results_f_76_p = evaluateResults(ground_truth_of_f("fighting(id7,id6)"), piec(map_of_f("fighting(id7,id6)"), thround))

                wf = wf + results_f_76_r._1
                xf = xf + results_f_76_r._2
                yf = yf + results_f_76_r._3
                zf = zf + results_f_76_r._4

                df = df + results_f_76_p._1
                af = af + results_f_76_p._2
                bf = bf + results_f_76_p._3
                cf = cf + results_f_76_p._4

                val prec_f_probec = if ((zf + xf) == 0) 0
                                    else BigDecimal(zf / (zf + xf).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val rec_f_probec = if ((zf + yf) == 0) 0
                                   else BigDecimal(zf / (zf + yf).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val f1_f_probec = if ((prec_f_probec + rec_f_probec) == 0.0) 0
                                  else BigDecimal((2 * prec_f_probec * rec_f_probec) / (prec_f_probec + rec_f_probec)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                println(s"precision_fighting_prob-ec : ${prec_f_probec}")
                println(s"recall_fighting_prob-ec : ${rec_f_probec}")
                println(s"f1-score_fighting_prob-ec : ${f1_f_probec}")

                val prec_f_piec = if ((cf + af) == 0) 0
                                  else BigDecimal(cf / (cf + af).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val rec_f_piec = if ((cf + bf) == 0) 0
                                 else BigDecimal(cf / (cf + bf).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val f1_f_piec = if ((prec_f_piec + rec_f_piec) == 0.0) 0
                                else BigDecimal((2 * prec_f_piec * rec_f_piec) / (prec_f_piec + rec_f_piec)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                println(s"precision_fighting_piec : ${prec_f_piec}")
                println(s"recall_fighting_piec : ${rec_f_piec}")
                println(s"f1-score_fighting_piec : ${f1_f_piec}\n\n")

                val prout1_f = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PROBEC/$noiseAmount/fighting_${noiseAmount}-${dst}-t${thround}.fighting-precision.data")
                val recout1_f = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PROBEC/$noiseAmount/fighting_${noiseAmount}-${dst}-t${thround}.fighting-recall.data")
                val f1out1_f = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PROBEC/$noiseAmount/fighting_${noiseAmount}-${dst}-t${thround}.fighting-fmeasure.data")

                if (!prout1_f.getParentFile.exists()) prout1_f.getParentFile.mkdirs()
                if (!prout1_f.exists()) prout1_f.createNewFile()
                if (!recout1_f.exists()) recout1_f.createNewFile()
                if (!f1out1_f.exists()) f1out1_f.createNewFile()

                val prout2_f = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PIEC/$noiseAmount/fighting_${noiseAmount}-${dst}-t${thround}.fighting-precision.data")
                val recout2_f = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PIEC/$noiseAmount/fighting_${noiseAmount}-${dst}-t${thround}.fighting-recall.data")
                val f1out2_f = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PIEC/$noiseAmount/fighting_${noiseAmount}-${dst}-t${thround}.fighting-fmeasure.data")

                if (!prout2_f.getParentFile.exists()) prout2_f.getParentFile.mkdirs()
                if (!prout2_f.exists()) prout2_f.createNewFile()
                if (!recout2_f.exists()) recout2_f.createNewFile()
                if (!f1out2_f.exists()) f1out2_f.createNewFile()

                val fw11_f = new FileWriter(prout1_f, true)
                val fw12_f = new FileWriter(recout1_f, true)
                val fw13_f = new FileWriter(f1out1_f, true)

                val fw21_f = new FileWriter(prout2_f, true)
                val fw22_f = new FileWriter(recout2_f, true)
                val fw23_f = new FileWriter(f1out2_f, true)

                fw11_f.write(s"${gammaValue}:${runNo}:${prec_f_probec}\n")
                fw12_f.write(s"${gammaValue}:${runNo}:${rec_f_probec}\n")
                fw13_f.write(s"${gammaValue}:${runNo}:${f1_f_probec}\n")

                fw21_f.write(s"${gammaValue}:${runNo}:${prec_f_piec}\n")
                fw22_f.write(s"${gammaValue}:${runNo}:${rec_f_piec}\n")
                fw23_f.write(s"${gammaValue}:${runNo}:${f1_f_piec}\n")

                fw11_f.close()
                fw12_f.close()
                fw13_f.close()

                fw21_f.close()
                fw22_f.close()
                fw23_f.close()
            }
        }

        println(".-\n\n")
    }
}
