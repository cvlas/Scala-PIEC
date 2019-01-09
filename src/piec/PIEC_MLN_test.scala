package piec

import java.io.{File, FileWriter, PrintWriter}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

/**
  * @author Christos G. Vlassopoulos (cvlas@iit.demokritos.gr)
  *
  *         2019-01-07
  */

object PIEC_MLN_test extends App
{
    def formatGround(x: List[(Int, Int)], n: Int): Array[Int] =
    {
        val start = Array.fill[Int](n)(0)

        for (i <- x)
        {
            val y = i._1
            start(y) = 1
        }

        for ((a, b) <- x)
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

    def mlnec_intervals(z: Array[Double], threshold: Double): Array[Int] =
    {
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

    def getCredible(tuples: List[(Int, Int)], prefix: Array[Double]): Array[Int] =
    {
        if (tuples.isEmpty)
        {
            Array.fill[Int](prefix.length)(0)
        }
        else
        {
            if (tuples.length == 1)
            {
                formatGround(tuples, prefix.length)
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

                formatGround(overlap.toList, prefix.length)
            }
        }
    }

    def piec(inputArray: Array[Double], threshold: Double): Array[Int] =
    {
        val prefixInput = new Array[Double](inputArray.length)
        prefixInput(0) = inputArray(0)

        for (i <- 1 until inputArray.length)
        {
            prefixInput(i) = BigDecimal(prefixInput(i - 1) + inputArray(i)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
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
            prefix(i) = BigDecimal(prefix(i - 1) + inputArray(i)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
        }

        dp(inputArray.length - 1) = prefix(inputArray.length - 1)

        for (i <- inputArray.length - 2 until -1 by -1)
        {
            dp(i) = BigDecimal(Seq(dp(i + 1), prefix(i)).max).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
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
            val n = g.length

            if (n > 0)
            {
                for (i <- 0 until n)
                {
                    if (g(i) == 0 && p(i) == 0)
                    {
                        tn += 1
                    }
                    if (g(i) == 0 && p(i) == 1)
                    {
                        fp += 1
                    }
                    if (g(i) == 1 && p(i) == 0)
                    {
                        fn += 1
                    }
                    if (g(i) == 1 && p(i) == 1)
                    {
                        tp += 1
                    }
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

    val mlnHomePath = "C:\\Users\\chris\\Desktop\\PIEC-paper-master\\data MLN-EC\\MLN-EC"
    val oslaHomePath = s"$mlnHomePath\\OSLa_results+figures"
    val dnHomePath = s"$mlnHomePath\\DN_results+figures"

    val oslaHome = new File(oslaHomePath)

    for (ltaHome <- oslaHome.listFiles())
    {
        println(s"% ------------------------------------------------------------------------------\n% LONG-TERM ACTIVITY: ${ltaHome.getName}\n% ------------------------------------------------------------------------------\n")

        val resultsHomePath = s"${ltaHome.getAbsolutePath}\\results"
        // val annotationHomePath = s"$resultsHomePath\\annotation"

        val resultsHome = new File(resultsHomePath)
        // val annotationHome = new File(annotationHomePath)

        for (resultFile <- resultsHome.listFiles().filter(x => x.getName.endsWith(".csv")))
        {
            val resultFileNameSplit = resultFile.getName.split("\\.")(0).split("_")
            val (idA, idB) = (resultFileNameSplit(resultFileNameSplit.length - 1), resultFileNameSplit(resultFileNameSplit.length - 2))

            val lastLine = Source.fromFile(resultFile).getLines().foldLeft(Option.empty[String]) { case (_, line) => Some(line) }

            val lastPoint = lastLine match
            {
                case Some(value) => value.split(",")(0).toInt
                case None => 0
            }

            for (th <- 0.5 to 0.91 by 0.2)
            {
                val probabilities = Array.fill(lastPoint + 1)(0.0)
                val groundTruth = Array.fill(lastPoint + 1)(0)

                for (line <- Source.fromFile(resultFile).getLines())
                {
                    val (time, probability, annot) = line.split(",") match
                    {
                        case Array(s1, s2, s3) => (s1.toInt, s2.toDouble, s3.toInt)
                    }

                    probabilities(time) = probability
                    groundTruth(time) = annot
                }

                val thround = BigDecimal(th).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                println(s"Working on ${resultFile.getName} MLN-EC result file, threshold: $thround...")

                val results_m = evaluateResults(groundTruth, mlnec_intervals(probabilities, thround))
                println(s"MLN-EC: $results_m")
                val results_p = evaluateResults(groundTruth, piec(probabilities, thround))
                println(s"PIEC: $results_p\n")

                val prec_mlnec = if ((results_m._4 + results_m._2) == 0) 0
                                     else BigDecimal(results_m._4 / (results_m._4 + results_m._2).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val rec_mlnec = if ((results_m._4 + results_m._3) == 0) 0
                                    else BigDecimal(results_m._4 / (results_m._4 + results_m._3).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val f1_mlnec = if ((prec_mlnec + rec_mlnec) == 0.0) 0
                                   else BigDecimal((2 * prec_mlnec * rec_mlnec) / (prec_mlnec + rec_mlnec)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                println(s"precision_${ltaHome.getName}_mln-ec : ${prec_mlnec}")
                println(s"recall_${ltaHome.getName}_mln-ec : ${rec_mlnec}")
                println(s"f1-score_${ltaHome.getName}_mln-ec : ${f1_mlnec}")

                val prec_piec = if ((results_p._4 + results_p._2) == 0) 0
                else BigDecimal(results_p._4 / (results_p._4 + results_p._2).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val rec_piec = if ((results_p._4 + results_p._3) == 0) 0
                else BigDecimal(results_p._4 / (results_p._4 + results_p._3).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val f1_piec = if ((prec_piec + rec_piec) == 0.0) 0
                else BigDecimal((2 * prec_piec * rec_piec) / (prec_piec + rec_piec)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                println(s"precision_${ltaHome.getName}_piec : ${prec_piec}")
                println(s"recall_${ltaHome.getName}_piec : ${rec_piec}")
                println(s"f1-score_${ltaHome.getName}_piec : ${f1_piec}\n\n")
            }

            println("% ------------------------------------------------------------------------------\n")
        }
    }
}
