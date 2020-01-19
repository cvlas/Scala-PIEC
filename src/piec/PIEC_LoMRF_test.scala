package piec

import java.io.{File, FileWriter, PrintWriter}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

/**
  * @author Christos G. Vlassopoulos (cvlas@iit.demokritos.gr)
  *
  *         2019-01-07
  */

object PIEC_LoMRF_test extends App
{
    /**
      * Takes as input a list of intervals of the form (start, end) and returns
      * an Array of 0's and 1's. The i-th element of the resulting Array is a 1
      * if i is contained in at least one interval in the input List,
      * otherwise it is a 0.
      *
      * @param x the input intervals List
      * @param n the length of the resulting Array
      * @return an Array of 0's and 1's
      */
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

    /**
      * Takes as input an Array of instantaneous probabilities and applies
      * a probability threshold on them. The i-th element of the resulting
      * Array is a 1 if the corresponding instantaneous probability is equal to
      * or above the threshold, and a 0 otherwise.
      *
      * @param z the Array of instantaneous probabilities
      * @param threshold the probability threshold
      * @return an Array of 0's and 1's
      */
    def lomrf_intervals(z: Array[Double], threshold: Double): Array[Int] =
    {
        var tmpArray = Array[Int]()

        for (x <- z)
        {
            if (x >= threshold)
            {
                tmpArray = tmpArray :+ 1
            }
            else
            {
                tmpArray = tmpArray :+ 0
            }
        }

        tmpArray
    }

    /**
      * Takes a list of probabilistic maximal intervals and filters out all but
      * the credible ones.
      *
      * Credibility = the sum of the instantaneous probabilities
      *
      * @param listOfIntervals the complete List of probabilistic maximal intervals
      * @param prefix Array containing the progressive instantaneous probability sums
      * @return an Array that contains 1's for timepoints that belong in credible
      *         probabilistic maximal intervals and 0's everywhere else
      */
    def getCredible1(listOfIntervals: List[(Int, Int)], prefix: Array[Double]): Array[Int] =
    {
        if (listOfIntervals.isEmpty)
        {
            Array.fill[Int](prefix.length)(0)
        }
        else
        {
            if (listOfIntervals.length == 1)
            {
                formatGround(listOfIntervals, prefix.length)
            }
            else
            {
                var overlap = new ListBuffer[(Int, Int)]()

                var tmp = 0
                var currentEnd = listOfIntervals(0)._2
                var currentStart = listOfIntervals(0)._1
                var currentInterval = listOfIntervals(0)
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

                for (i <- 1 until listOfIntervals.size)
                {
                    if (listOfIntervals(i)._1 < currentEnd)
                    {
                        if (BigDecimal(prefix(listOfIntervals(i)._2) - prefix(listOfIntervals(i)._1 - 1)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble >= maxCredibility)
                        {
                            maxCredibility = BigDecimal(prefix(listOfIntervals(i)._2) - prefix(listOfIntervals(i)._1 - 1)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                            currentInterval = listOfIntervals(i)
                        }

                        currentEnd = listOfIntervals(i)._2
                    }
                    else
                    {
                        overlap += currentInterval

                        currentInterval = listOfIntervals(i)

                        currentEnd = listOfIntervals(i)._2
                        maxCredibility = BigDecimal(prefix(listOfIntervals(i)._2) - prefix(listOfIntervals(i)._1 - 1)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                    }
                }

                overlap += currentInterval

                formatGround(overlap.toList, prefix.length)
            }
        }
    }

    /**
      * Takes a list of probabilistic maximal intervals and filters out all but
      * the credible ones.
      *
      * Credibility = the sum of the cubes of the instantaneous probabilities
      *
      * @param listOfIntervals the complete List of probabilistic maximal intervals
      * @param probs Array containing the instantaneous probabilities
      * @return an Array that contains 1's for timepoints that belong in credible
      *         probabilistic maximal intervals and 0's everywhere else
      */
    def getCredible2(listOfIntervals: List[(Int, Int)], probs: Array[Double], threshold: Double): Array[Int] =
    {
        if (listOfIntervals.isEmpty)
        {
            Array.fill[Int](probs.length)(0)
        }
        else
        {
            if (listOfIntervals.length == 1)
            {
                formatGround(listOfIntervals, probs.length)
            }
            else
            {
                var overlap = new ListBuffer[(Int, Int)]()

                var tmp = 0
                var currentInterval = listOfIntervals(0)
                var currentEnd = currentInterval._2
                var currentStart = currentInterval._1
                var length = currentEnd - currentStart + 1
                var maxCredibility = 0.0

                // If interval begins at timepoint 0
                if (currentStart == 0)
                {
                    // Credibility should be equal to the prefix at the end
                    // TODO: Check for possibly better rounding options
                    val sq_probs = for (i <- currentStart to currentEnd) yield BigDecimal(math.pow(probs(i) + threshold, 3)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                    val sum_sq_probs = sq_probs.sum
                    maxCredibility = BigDecimal(sum_sq_probs).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                }
                else
                {
                    // Calculate credibility as a difference of prefixes
                    // TODO: Check for possibly better rounding options
                    val sq_probs = for (i <- currentStart to currentEnd) yield BigDecimal(math.pow(probs(i) + threshold, 3)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                    val sum_sq_probs = sq_probs.sum
                    maxCredibility = BigDecimal(sum_sq_probs).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                }

                for (i <- 1 until listOfIntervals.size)
                {
                    length = listOfIntervals(i)._2 - listOfIntervals(i)._1 + 1

                    if (listOfIntervals(i)._1 < currentEnd)
                    {
                        val sq_probs = for (i <- listOfIntervals(i)._1 to listOfIntervals(i)._2) yield BigDecimal(math.pow(probs(i) + threshold, 3)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                        val sum_sq_probs = sq_probs.sum

                        if (BigDecimal(sum_sq_probs).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble >= maxCredibility)
                        {
                            maxCredibility = BigDecimal(sum_sq_probs).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                            currentInterval = listOfIntervals(i)
                        }

                        currentEnd = listOfIntervals(i)._2
                    }
                    else
                    {
                        overlap += currentInterval

                        currentInterval = listOfIntervals(i)

                        currentEnd = listOfIntervals(i)._2
                        val sq_probs = for (i <- listOfIntervals(i)._1 to listOfIntervals(i)._2) yield BigDecimal(math.pow(probs(i) + threshold, 3)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                        val sum_sq_probs = sq_probs.sum
                        maxCredibility = BigDecimal(sum_sq_probs).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                    }
                }

                overlap += currentInterval

                formatGround(overlap.toList, probs.length)
            }
        }
    }

    def piec(inputArray: Array[Double], threshold: Double, cred_flag: Boolean) : Array[Int] =
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
        var i = 0

        while (i < inputArray.length)
        {
            val x = inputArray(i)
            inputArray(i) = BigDecimal(x - threshold).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            i = i + 1
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

        if (!cred_flag)
            getCredible1(result.toList, prefixInput)
        else
            getCredible2(result.toList, inputArray, threshold)
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

    /*val testArr = Array[Double](1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.6, 1.0, 1.0, 1.0, 1.0, 1.0, 0.6, 1.0, 1.0, 1.0, 0.6, 1.0, 0.6, 1.0, 0.6, 0.5, 1.0, 1.0, 0.5, 1.0, 1.0, 1.0, 0.5, 1.0, 0.65, 1.0, 0.6, 0.6, 0.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.6, 1.0, 1.0)

    val resultingArray = lomrf_intervals(testArr, 0.7)
    println(s"${resultingArray.mkString("[", ", ", "]")}")*/

    val mlnHomePath = "/home/cgvlas/Demokritos/PIEC-paper/data MLN-EC/MLN-EC"
    val dnHomePath = s"$mlnHomePath/DN_results+figures"

    val dnHome = new File(dnHomePath)

    for (ltaHome <- dnHome.listFiles().filter(_.isDirectory).sortWith(_.getName < _.getName))
    {
        println(s"% ------------------------------------------------------------------------------\n% LONG-TERM ACTIVITY: ${ltaHome.getName}\n% ------------------------------------------------------------------------------\n")

        val resultsHomePath = s"${ltaHome.getAbsolutePath}/results"
        // val annotationHomePath = s"$resultsHomePath\\annotation"

        val resultsHome = new File(resultsHomePath)
        // val annotationHome = new File(annotationHomePath)

        var compPrec = new File(s"${ltaHome.getAbsolutePath}/piec_lomrf_comparison_precision.csv")
        var compRec = new File(s"${ltaHome.getAbsolutePath}/piec_lomrf_comparison_recall.csv")
        var compF1 = new File(s"${ltaHome.getAbsolutePath}/piec_lomrf_comparison_f-measure.csv")
        if (!compPrec.getParentFile.exists()) compPrec.getParentFile.mkdirs()
        if (!compPrec.exists()) compPrec.createNewFile()
        if (!compRec.getParentFile.exists()) compRec.getParentFile.mkdirs()
        if (!compRec.exists()) compRec.createNewFile()
        if (!compF1.getParentFile.exists()) compF1.getParentFile.mkdirs()
        if (!compF1.exists()) compF1.createNewFile()

        val fw1 = new FileWriter(compPrec,true)
        val fw2 = new FileWriter(compRec,true)
        val fw3 = new FileWriter(compF1,true)

        fw1.write(s"Thr\tPIEC1-Micro\tPIEC2-Micro\tLoMRF-Micro\tPIEC1-Macro\tPIEC2-Macro\tLoMRF-Macro\n")
        fw2.write(s"Thr\tPIEC1-Micro\tPIEC2-Micro\tLoMRF-Micro\tPIEC1-Macro\tPIEC2-Macro\tLoMRF-Macro\n")
        fw3.write(s"Thr\tPIEC1-Micro\tPIEC2-Micro\tLoMRF-Micro\tPIEC1-Macro\tPIEC2-Macro\tLoMRF-Macro\n")

        for (th <- 0.1 to 0.91 by 0.1)
        {
            val thround = BigDecimal(th).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

            var (microSumTP_lomrf, microSumFP_lomrf, microSumFN_lomrf, microSumTP_piec1, microSumFP_piec1, microSumFN_piec1, microSumTP_piec2, microSumFP_piec2, microSumFN_piec2) = (0, 0, 0, 0, 0, 0, 0, 0, 0)
            var (macroPrecSum_lomrf, macroRecSum_lomrf, macroF1Sum_lomrf, macroPrecSum_piec1, macroRecSum_piec1, macroF1Sum_piec1, macroPrecSum_piec2, macroRecSum_piec2, macroF1Sum_piec2) = (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
            var (all_negatives_counter_m, all_negatives_counter_p1, all_negatives_counter_p2) = (0, 0, 0)

            for (resultFile <- resultsHome.listFiles().filter(x => x.getName.endsWith(".csv")).sortWith(_.getName < _.getName))
            {
                //println(s"Working on ${resultFile.getName} LoMRF result file, threshold: $thround...")

                val lastLine = Source.fromFile(resultFile).getLines().foldLeft(Option.empty[String]) { case (_, line) => Some(line) }

                val lastPoint = lastLine match
                {
                    case Some(value) => value.split(",")(0).toInt
                    case None => 0
                }

                var probabilities = Array.fill(lastPoint + 1)(0.0)
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

                val mintervals = lomrf_intervals(probabilities, thround)
                val results_m = evaluateResults(groundTruth, mintervals)
                var all_negatives_m = false
                println(s"LoMRF: (${results_m._2}, ${results_m._3}, ${results_m._4})")

                val pintervals1 = piec(probabilities, thround, false)
                val results_p1 = evaluateResults(groundTruth, pintervals1)
                var all_negatives_p1 = false
                println(s"PIEC1:  (${results_p1._2}, ${results_p1._3}, ${results_p1._4})")

                // Restoring probabilities...
                probabilities = probabilities.map(x => x + thround)

                val pintervals2 = piec(probabilities, thround, true)
                val results_p2 = evaluateResults(groundTruth, pintervals2)
                var all_negatives_p2 = false
                println(s"PIEC (AC):  (${results_p2._2}, ${results_p2._3}, ${results_p2._4})\n")

                // Restoring probabilities...
                probabilities = probabilities.map(x => x + thround)

                var totalOut = new File(s"${resultFile.getAbsolutePath}.out")
                if (!totalOut.exists()) totalOut.createNewFile()

                val fw = new FileWriter(totalOut,true)
                fw.write(s"T,P,LoMRF,PIEC1,PIEC2,GT,Th\n")

                for (i <- probabilities.indices)
                {
                    fw.write(s"${i},${BigDecimal(probabilities(i)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}," +
                        s"${if (mintervals(i) == 0) 0 else BigDecimal(mintervals(i) + 0.05).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}," +
                        s"${if (pintervals1(i) == 0) 0 else BigDecimal(pintervals1(i) + 0.1).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}," +
                        s"${if (pintervals2(i) == 0) 0 else BigDecimal(pintervals2(i) + 0.15).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}," +
                        s"${if (groundTruth(i) == 0) 0 else BigDecimal(groundTruth(i) + 0.2).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble}," +
                        s"${thround}\n")
                }

                fw.close()

                microSumTP_lomrf = microSumTP_lomrf + results_m._4
                microSumFP_lomrf = microSumFP_lomrf + results_m._2
                microSumFN_lomrf = microSumFN_lomrf + results_m._3

                microSumTP_piec1 = microSumTP_piec1 + results_p1._4
                microSumFP_piec1 = microSumFP_piec1 + results_p1._2
                microSumFN_piec1 = microSumFN_piec1 + results_p1._3

                microSumTP_piec2 = microSumTP_piec2 + results_p2._4
                microSumFP_piec2 = microSumFP_piec2 + results_p2._2
                microSumFN_piec2 = microSumFN_piec2 + results_p2._3

                if (results_m._2 == 0 && results_m._3 == 0 && results_m._4 == 0 &&
                    results_p1._2 == 0 && results_p1._3 == 0 && results_p1._4 == 0 &&
                    results_p2._2 == 0 && results_p2._3 == 0 && results_p2._4 == 0)
                {
                    all_negatives_counter_m = all_negatives_counter_m + 1
                    all_negatives_counter_p1 = all_negatives_counter_p1 + 1
                    all_negatives_counter_p2 = all_negatives_counter_p2 + 1
                }
                else
                {
                    val prec_lomrf = if ((results_m._4 + results_m._2) == 0) 0
                                     else BigDecimal(results_m._4 / (results_m._4 + results_m._2).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                    val rec_lomrf = if ((results_m._4 + results_m._3) == 0) 0
                                    else BigDecimal(results_m._4 / (results_m._4 + results_m._3).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                    val f1_lomrf = BigDecimal((2 * results_m._4) / ((2 * results_m._4) + results_m._2 + results_m._3).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                    val prec_piec1 = if ((results_p1._4 + results_p1._2) == 0) 0
                                     else BigDecimal(results_p1._4 / (results_p1._4 + results_p1._2).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                    val rec_piec1 = if ((results_p1._4 + results_p1._3) == 0) 0
                                    else BigDecimal(results_p1._4 / (results_p1._4 + results_p1._3).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                    val f1_piec1 = BigDecimal((2 * results_p1._4) / ((2 * results_p1._4) + results_p1._2 + results_p1._3).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                    val prec_piec2 = if ((results_p2._4 + results_p2._2) == 0) 0
                                     else BigDecimal(results_p2._4 / (results_p2._4 + results_p2._2).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                    val rec_piec2 = if ((results_p2._4 + results_p2._3) == 0) 0
                                    else BigDecimal(results_p2._4 / (results_p2._4 + results_p2._3).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                    val f1_piec2 = BigDecimal((2 * results_p2._4) / ((2 * results_p2._4) + results_p2._2 + results_p2._3).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                    macroPrecSum_lomrf = macroPrecSum_lomrf + prec_lomrf
                    macroRecSum_lomrf = macroRecSum_lomrf + rec_lomrf
                    macroF1Sum_lomrf = macroF1Sum_lomrf + f1_lomrf

                    macroPrecSum_piec1 = macroPrecSum_piec1 + prec_piec1
                    macroRecSum_piec1 = macroRecSum_piec1 + rec_piec1
                    macroF1Sum_piec1 = macroF1Sum_piec1 + f1_piec1

                    macroPrecSum_piec2 = macroPrecSum_piec2 + prec_piec2
                    macroRecSum_piec2 = macroRecSum_piec2 + rec_piec2
                    macroF1Sum_piec2 = macroF1Sum_piec2 + f1_piec2
                }

                //println(s"precision_${ltaHome.getName}_mln-ec : ${prec_lomrf}")
                //println(s"recall_${ltaHome.getName}_mln-ec : ${rec_lomrf}")
                //println(s"f1-score_${ltaHome.getName}_mln-ec : ${f1_lomrf}")

                //println(s"precision_${ltaHome.getName}_piec : ${prec_piec}")
                //println(s"recall_${ltaHome.getName}_piec : ${rec_piec}")
                //println(s"f1-score_${ltaHome.getName}_piec : ${f1_piec}\n\n")

                //println(s"precision_${ltaHome.getName}_piec : ${prec_piec}")
                //println(s"recall_${ltaHome.getName}_piec : ${rec_piec}")
                //println(s"f1-score_${ltaHome.getName}_piec : ${f1_piec}\n\n")
            }

            val csv_files_count = resultsHome.listFiles().count(x => x.getName.endsWith(".csv"))

            val macroPrecAvg_lomrf = BigDecimal(macroPrecSum_lomrf/(csv_files_count - all_negatives_counter_m).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val macroPrecAvg_piec1 = BigDecimal(macroPrecSum_piec1/(csv_files_count - all_negatives_counter_p1).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val macroPrecAvg_piec2 = BigDecimal(macroPrecSum_piec2/(csv_files_count - all_negatives_counter_p2).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val macroRecAvg_lomrf = BigDecimal(macroRecSum_lomrf/(csv_files_count - all_negatives_counter_m).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val macroRecAvg_piec1 = BigDecimal(macroRecSum_piec1/(csv_files_count - all_negatives_counter_p1).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val macroRecAvg_piec2 = BigDecimal(macroRecSum_piec2/(csv_files_count - all_negatives_counter_p2).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val macroF1Avg_lomrf = BigDecimal(macroF1Sum_lomrf/(csv_files_count - all_negatives_counter_m).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val macroF1Avg_piec1 = BigDecimal(macroF1Sum_piec1/(csv_files_count - all_negatives_counter_p1).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val macroF1Avg_piec2 = BigDecimal(macroF1Sum_piec2/(csv_files_count - all_negatives_counter_p2).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

            println(s"MACRO-AVERAGE F1-SCORE FOR LoMRF + THRESHOLD: ${macroF1Avg_lomrf}")
            println(s"MACRO-AVERAGE F1-SCORE FOR LoMRF + PIEC     : ${macroF1Avg_piec1}")
            println(s"MACRO-AVERAGE F1-SCORE FOR LoMRF + PIEC (AC): ${macroF1Avg_piec2}")

            val microPrec_lomrf = if ((microSumTP_lomrf + microSumFP_lomrf) == 0) 0
                                  else BigDecimal(microSumTP_lomrf / (microSumTP_lomrf + microSumFP_lomrf).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val microRec_lomrf = if ((microSumTP_lomrf + microSumFN_lomrf) == 0) 0
                                 else BigDecimal(microSumTP_lomrf / (microSumTP_lomrf + microSumFN_lomrf).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val microF1_lomrf = if ((microPrec_lomrf + microRec_lomrf) == 0.0) 0
                                else BigDecimal((2 * microPrec_lomrf * microRec_lomrf) / (microPrec_lomrf + microRec_lomrf)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

            val microPrec_piec1 = if ((microSumTP_piec1 + microSumFP_piec1) == 0) 0
                                  else BigDecimal(microSumTP_piec1 / (microSumTP_piec1 + microSumFP_piec1).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val microRec_piec1 = if ((microSumTP_piec1 + microSumFN_piec1) == 0) 0
                                 else BigDecimal(microSumTP_piec1 / (microSumTP_piec1 + microSumFN_piec1).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val microF1_piec1 = if ((microPrec_piec1 + microRec_piec1) == 0.0) 0
                                else BigDecimal((2 * microPrec_piec1 * microRec_piec1) / (microPrec_piec1 + microRec_piec1)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

            val microPrec_piec2 = if ((microSumTP_piec2 + microSumFP_piec2) == 0) 0
                                  else BigDecimal(microSumTP_piec2 / (microSumTP_piec2 + microSumFP_piec2).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val microRec_piec2 = if ((microSumTP_piec2 + microSumFN_piec2) == 0) 0
                                 else BigDecimal(microSumTP_piec2 / (microSumTP_piec2 + microSumFN_piec2).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val microF1_piec2 = if ((microPrec_piec2 + microRec_piec2) == 0.0) 0
                                else BigDecimal((2 * microPrec_piec2 * microRec_piec2) / (microPrec_piec2 + microRec_piec2)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

            println(s"MICRO-AVERAGE F1-SCORE FOR LoMRF + THRESHOLD: ${microF1_lomrf}")
            println(s"MICRO-AVERAGE F1-SCORE FOR LoMRF + PIEC     : ${microF1_piec1}")
            println(s"MICRO-AVERAGE F1-SCORE FOR LoMRF + PIEC (AC): ${microF1_piec2}")

            fw1.write(f"${thround}\t${microPrec_piec1}%1.7f\t${microPrec_piec2}%1.7f\t${microPrec_lomrf}%1.7f\t${macroPrecAvg_piec1}%1.7f\t${macroPrecAvg_piec2}%1.7f\t${macroPrecAvg_lomrf}%1.7f\n")
            fw2.write(f"${thround}\t${microRec_piec1}%1.7f\t${microRec_piec2}%1.7f\t${microRec_lomrf}%1.7f\t${macroRecAvg_piec1}%1.7f\t${macroRecAvg_piec2}%1.7f\t${macroRecAvg_lomrf}%1.7f\n")
            fw3.write(f"${thround}\t${microF1_piec1}%1.7f\t${microF1_piec2}%1.7f\t${microF1_lomrf}%1.7f\t${macroF1Avg_piec1}%1.7f\t${macroF1Avg_piec2}%1.7f\t${macroF1Avg_lomrf}%1.7f\n")

            println("% ------------------------------------------------------------------------------\n")
        }

        fw1.close()
        fw2.close()
        fw3.close()
    }
}
