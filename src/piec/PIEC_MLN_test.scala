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

    val mlnHomePath = "/home/cgvlas/Demokritos/PIEC-paper/data MLN-EC/MLN-EC"
    val oslaHomePath = s"$mlnHomePath/OSLa_results+figures"
    val dnHomePath = s"$mlnHomePath/DN_results+figures"

    val oslaHome = new File(oslaHomePath)

    for (ltaHome <- oslaHome.listFiles().filter(_.isDirectory).sortWith(_.getName < _.getName))
    {
        println(s"% ------------------------------------------------------------------------------\n% LONG-TERM ACTIVITY: ${ltaHome.getName}\n% ------------------------------------------------------------------------------\n")

        val resultsHomePath = s"${ltaHome.getAbsolutePath}/results"
        // val annotationHomePath = s"$resultsHomePath\\annotation"

        val resultsHome = new File(resultsHomePath)
        // val annotationHome = new File(annotationHomePath)

        var compPrec = new File(s"${ltaHome.getAbsolutePath}/piec_mln_comparison_precision.csv")
        var compRec = new File(s"${ltaHome.getAbsolutePath}/piec_mln_comparison_recall.csv")
        var compF1 = new File(s"${ltaHome.getAbsolutePath}/piec_mln_comparison_f-measure.csv")
        if (!compPrec.getParentFile.exists()) compPrec.getParentFile.mkdirs()
        if (!compPrec.exists()) compPrec.createNewFile()
        if (!compRec.getParentFile.exists()) compRec.getParentFile.mkdirs()
        if (!compRec.exists()) compRec.createNewFile()
        if (!compF1.getParentFile.exists()) compF1.getParentFile.mkdirs()
        if (!compF1.exists()) compF1.createNewFile()

        val fw1 = new FileWriter(compPrec,true)
        val fw2 = new FileWriter(compRec,true)
        val fw3 = new FileWriter(compF1,true)

        fw1.write(s"Thr\tPIEC-Micro\tMLN-EC-Micro\tPIEC-Macro\tMLN-EC-Macro\n")
        fw2.write(s"Thr\tPIEC-Micro\tMLN-EC-Micro\tPIEC-Macro\tMLN-EC-Macro\n")
        fw3.write(s"Thr\tPIEC-Micro\tMLN-EC-Micro\tPIEC-Macro\tMLN-EC-Macro\n")

        for (th <- 0.1 to 1.01 by 0.1)
        {
            val thround = BigDecimal(th).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

            var (microSumTP_mlnec, microSumFP_mlnec, microSumFN_mlnec, microSumTP_piec, microSumFP_piec, microSumFN_piec) = (0, 0, 0, 0, 0, 0)
            var (macroPrecSum_mlnec, macroRecSum_mlnec, macroF1Sum_mlnec, macroPrecSum_piec, macroRecSum_piec, macroF1Sum_piec) = (0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
            var (tzoufia_m, tzoufia_p) = (0, 0)

            for (resultFile <- resultsHome.listFiles().filter(x => x.getName.endsWith(".csv")).sortWith(_.getName < _.getName))
            {
                //println(s"Working on ${resultFile.getName} MLN-EC result file, threshold: $thround...")

                val resultFileNameSplit = resultFile.getName.split("\\.")(0).split("_")
                val (idA, idB) = (resultFileNameSplit(resultFileNameSplit.length - 1), resultFileNameSplit(resultFileNameSplit.length - 2))

                val lastLine = Source.fromFile(resultFile).getLines().foldLeft(Option.empty[String]) { case (_, line) => Some(line) }

                val lastPoint = lastLine match
                {
                    case Some(value) => value.split(",")(0).toInt
                    case None => 0
                }

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

                val results_m = evaluateResults(groundTruth, mlnec_intervals(probabilities, thround))
                var tzoufio_m = false
                //println(s"MLN-EC: (${results_m._2}, ${results_m._3}, ${results_m._4})")

                val results_p = evaluateResults(groundTruth, piec(probabilities, thround))
                var tzoufio_p = false
                //println(s"PIEC:   (${results_p._2}, ${results_p._3}, ${results_p._4})\n")

                val prec_mlnec = if ((results_m._4 + results_m._2) == 0)
                                 {
                                     tzoufio_m = true
                                     0
                                 }
                                 else BigDecimal(results_m._4 / (results_m._4 + results_m._2).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val rec_mlnec = if ((results_m._4 + results_m._3) == 0)
                                {
                                    tzoufio_m = true
                                    0
                                }
                                else BigDecimal(results_m._4 / (results_m._4 + results_m._3).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val f1_mlnec = if ((prec_mlnec + rec_mlnec) == 0.0)
                               {
                                   tzoufio_m = true
                                   0
                               }
                               else BigDecimal((2 * prec_mlnec * rec_mlnec) / (prec_mlnec + rec_mlnec)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                if (!tzoufio_m)
                {
                    microSumTP_mlnec = microSumTP_mlnec + results_m._4
                    microSumFP_mlnec = microSumFP_mlnec + results_m._2
                    microSumFN_mlnec = microSumFN_mlnec + results_m._3

                    macroPrecSum_mlnec = macroPrecSum_mlnec + prec_mlnec
                    macroRecSum_mlnec = macroRecSum_mlnec + rec_mlnec
                    macroF1Sum_mlnec = macroF1Sum_mlnec + f1_mlnec
                }
                else tzoufia_m = tzoufia_m + 1

                //println(s"precision_${ltaHome.getName}_mln-ec : ${prec_mlnec}")
                //println(s"recall_${ltaHome.getName}_mln-ec : ${rec_mlnec}")
                //println(s"f1-score_${ltaHome.getName}_mln-ec : ${f1_mlnec}")

                val prec_piec = if ((results_p._4 + results_p._2) == 0)
                                {
                                    tzoufio_p = true
                                    0
                                }
                                else BigDecimal(results_p._4 / (results_p._4 + results_p._2).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val rec_piec = if ((results_p._4 + results_p._3) == 0)
                               {
                                   tzoufio_p = true
                                   0
                               }
                               else BigDecimal(results_p._4 / (results_p._4 + results_p._3).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val f1_piec = if ((prec_piec + rec_piec) == 0.0)
                              {
                                  tzoufio_p = true
                                  0
                              }
                              else BigDecimal((2 * prec_piec * rec_piec) / (prec_piec + rec_piec)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                if (!tzoufio_p)
                {
                    microSumTP_piec = microSumTP_piec + results_p._4
                    microSumFP_piec = microSumFP_piec + results_p._2
                    microSumFN_piec = microSumFN_piec + results_p._3

                    macroPrecSum_piec = macroPrecSum_piec + prec_piec
                    macroRecSum_piec = macroRecSum_piec + rec_piec
                    macroF1Sum_piec = macroF1Sum_piec + f1_piec
                }
                else tzoufia_p = tzoufia_p + 1

                //println(s"precision_${ltaHome.getName}_piec : ${prec_piec}")
                //println(s"recall_${ltaHome.getName}_piec : ${rec_piec}")
                //println(s"f1-score_${ltaHome.getName}_piec : ${f1_piec}\n\n")
            }

            val macroPrecAvg_mlnec = BigDecimal(macroPrecSum_mlnec/(resultsHome.listFiles().count(x => x.getName.endsWith(".csv")) - tzoufia_m).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val macroPrecAvg_piec = BigDecimal(macroPrecSum_piec/(resultsHome.listFiles().count(x => x.getName.endsWith(".csv")) - tzoufia_p).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val macroRecAvg_mlnec = BigDecimal(macroRecSum_mlnec/(resultsHome.listFiles().count(x => x.getName.endsWith(".csv")) - tzoufia_m).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val macroRecAvg_piec = BigDecimal(macroRecSum_piec/(resultsHome.listFiles().count(x => x.getName.endsWith(".csv")) - tzoufia_p).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val macroF1Avg_mlnec = BigDecimal(macroF1Sum_mlnec/(resultsHome.listFiles().count(x => x.getName.endsWith(".csv")) - tzoufia_m).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val macroF1Avg_piec = BigDecimal(macroF1Sum_piec/(resultsHome.listFiles().count(x => x.getName.endsWith(".csv")) - tzoufia_p).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

            println(s"MACRO-AVERAGE F1-SCORE FOR MLN-EC + THRESHOLD: ${macroF1Avg_mlnec}")
            println(s"MACRO-AVERAGE F1-SCORE FOR MLN-EC + PIEC     : ${macroF1Avg_piec}")

            val microPrec_mlnec = if ((microSumTP_mlnec + microSumFP_mlnec) == 0) 0
                                  else BigDecimal(microSumTP_mlnec / (microSumTP_mlnec + microSumFP_mlnec).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val microRec_mlnec = if ((microSumTP_mlnec + microSumFN_mlnec) == 0) 0
                                 else BigDecimal(microSumTP_mlnec / (microSumTP_mlnec + microSumFN_mlnec).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val microF1_mlnec = if ((microPrec_mlnec + microRec_mlnec) == 0.0) 0
                                else BigDecimal((2 * microPrec_mlnec * microRec_mlnec) / (microPrec_mlnec + microRec_mlnec)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

            val microPrec_piec = if ((microSumTP_piec + microSumFP_piec) == 0) 0
                                 else BigDecimal(microSumTP_piec / (microSumTP_piec + microSumFP_piec).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val microRec_piec = if ((microSumTP_piec + microSumFN_piec) == 0) 0
                                else BigDecimal(microSumTP_piec / (microSumTP_piec + microSumFN_piec).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            val microF1_piec = if ((microPrec_piec + microRec_piec) == 0.0) 0
                               else BigDecimal((2 * microPrec_piec * microRec_piec) / (microPrec_piec + microRec_piec)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

            println(s"MICRO-AVERAGE F1-SCORE FOR MLN-EC + THRESHOLD: ${microF1_mlnec}")
            println(s"MICRO-AVERAGE F1-SCORE FOR MLN-EC + PIEC     : ${microF1_piec}")

            fw1.write(f"${thround}\t${microPrec_piec}%1.7f\t${microPrec_mlnec}%1.7f\t${macroPrecAvg_piec}%1.7f\t${macroPrecAvg_mlnec}%1.7f\n")
            fw2.write(f"${thround}\t${microRec_piec}%1.7f\t${microRec_mlnec}%1.7f\t${macroRecAvg_piec}%1.7f\t${macroRecAvg_mlnec}%1.7f\n")
            fw3.write(f"${thround}\t${microF1_piec}%1.7f\t${microF1_mlnec}%1.7f\t${macroF1Avg_piec}%1.7f\t${macroF1Avg_mlnec}%1.7f\n")

            println("% ------------------------------------------------------------------------------\n")
        }

        fw1.close()
        fw2.close()
        fw3.close()
    }
}
