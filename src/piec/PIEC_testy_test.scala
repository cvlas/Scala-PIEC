package piec

import java.io.{File, FileWriter, PrintWriter}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

/**
  * @author Christos G. Vlassopoulos (cvlas@iit.demokritos.gr)
  *
  *         2019-01-07
  */

object PIEC_testy_test extends App
{
    /**
      * Takes as input a list of intervals of the form (start, end) and returns
      * an array of 0's and 1's. The i-th element of the resulting Array is a 1
      * if i is contained in at least one interval in the input List,
      * otherwise is a 0.
      *
      * @param x the input intervals List
      * @param n the length of the resulting Array
      * @return an array of 0's and 1's
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
    def mlnec_intervals(z: Array[Double], threshold: Double): Array[Int] =
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
    def getCredible(listOfIntervals: List[(Int, Int)], prefix: Array[Double]): Array[Int] =
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
      * The PIEC algorithm. It originally appears in "Artikis A., Makris E. and Paliouras G.,
      * A Probabilistic Interval-based Event Calculus for Activity Recognition.
      * In Annals of Mathematics and Artificial Intelligence, 2019".
      *
      * Implementation in Scala by Christos Vlassopoulos.
      *
      * @param inputArray the array that contains all of the input instantaneous
      *                   probabilities
      * @param threshold the desired probability threshold
      * @param fw FileWriter variable that writes down probabilistic maximal intervals
      *           formatted in such a way, so that they can be visualized into ScalaTikz(*)
      *           plots.
      * @return credible probabilistic maximal intervals, formatted according to
      *         the formatGround method.
      *
      * (*) ScalaTikz is an open-source library for PGF/TIKZ vector graphics, developed
      *     by Evangelos Michelioudakis (https://github.com/vagmcs/ScalaTIKZ)
      */
    def piec(inputArray: Array[Double], threshold: Double, fw: FileWriter) : Array[Int] =
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

        /**
          * Export probabilistic maximal intervals into ScalaTikz format for plot
          * making.
          */
        val number_of_intervals = result.length
        fw.write(s"};\n\n\\addplot[solid, thin, color=orange, mark=none, mark size=1.0pt, mark options={draw=orange, fill=orange}] coordinates {\n")
        var height = 1.08
        var currentEnd = -1
        for (j <- result.indices)
        {
            if (result(j)._1 < currentEnd)
            {
                height = BigDecimal(height + 0.02).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            }
            else
            {
                height = 1.08
            }

            for (i <- result(j)._1 to result(j)._2)
            {
                fw.write(s"(${i.toDouble}, $height)\n")
            }
            fw.write(s"\n")
            currentEnd = result(j)._2
        }
        fw.write(s"};\n\n")

        /**
          * Distinguish the credible probabilistic maximal intervals, according
          * to the credibility strategy.
          */
        getCredible(result.toList, prefixInput)
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

    val resultingArray = mlnec_intervals(testArr, 0.7)
    println(s"${resultingArray.mkString("[", ", ", "]")}")*/

    val mlnHomePath = "/home/cgvlas/Demokritos/PIEC-paper/data MLN-EC/MLN-EC"
    val oslaHomePath = s"$mlnHomePath/DN_results+figures"

    val oslaHome = new File(oslaHomePath)

    for (ltaHome <- oslaHome.listFiles().filter(_.isDirectory).sortWith(_.getName < _.getName))
    {
        println(s"% ------------------------------------------------------------------------------\n% LONG-TERM ACTIVITY: ${ltaHome.getName}\n% ------------------------------------------------------------------------------\n")

        val resultsHomePath = s"${ltaHome.getAbsolutePath}/results"
        // val annotationHomePath = s"$resultsHomePath\\annotation"

        val resultsHome = new File(resultsHomePath)
        // val annotationHome = new File(annotationHomePath)

        for (th <- 0.7 to 0.71 by 0.1)
        {
            val thround = BigDecimal(th).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

            for (resultFile <- resultsHome.listFiles().filter(x => x.getName.endsWith(".csv")).sortWith(_.getName < _.getName))
            {
                //println(s"Working on ${resultFile.getName} MLN-EC result file, threshold: $thround...")

                val lastLine = Source.fromFile(resultFile).getLines().foldLeft(Option.empty[String])
                {
                    case (_, line) => Some(line)
                }

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

                val y_max = 1.1

                val file = new File(s"${resultsHomePath}/${resultFile.getName}.piec.tex")
                val fw = new FileWriter(file)
                fw.write(s"\\documentclass{standalone}\n\n" +
                    "\\usepackage{tikz,pgfplots,luatex85}\n" +
                    "\\usetikzlibrary{plotmarks}\n" +
                    "\\usetikzlibrary{patterns}\n" +
                    "\\usetikzlibrary{pgfplots.polar}\n" +
                    "\\pgfplotsset{compat=newest}\n\n" +
                    "\\begin{document}\n" +
                    "\\pagestyle{empty}\n" +
                    "\\begin{tikzpicture}[scale=2]\n" +
                    s"\\begin{axis}[xmode=linear, ymode=linear, zmode=linear, axis background/.style={fill=white}, axis x line=box, axis y line=box, xticklabels={,,}, x tick label style={rotate=0}, y tick label style={rotate=0}, xlabel=Time, ylabel=Probability of LTA, xmin=0.0, xmax=${lastPoint}, ymin=0.0, ymax=${y_max}]\n\n" +
                    "\\addplot[solid, thin, color=black, mark=none, mark size=1.0pt, mark options={draw=black, fill=black}] coordinates {\n")
                for (i <- probabilities.indices)
                {
                    fw.write(s"(${i}, ${probabilities(i)})\n")
                }
                fw.write("};\n\n\\addplot[dashed, thin, color=gray, mark=none, mark size=1.0pt, mark options={draw=gray, fill=gray}] coordinates {\n")
                for (i <- probabilities.indices)
                {
                    fw.write(s"(${i}, 0.7)\n")
                }

                fw.write("};\n\n\\addplot[solid, thin, color=blue!75!black, mark=none, mark size=1.0pt, mark options={draw=blue!75!black, fill=blue!75!black}] coordinates {\n")
                for (i <- groundTruth.indices)
                {
                    if (groundTruth(i) != 0)
                    {
                        fw.write(s"(${i}, 1.02)\n")
                    }
                    else
                    {
                        fw.write("\n")
                    }
                }

                fw.write("};\n\n\\addplot[solid, thin, color=green!75!black, mark=none, mark size=1.0pt, mark options={draw=green!75!black, fill=green!75!black}] coordinates {\n")
                val mintervals = mlnec_intervals(probabilities, thround)
                for (i <- mintervals.indices)
                {
                    if (mintervals(i) != 0)
                    {
                        fw.write(s"(${i}, 1.04)\n")
                    }
                    else
                    {
                        fw.write("\n")
                    }
                }

                val pintervals = piec(probabilities, thround)
                fw.write("};\n\n\\addplot[solid, thin, color=red!75!black, mark=none, mark size=1.0pt, mark options={draw=red!75!black, fill=red!75!black}] coordinates {\n")
                for (i <- pintervals.indices)
                {
                    if (pintervals(i) != 0)
                    {
                        fw.write(s"(${i}, 1.06)\n")
                    }
                    else
                    {
                        fw.write("\n")
                    }
                }

                fw.write(s"};\n\n\\end{axis}\n\\end{tikzpicture}\n\\end{document}")
                fw.close()
            }
        }
    }
}
