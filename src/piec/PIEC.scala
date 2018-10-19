package piec

import java.io.{File, PrintWriter}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

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

    def piec(a: Array[Double], t: Double) : ListBuffer[(Int, Int)] =
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
                if (start == end && a(start) == t)
                {
                    output += ((start, end))
                }
                flag = false
                start += 1
            }
        }

        val result = getCredible(prefixInput, output)

        result
    }

    def findAllProbECResultFiles(parentDir: File): List[File] =
    {
        print("Gathering all necessary .result files... ")
        var auxQueue = new mutable.Queue[File]()
        var resultsList = new ListBuffer[File]()

        if (parentDir.isDirectory)
        {
            auxQueue ++= parentDir.listFiles()

            while (auxQueue.nonEmpty)
            {
                var currentFile = auxQueue.dequeue()

                if (currentFile.isDirectory)
                {
                    auxQueue ++= currentFile.listFiles()
                }
                else
                {
                    if (currentFile.getName.startsWith("Prob-EC") && currentFile.getName.endsWith(".result"))
                    {
                        resultsList += currentFile
                    }
                }
            }
        }

        println("Done.")
        Thread.sleep(1000l)
        resultsList.toList
    }

    def setUpGroundTruth(dir: File): mutable.HashMap[String, ArrayBuffer[Int]] =
    {
        var map = new mutable.HashMap[String, ArrayBuffer[Int]]()

        for (f <- dir.listFiles().filter(_.getName.contains("Grp")).filterNot(_.getName.contains("Appearence")).filterNot(_.getName.contains("Movement")))
        {
            print(s"Setting up Groung Truth for $f... ")
            var lines = Source.fromFile(f).getLines().filter(_.nonEmpty).filter(!_.startsWith("%")).mkString("\n")
            var timepoints = new ArrayBuffer[Int]()

            val pattern = "happensAt\\( ([abcefghijlmnotv_]+)\\( grp_ID([0-9]+), \\[ ([a-zA-Z0-9_]+), ([a-zA-Z0-9_]+) \\]\\), ([0-9]+)".r

            for (p <- pattern.findAllMatchIn(lines).toList)
            {
                var hle1 = s"${p.group(1)}(${p.group(3)},${p.group(4)})=true"
                //var hle2 = s"${p.group(1)}(${p.group(4)},${p.group(3)})=true"   // Two alternatives are needed here
                var tp = p.group(5).toInt / 40

                // First alternative
                if (!map.contains(hle1))
                {
                    timepoints = new ArrayBuffer[Int]()
                    timepoints += tp
                    map += ((hle1, timepoints))
                }
                else
                {
                    timepoints = map(hle1)

                    if (!timepoints.contains(tp))
                    {
                        timepoints += tp
                        map += ((hle1, timepoints))
                    }
                }

                // Second alternative
//                if (!map.contains(hle2))
//                {
//                    timepoints = new ArrayBuffer[Int]()
//                    timepoints += tp
//                    map += ((hle2, timepoints))
//                }
//                else
//                {
//                    timepoints = map(hle2)
//
//                    if (!timepoints.contains(tp))
//                    {
//                        timepoints += tp
//                        map += ((hle2, timepoints))
//                    }
//                }
            }

            println("Done.")
        }

        map
    }

    def expandIntervals(listOfIntervals: ListBuffer[(Int, Int)]): ArrayBuffer[Int] =
    {
        var result = new ArrayBuffer[Int]()

        for ((s, t) <- listOfIntervals)
        {
            for (i <- s to t)
            {
                result += i
            }
        }

        result
    }

    def evaluateResults(p: ArrayBuffer[Int], g: ArrayBuffer[Int]): (Int, Int, Int) =
    {
        var (tp, fp, fn) = (0, 0, 0)
        var (i, j) = (0, 0)
        var pDone = false

        while ((i < p.length) && (j < g.length))
        {
            if (p(i) == g(j))
            {
                tp += 1
                i += 1
                j += 1
            }
            else
            {
                if (p(i) < g(j))
                {
                    fp += 1
                    i += 1
                }
                else
                {
                    fn += 1
                    j += 1
                }
            }

            if (i == p.length) pDone = true
        }

        if (pDone)
        {
            fn += (g.length - j)
        }
        else
        {
            fp += (p.length - i)
        }

        (tp, fp, fn)
    }

    val parentDir = new File("/home/cgvlas/Demokritos/TPLP-Data.v2012.11.10/experiments")
    val dataSetDir = new File("/home/cgvlas/Demokritos/TPLP-Data.v2012.11.10/dataset")
    val probECResultFiles = findAllProbECResultFiles(parentDir)

    val pw = new PrintWriter("/home/cgvlas/Demokritos/TPLP-Data.v2012.11.10/experiments/PIEC/PIEC.result", "UTF-8")

    for (f <- probECResultFiles)
    {
        var path_of_f = f.getAbsolutePath

        var pathParts = path_of_f.split("\\Q/\\E")
        var videoId = pathParts.init.last
        var enhorig = if (pathParts.init.init.last.split("\\Q_\\E").head == "orig") "original" else "enhanced"
        var dataDir = new File(s"/home/cgvlas/Demokritos/TPLP-Data.v2012.11.10/dataset/$enhorig/$videoId/")

        var ground_truth_of_f = setUpGroundTruth(dataDir)

        pw.write(s"$path_of_f\n")

        println(path_of_f)

        var lines_of_f = Source.fromFile(f).getLines().filter(_.nonEmpty).filter(!_.startsWith("%")).mkString("\n")
        var map_of_f = new mutable.HashMap[String, ListBuffer[(Double, Int)]]()
        var tuples = new ListBuffer[(Double, Int)]()

        val pattern = "([0-9.e-]+)::holdsAt([(])([abcefghijlmnotv_]+)([(])([a-zA-z0-9, ]+)([)=]+)([a-zA-Z0-9]+), ([0-9]+)".r

        for (p <- pattern.findAllMatchIn(lines_of_f).toList)
        {
            var hle = s"${p.group(3)}${p.group(4)}${p.group(5)}${p.group(6)}${p.group(7)}"
            var pt = (p.group(1).toDouble, p.group(8).toInt)

            if (!map_of_f.contains(hle))
            {
                tuples = new ListBuffer[(Double, Int)]()
                tuples += pt
                map_of_f += ((hle, tuples))
            }
            else
            {
                tuples = map_of_f(hle)

                if (!tuples.contains(pt))
                {
                    tuples += pt
                    map_of_f += ((hle, tuples))
                }
            }
        }

        for (hle <- map_of_f.keys)
        {
            var start = map_of_f(hle).head._2
            var end = map_of_f(hle).last._2

            if (map_of_f(hle).length != (end - start + 1))
            {
                //println(s"There are discontiguities for HLE $hle...")

                var discontiguities = new ListBuffer[(Int, Int)]()
                var prev = 0
                var curr = 0

                for ((p, t) <- map_of_f(hle))
                {
                    curr = t

                    if ((curr > (prev + 1)) && prev != 0)
                    {
                        discontiguities += ((prev - start + 1, curr - start - 1))
                    }

                    prev = curr
                }

                var copy = map_of_f(hle)

                for ((d_start, d_end) <- discontiguities)
                {
                    for (i <- d_start to d_end)
                    {
                        copy.insert(i, (0.0, i + start))
                    }
                }

                map_of_f += ((hle, copy))
                //println("Discontiguities fixed...")
            }
        }

        for (hle <- map_of_f.keys)
        {
            if (!ground_truth_of_f.contains(hle))
            {
                ground_truth_of_f += ((hle, new ArrayBuffer[Int]()))
                println(s"WARNING: Found a HLE ($hle) that is not present in the Ground Truth...")
            }

            var probabilities = (for (tuple <- map_of_f(hle)) yield tuple._1).toArray
            var timepoints = (for (tuple <- map_of_f(hle)) yield tuple._2).toArray
            var offset = timepoints.head

            var tmpResult = piec(probabilities, 0.5)
            var finalResult = new ListBuffer[(Int, Int)]()

            for (interval <- tmpResult)
            {
                finalResult += ((interval._1 + offset, interval._2 + offset))
            }

            pw.write(s"HLE: $hle\n")
            pw.write(s"PIEC INPUT (probabilities):  ${probabilities.mkString("[", ", ", "]")}\n")
            pw.write(s"           (timepoints):     ${timepoints.mkString("[", ", ", "]")}\n")
            pw.write(s"PIEC OUTPUT (intervals):     ${finalResult.mkString("[", ", ", "]")}\n")

            var groundTruthPoints = ground_truth_of_f(hle)
            var finalResultPoints = expandIntervals(finalResult)

            pw.write(s"            (timepoints):    ${finalResultPoints.mkString("[", ", ", "]")}\n")
            pw.write(s"GROUND TRUTH (timepoints):   ${groundTruthPoints.mkString("[", ", ", "]")}\n")

            var (tp, fp, fn) = evaluateResults(finalResultPoints, groundTruthPoints)

            var precision = if ((tp + fp) == 0) -1.0 else 5/(4 + 3).toDouble
            var recall = if ((tp + fn) == 0) -1.0 else 9/(2 + 5).toDouble
            var f1Score = if ((tp + fp + fn) == 0) -1.0 else 2*8/(2*7 + 9 + 3).toDouble

            pw.write(s"PRECISION:                   $precision\n")
            pw.write(s"RECALL:                      $recall\n")
            pw.write(s"F1-SCORE:                    $f1Score\n\n")
        }

        pw.write("\n\n")
    }

    pw.close()
}
