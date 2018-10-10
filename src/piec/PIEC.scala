package piec2

import java.io.File

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
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

    def piec(a: Array[Double], t: Double) : Unit =
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

        //var stepCounter = 0

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

                    //println(s"1:Added interval ${(start, end)}, now output is $output\n")
                }

                flag = true
                end += 1
            }
            else
            {
                if (start < end && flag)
                {
                    output += ((start, end-1))

                    //println(s"2:Added interval ${(start, end-1)}, now output is $output\n")
                }
                if (start == end && a(start) == t)
                {
                    output += ((start, end))

                    //println(s"3:Added interval ${(start, end)}, now output is $output\n")
                }
                flag = false
                start += 1
            }
        }

        val result = getCredible(prefixInput, output)

        println(s"Most credible maximal interval(s): ${result.mkString("[", ",", "]")}.")
    }

    def findAllProbECResultFiles(parentDir: File): List[File] =
    {
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

        resultsList.toList
    }

    val parentDir = new File("C:\\Users\\Christos\\Demokritos\\TPLP-Data.v2012.11.10\\experiments\\clean_data\\orig_all\\01-Walk1")
    val probECResultFiles = findAllProbECResultFiles(parentDir)

    for (f <- probECResultFiles)
    {
        var path_of_f = f.getAbsolutePath
        var lines_of_f = Source.fromFile(f).getLines().filter(_.nonEmpty).filter(!_.startsWith("%")).mkString("\n")
        var map_of_f = new mutable.HashMap[String, ListBuffer[(Double, Int)]]()
        var tuples = new ListBuffer[(Double, Int)]()

        val pattern = "([0-9.]+)::holdsAt([(])([a-zA-Z]+)([(])([a-zA-z0-9, ]+)([)=]+)([a-zA-Z0-9]+), ([0-9]+)".r

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
                println(s"There are discontiguities for HLE $hle...")

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
                println("Discontiguities fixed...")
            }
        }

        for (hle <- map_of_f.keys)
        {
            println(s"We are about to run PIEC on file $path_of_f")
            println(s"HLE: $hle")

            var probs = (for (tuple <- map_of_f(hle)) yield tuple._1).toArray
            var timepoints = (for (tuple <- map_of_f(hle)) yield tuple._2).toArray

            println(s"Probs: ${probs.mkString("[", ", ", "]")}")
            println(s"Timepoints: ${timepoints.mkString("[", ", ", "]")}")

            piec(probs, 0.5)

            println(s"... with an offset of ${timepoints.head}.\n")
        }
    }
}
