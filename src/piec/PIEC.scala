package piec

import java.io.{File, PrintWriter}

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

        //println(s"Most credible maximal interval(s): ${result.mkString("[", ",", "]")}.")
        result
    }

    val dataSetType = Seq[String]("enh", "orig")
    val gammas = 0.0 to 8.0 by 0.5
    val noiseAmount = Seq[String]("smooth", "intermediate", "strong")
    //val noiseType = Seq[String]("clean", "noisy")
    val runs = 1 to 5
    val thresholds = 0.3 to 0.7 by 0.2
    val videoID = 1 to 30
    val videos = Seq[String]("01-Walk1", "02-Walk2", "03-Walk3",
        "04-Browse1", "05-Browse2", "06-Browse3",
        "07-Browse4", "08-Browse_WhileWaiting1", "09-Browse_WhileWaiting2",
        "10-Rest_InChair", "11-Rest_SlumpOnFloor", "12-Rest_WiggleOnFloor",
        "13-Rest_FallOnFloor", "14-LeftBag", "15-LeftBag_AtChair",
        "16-LeftBag_BehindChair", "17-LeftBox", "18-LeftBag_PickedUp",
        "19-Meet_WalkTogether1", "20-Meet_WalkTogether2", "21-Meet_WalkSplit",
        "22-Meet_Split3rdGuy", "23-Meet_Crowd", "24-Meet_Split",
        "25-Fight_RunAway1", "26-Fight_RunAway2", "27-Fight_OneManDown1",
        "27-Fight_OneManDown2", "27-Fight_OneManDown3", "28-Fight_Chase")

    def findAllProbECResultFiles(): List[File] =
    {
        print("Gathering all necessary .result files... ")
        var resultsList = new ListBuffer[File]()

        for (dt <- dataSetType)
        {
            var dtFull = if (dt == "enh") "enh_all" else "orig_all"

            for (ga <- gammas)
            {
                if (ga == 0.0)
                {
                    for (vd <- videos)
                    {
                        resultsList += new File(s"/home/cgvlas/Demokritos/TPLP-toy/experiments/clean_data/${dtFull}/${vd}/Prob-EC.result")
                    }
                }
                else
                {
                    for (na <- noiseAmount)
                    {
                        for (rn <- runs)
                        {
                            for (vd <- videos)
                            {
                                resultsList += new File(s"/home/cgvlas/Demokritos/TPLP-toy/experiments/noisy_data/${ga}/${dtFull}_run_${rn}/${vd}/Prob-EC_${na}.result")
                            }
                        }
                    }
                }
            }
        }

        println("Done.")
        resultsList.toList
    }

    val parentDir = new File("/home/cgvlas/Demokritos/TPLP-toy/experiments")
    val probECResultFiles = findAllProbECResultFiles()

    //val outFile = new File("../out/PIEC.result")

    //if (!outFile.exists())
    //{
    //    outFile.createNewFile()
    //}

    val pw = new PrintWriter("/home/cgvlas/Demokritos/TPLP-toy/experiments/PIEC/PIEC.result", "UTF-8")
    val pw2 = new PrintWriter("/home/cgvlas/Demokritos/TPLP-toy/experiments/PIEC/PYEC.input", "UTF-8")

    for (f <- probECResultFiles)
    {
        var path_of_f = f.getAbsolutePath
        //pw.write(s"FILE: $path_of_f\n")

        println(path_of_f)

        var lines_of_f = Source.fromFile(f).getLines().filter(_.nonEmpty).filter(!_.startsWith("%")).mkString("\n")
        var map_of_f = new mutable.HashMap[String, ListBuffer[(Double, Int)]]()
        var tuples = new ListBuffer[(Double, Int)]()

        val pattern = "([0-9.e-]+)::holdsAt([(])([a-zA-Z]+)([(])([a-zA-z0-9, ]+)([)=]+)([a-zA-Z0-9]+), ([0-9]+)".r

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
            var probabilities = (for (tuple <- map_of_f(hle)) yield tuple._1).toArray
            var timepoints = (for (tuple <- map_of_f(hle)) yield tuple._2).toArray
            var offset = timepoints.head

            var tmpResult = piec(probabilities, 0.5)
            var finalResult = new ListBuffer[(Int, Int)]()

            for (interval <- tmpResult)
            {
                finalResult += ((interval._1 + offset, interval._2 + offset))
            }

            pw.write(s"$path_of_f:$hle:${finalResult.mkString("[", ", ", "]")}\n")
            pw2.write(s"$path_of_f:$hle:${probabilities.mkString("[", ", ", "]")}:${offset}\n")
            //pw.write(s"PIEC INPUT (probabilities):  ${probabilities.mkString("[", ", ", "]")}\n")
            //pw.write(s"           (timepoints):     ${timepoints.mkString("[", ", ", "]")}\n")
            //pw.write(s"PIEC OUTPUT:                 ${finalResult.mkString("[", ", ", "]")}\n\n")
        }

        //pw.write("\n\n")
    }

    pw.close()
    pw2.close()
}
