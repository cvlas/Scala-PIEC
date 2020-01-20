package piec

import java.io.{File, FileWriter}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
  * @author Christos G. Vlassopoulos (cvlas@iit.demokritos.gr)
  *
  * 2018-07-27
  */

object PIEC_test extends App
{
    /**
      * Takes as input a list of intervals of the form (start, end) and returns
      * an array of 0's and 1's. The i-th element of the resulting Array is a 1
      * if i is contained in at least one interval in the input List,
      * otherwise is a 0.
      *
      * @param x the input intervals List
      * @return an array of 0's and 1's
      */
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
    def probec_intervals(z: Array[Double], threshold: Double): Array[Int] =
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
      * @param tuples the complete List of probabilistic maximal intervals
      * @param prefix Array containing the progressive instantaneous probability sums
      * @return an Array that contains 1's for timepoints that belong in credible
      *         probabilistic maximal intervals and 0's everywhere else
      */
    def getCredible1(tuples: List[(Int, Int)], prefix: Array[Double]) : Array[Int] =
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

    /**
      * Takes a list of probabilistic maximal intervals and filters out all but
      * the credible ones.
      *
      * Credibility = the average of the instantaneous probabilities
      *
      * @param tuples the complete List of probabilistic maximal intervals
      * @param prefix Array containing the progressive instantaneous probability sums
      * @return an Array that contains 1's for timepoints that belong in credible
      *         probabilistic maximal intervals and 0's everywhere else
      */
    def getCredible2(tuples: List[(Int, Int)], prefix: Array[Double]) : Array[Int] =
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
                var currentInterval = tuples(0)
                var currentValue = currentInterval._2
                var currentStart = currentInterval._1
                var length = currentValue - currentStart + 1
                var maxCredibility = 0.0

                // If interval begins at timepoint 0
                if (currentStart == 0)
                {
                    // Credibility should be equal to the prefix at the end
                    // TODO: Check for possibly better rounding options
                    maxCredibility = BigDecimal(prefix(currentValue)/length).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                }
                else
                {
                    // Calculate credibility as a difference of prefixes
                    // TODO: Check for possibly better rounding options
                    maxCredibility = BigDecimal((prefix(currentValue) - prefix(currentStart - 1))/length).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                }

                for (i <- 1 until tuples.size)
                {
                    length = tuples(i)._2 - tuples(i)._1 + 1

                    if (tuples(i)._1 < currentValue)
                    {
                        if (BigDecimal((prefix(tuples(i)._2) - prefix(tuples(i)._1 - 1))/length).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble >= maxCredibility)
                        {
                            maxCredibility = BigDecimal((prefix(tuples(i)._2) - prefix(tuples(i)._1 - 1))/length).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                            currentInterval = tuples(i)
                        }

                        currentValue = tuples(i)._2
                    }
                    else
                    {
                        overlap += currentInterval
                        currentInterval = tuples(i)
                        currentValue = tuples(i)._2
                        maxCredibility = BigDecimal((prefix(tuples(i)._2) - prefix(tuples(i)._1 - 1))/length).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                    }
                }

                overlap += currentInterval

                formatGround(overlap.toList)
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
      * @param cred_flag Boolean flag. Setting it to true causes the algorithm to
      *                  use the alternative interval credibility strategy (method getCredible2).
      *                  Otherwise, the default interval credibility strategy is
      *                  used (i.e.: method getCredible1)
      * @return credible probabilistic maximal intervals, formatted according to
      *         the formatGround method.
      */
    def piec(inputArray: Array[Double], threshold: Double, cred_flag: Boolean) : Array[Int] =
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

        /**
          * Distinguish the credible probabilistic maximal intervals, according
          * to the desired credibility strategy.
          */
        if (!cred_flag)
            getCredible1(result.toList, prefixInput)
        else
            getCredible2(result.toList, prefixInput)
    }

    /**
      * Takes the output of PIEC and compares it to the ground truth.
      * Counts True Positives, False Positives, True Negatives and False Negatives.
      *
      * @param g the ground truth
      * @param p the output of PIEC
      * @return a tuple containing the number of True Positives, False Positives,
      *         True Negatives and False Negatives.
      */
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
                    if (g(i) == 0 && p(i) == 0) tn += 1
                    if (g(i) == 0 && p(i) == 1) fp += 1
                    if (g(i) == 1 && p(i) == 0) fn += 1
                    if (g(i) == 1 && p(i) == 1) tp += 1
                }
            }
            else
            {
                println(s"WARNING! g.length = p.length = $n!!!")
            }
        }
        else
        {
            println(s"WARNING! g.length = ${g.length}, but p.length = ${p.length}!!!")
        }

        (tn, fp, fn, tp)
    }

    val experimentsHome = new File("./eval")
    val probEC_Home = s"$experimentsHome/Prob-EC data"

    /**
      * Collection of Prob-EC output files
      */
    val probEC_result_file_paths = Array[String](
        s"$probEC_Home/enhanced_noisy_data/0.0/outputfile_noise_free",
        s"$probEC_Home/enhanced_noisy_data/0.5/enh_all_run_1/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/0.5/enh_all_run_1/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/0.5/enh_all_run_2/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/0.5/enh_all_run_2/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/0.5/enh_all_run_3/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/0.5/enh_all_run_3/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/0.5/enh_all_run_4/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/0.5/enh_all_run_4/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/0.5/enh_all_run_5/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/0.5/enh_all_run_5/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/1.0/enh_all_run_1/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/1.0/enh_all_run_1/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/1.0/enh_all_run_2/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/1.0/enh_all_run_2/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/1.0/enh_all_run_3/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/1.0/enh_all_run_3/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/1.0/enh_all_run_4/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/1.0/enh_all_run_4/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/1.0/enh_all_run_5/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/1.0/enh_all_run_5/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/1.5/enh_all_run_1/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/1.5/enh_all_run_1/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/1.5/enh_all_run_2/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/1.5/enh_all_run_2/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/1.5/enh_all_run_3/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/1.5/enh_all_run_3/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/1.5/enh_all_run_4/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/1.5/enh_all_run_4/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/1.5/enh_all_run_5/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/1.5/enh_all_run_5/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/2.0/enh_all_run_1/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/2.0/enh_all_run_1/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/2.0/enh_all_run_2/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/2.0/enh_all_run_2/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/2.0/enh_all_run_3/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/2.0/enh_all_run_3/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/2.0/enh_all_run_4/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/2.0/enh_all_run_4/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/2.0/enh_all_run_5/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/2.0/enh_all_run_5/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/2.5/enh_all_run_1/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/2.5/enh_all_run_1/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/2.5/enh_all_run_2/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/2.5/enh_all_run_2/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/2.5/enh_all_run_3/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/2.5/enh_all_run_3/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/2.5/enh_all_run_4/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/2.5/enh_all_run_4/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/2.5/enh_all_run_5/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/2.5/enh_all_run_5/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/3.0/enh_all_run_1/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/3.0/enh_all_run_1/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/3.0/enh_all_run_2/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/3.0/enh_all_run_2/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/3.0/enh_all_run_3/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/3.0/enh_all_run_3/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/3.0/enh_all_run_4/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/3.0/enh_all_run_4/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/3.0/enh_all_run_5/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/3.0/enh_all_run_5/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/3.5/enh_all_run_1/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/3.5/enh_all_run_1/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/3.5/enh_all_run_2/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/3.5/enh_all_run_2/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/3.5/enh_all_run_3/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/3.5/enh_all_run_3/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/3.5/enh_all_run_4/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/3.5/enh_all_run_4/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/3.5/enh_all_run_5/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/3.5/enh_all_run_5/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/4.0/enh_all_run_1/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/4.0/enh_all_run_1/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/4.0/enh_all_run_2/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/4.0/enh_all_run_2/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/4.0/enh_all_run_3/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/4.0/enh_all_run_3/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/4.0/enh_all_run_4/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/4.0/enh_all_run_4/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/4.0/enh_all_run_5/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/4.0/enh_all_run_5/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/4.5/enh_all_run_1/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/4.5/enh_all_run_1/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/4.5/enh_all_run_2/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/4.5/enh_all_run_2/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/4.5/enh_all_run_3/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/4.5/enh_all_run_3/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/4.5/enh_all_run_4/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/4.5/enh_all_run_4/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/4.5/enh_all_run_5/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/4.5/enh_all_run_5/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/5.0/enh_all_run_1/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/5.0/enh_all_run_1/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/5.0/enh_all_run_2/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/5.0/enh_all_run_2/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/5.0/enh_all_run_3/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/5.0/enh_all_run_3/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/5.0/enh_all_run_4/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/5.0/enh_all_run_4/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/5.0/enh_all_run_5/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/5.0/enh_all_run_5/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/5.5/enh_all_run_1/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/5.5/enh_all_run_1/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/5.5/enh_all_run_2/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/5.5/enh_all_run_2/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/5.5/enh_all_run_3/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/5.5/enh_all_run_3/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/5.5/enh_all_run_4/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/5.5/enh_all_run_4/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/5.5/enh_all_run_5/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/5.5/enh_all_run_5/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/6.0/enh_all_run_1/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/6.0/enh_all_run_1/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/6.0/enh_all_run_2/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/6.0/enh_all_run_2/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/6.0/enh_all_run_3/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/6.0/enh_all_run_3/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/6.0/enh_all_run_4/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/6.0/enh_all_run_4/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/6.0/enh_all_run_5/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/6.0/enh_all_run_5/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/6.5/enh_all_run_1/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/6.5/enh_all_run_1/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/6.5/enh_all_run_2/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/6.5/enh_all_run_2/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/6.5/enh_all_run_3/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/6.5/enh_all_run_3/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/6.5/enh_all_run_4/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/6.5/enh_all_run_4/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/6.5/enh_all_run_5/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/6.5/enh_all_run_5/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/7.0/enh_all_run_1/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/7.0/enh_all_run_1/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/7.0/enh_all_run_2/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/7.0/enh_all_run_2/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/7.0/enh_all_run_3/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/7.0/enh_all_run_3/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/7.0/enh_all_run_4/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/7.0/enh_all_run_4/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/7.0/enh_all_run_5/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/7.0/enh_all_run_5/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/7.5/enh_all_run_1/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/7.5/enh_all_run_1/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/7.5/enh_all_run_2/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/7.5/enh_all_run_2/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/7.5/enh_all_run_3/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/7.5/enh_all_run_3/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/7.5/enh_all_run_4/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/7.5/enh_all_run_4/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/7.5/enh_all_run_5/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/7.5/enh_all_run_5/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/8.0/enh_all_run_1/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/8.0/enh_all_run_1/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/8.0/enh_all_run_2/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/8.0/enh_all_run_2/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/8.0/enh_all_run_3/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/8.0/enh_all_run_3/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/8.0/enh_all_run_4/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/8.0/enh_all_run_4/outputfile_intermediate",
        s"$probEC_Home/enhanced_noisy_data/8.0/enh_all_run_5/outputfile_smooth",
        s"$probEC_Home/enhanced_noisy_data/8.0/enh_all_run_5/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/0.0/outputfile_noise_free",
        s"$probEC_Home/original_noisy_data/0.5/orig_all_run_1/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/0.5/orig_all_run_1/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/0.5/orig_all_run_2/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/0.5/orig_all_run_2/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/0.5/orig_all_run_3/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/0.5/orig_all_run_3/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/0.5/orig_all_run_4/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/0.5/orig_all_run_4/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/0.5/orig_all_run_5/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/0.5/orig_all_run_5/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/1.0/orig_all_run_1/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/1.0/orig_all_run_1/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/1.0/orig_all_run_2/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/1.0/orig_all_run_2/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/1.0/orig_all_run_3/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/1.0/orig_all_run_3/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/1.0/orig_all_run_4/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/1.0/orig_all_run_4/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/1.0/orig_all_run_5/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/1.0/orig_all_run_5/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/1.5/orig_all_run_1/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/1.5/orig_all_run_1/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/1.5/orig_all_run_2/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/1.5/orig_all_run_2/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/1.5/orig_all_run_3/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/1.5/orig_all_run_3/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/1.5/orig_all_run_4/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/1.5/orig_all_run_4/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/1.5/orig_all_run_5/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/1.5/orig_all_run_5/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/2.0/orig_all_run_1/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/2.0/orig_all_run_1/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/2.0/orig_all_run_2/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/2.0/orig_all_run_2/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/2.0/orig_all_run_3/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/2.0/orig_all_run_3/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/2.0/orig_all_run_4/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/2.0/orig_all_run_4/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/2.0/orig_all_run_5/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/2.0/orig_all_run_5/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/2.5/orig_all_run_1/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/2.5/orig_all_run_1/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/2.5/orig_all_run_2/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/2.5/orig_all_run_2/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/2.5/orig_all_run_3/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/2.5/orig_all_run_3/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/2.5/orig_all_run_4/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/2.5/orig_all_run_4/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/2.5/orig_all_run_5/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/2.5/orig_all_run_5/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/3.0/orig_all_run_1/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/3.0/orig_all_run_1/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/3.0/orig_all_run_2/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/3.0/orig_all_run_2/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/3.0/orig_all_run_3/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/3.0/orig_all_run_3/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/3.0/orig_all_run_4/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/3.0/orig_all_run_4/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/3.0/orig_all_run_5/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/3.0/orig_all_run_5/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/3.5/orig_all_run_1/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/3.5/orig_all_run_1/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/3.5/orig_all_run_2/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/3.5/orig_all_run_2/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/3.5/orig_all_run_3/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/3.5/orig_all_run_3/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/3.5/orig_all_run_4/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/3.5/orig_all_run_4/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/3.5/orig_all_run_5/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/3.5/orig_all_run_5/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/4.0/orig_all_run_1/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/4.0/orig_all_run_1/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/4.0/orig_all_run_2/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/4.0/orig_all_run_2/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/4.0/orig_all_run_3/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/4.0/orig_all_run_3/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/4.0/orig_all_run_4/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/4.0/orig_all_run_4/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/4.0/orig_all_run_5/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/4.0/orig_all_run_5/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/4.5/orig_all_run_1/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/4.5/orig_all_run_1/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/4.5/orig_all_run_2/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/4.5/orig_all_run_2/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/4.5/orig_all_run_3/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/4.5/orig_all_run_3/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/4.5/orig_all_run_4/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/4.5/orig_all_run_4/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/4.5/orig_all_run_5/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/4.5/orig_all_run_5/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/5.0/orig_all_run_1/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/5.0/orig_all_run_1/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/5.0/orig_all_run_2/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/5.0/orig_all_run_2/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/5.0/orig_all_run_3/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/5.0/orig_all_run_3/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/5.0/orig_all_run_4/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/5.0/orig_all_run_4/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/5.0/orig_all_run_5/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/5.0/orig_all_run_5/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/5.5/orig_all_run_1/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/5.5/orig_all_run_1/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/5.5/orig_all_run_2/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/5.5/orig_all_run_2/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/5.5/orig_all_run_3/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/5.5/orig_all_run_3/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/5.5/orig_all_run_4/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/5.5/orig_all_run_4/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/5.5/orig_all_run_5/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/5.5/orig_all_run_5/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/6.0/orig_all_run_1/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/6.0/orig_all_run_1/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/6.0/orig_all_run_2/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/6.0/orig_all_run_2/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/6.0/orig_all_run_3/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/6.0/orig_all_run_3/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/6.0/orig_all_run_4/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/6.0/orig_all_run_4/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/6.0/orig_all_run_5/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/6.0/orig_all_run_5/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/6.5/orig_all_run_1/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/6.5/orig_all_run_1/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/6.5/orig_all_run_2/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/6.5/orig_all_run_2/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/6.5/orig_all_run_3/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/6.5/orig_all_run_3/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/6.5/orig_all_run_4/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/6.5/orig_all_run_4/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/6.5/orig_all_run_5/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/6.5/orig_all_run_5/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/7.0/orig_all_run_1/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/7.0/orig_all_run_1/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/7.0/orig_all_run_2/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/7.0/orig_all_run_2/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/7.0/orig_all_run_3/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/7.0/orig_all_run_3/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/7.0/orig_all_run_4/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/7.0/orig_all_run_4/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/7.0/orig_all_run_5/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/7.0/orig_all_run_5/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/7.5/orig_all_run_1/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/7.5/orig_all_run_1/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/7.5/orig_all_run_2/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/7.5/orig_all_run_2/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/7.5/orig_all_run_3/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/7.5/orig_all_run_3/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/7.5/orig_all_run_4/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/7.5/orig_all_run_4/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/7.5/orig_all_run_5/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/7.5/orig_all_run_5/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/8.0/orig_all_run_1/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/8.0/orig_all_run_1/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/8.0/orig_all_run_2/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/8.0/orig_all_run_2/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/8.0/orig_all_run_3/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/8.0/orig_all_run_3/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/8.0/orig_all_run_4/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/8.0/orig_all_run_4/outputfile_intermediate",
        s"$probEC_Home/original_noisy_data/8.0/orig_all_run_5/outputfile_smooth",
        s"$probEC_Home/original_noisy_data/8.0/orig_all_run_5/outputfile_intermediate"
    )

    var activitiesMap = new mutable.HashMap[String, Array[Double]]()
    var groundTruthMap = new mutable.HashMap[String, Array[Int]]()

    /**
      * Gathering the ground truth for our experiments
      */
    groundTruthMap += (("meeting(id0,id1)", formatGround(List[(Int, Int)]((11154,11218),(19634,19655),(20797,20856)))))
    groundTruthMap += (("meeting(id1,id2)", formatGround(List[(Int, Int)]((682,1536),(12456,12578),(17820,17844),(18272,18316),(18680,18781)))))
    groundTruthMap += (("meeting(id1,id3)", formatGround(List[(Int, Int)]((18950,18975)))))
    groundTruthMap += (("meeting(id2,id6)", formatGround(List[(Int, Int)]((23453,23472)))))
    groundTruthMap += (("meeting(id4,id5)", formatGround(List[(Int, Int)]((143,610)))))
    groundTruthMap += (("meeting(id1,id0)", formatGround(List[(Int, Int)]((11154,11218),(19634,19655),(20797,20856)))))
    groundTruthMap += (("meeting(id2,id1)", formatGround(List[(Int, Int)]((682,1536),(12456,12578),(17820,17844),(18272,18316),(20528,20629)))))
    groundTruthMap += (("meeting(id3,id1)", formatGround(List[(Int, Int)]((18950,18975)))))
    groundTruthMap += (("meeting(id5,id4)", formatGround(List[(Int, Int)]((143,610)))))
    groundTruthMap += (("meeting(id6,id2)", formatGround(List[(Int, Int)]((23453,23472)))))

    groundTruthMap += (("fighting(id1,id2)", formatGround(List[(Int, Int)]((25036,25105)))))
    groundTruthMap += (("fighting(id2,id1)", formatGround(List[(Int, Int)]((25036,25105)))))
    groundTruthMap += (("fighting(id2,id6)", formatGround(List[(Int, Int)]((24160,24298)))))
    groundTruthMap += (("fighting(id6,id2)", formatGround(List[(Int, Int)]((24160,24298)))))
    groundTruthMap += (("fighting(id3,id4)", formatGround(List[(Int, Int)]((21879,22007)))))
    groundTruthMap += (("fighting(id4,id3)", formatGround(List[(Int, Int)]((21879,22007)))))
    groundTruthMap += (("fighting(id4,id5)", formatGround(List[(Int, Int)]((22387,22500),(23197,23313)))))
    groundTruthMap += (("fighting(id5,id4)", formatGround(List[(Int, Int)]((22387,22500),(23197,23313)))))
    groundTruthMap += (("fighting(id6,id7)", formatGround(List[(Int, Int)]((21371,21430)))))
    groundTruthMap += (("fighting(id7,id6)", formatGround(List[(Int, Int)]((21371,21430)))))

    groundTruthMap += (("moving(id0,id1)", formatGround(List[(Int, Int)]((11506,11592),(19656,19800),(20317,20607),(20623,20796)))))
    groundTruthMap += (("moving(id0,id2)", formatGround(List[(Int, Int)]((20346,20629)))))
    groundTruthMap += (("moving(id0,id3)", formatGround(List[(Int, Int)]((20385,20627)))))
    groundTruthMap += (("moving(id1,id2)", formatGround(List[(Int, Int)]((611,681),(12181,12455),(14133,14189),(17845,17984),(18317,18679),(20346,20607)))))
    groundTruthMap += (("moving(id1,id3)", formatGround(List[(Int, Int)]((18976,19061),(20385,20607)))))
    groundTruthMap += (("moving(id2,id3)", formatGround(List[(Int, Int)]((20385,20627)))))
    groundTruthMap += (("moving(id3,id4)", formatGround(List[(Int, Int)]((18189,18198)))))
    groundTruthMap += (("moving(id4,id5)", formatGround(List[(Int, Int)]((63,142)))))
    groundTruthMap += (("moving(id5,id6)", formatGround(List[(Int, Int)]((18117,18198)))))
    groundTruthMap += (("moving(id1,id0)", formatGround(List[(Int, Int)]((11506,11592),(19656,19800),(20317,20607),(20623,20796)))))
    groundTruthMap += (("moving(id2,id0)", formatGround(List[(Int, Int)]((20346,20629)))))
    groundTruthMap += (("moving(id3,id0)", formatGround(List[(Int, Int)]((20385,20627)))))
    groundTruthMap += (("moving(id2,id1)", formatGround(List[(Int, Int)]((611,681),(12181,12455),(14133,14189),(17845,17984),(18317,18679),(20346,20607)))))
    groundTruthMap += (("moving(id3,id1)", formatGround(List[(Int, Int)]((18976,19061),(20385,20607)))))
    groundTruthMap += (("moving(id3,id2)", formatGround(List[(Int, Int)]((20385,20627)))))
    groundTruthMap += (("moving(id4,id3)", formatGround(List[(Int, Int)]((18189,18198)))))
    groundTruthMap += (("moving(id5,id4)", formatGround(List[(Int, Int)]((63,142)))))
    groundTruthMap += (("moving(id6,id5)", formatGround(List[(Int, Int)]((18117,18198)))))

    for (piecInputFilePath <- probEC_result_file_paths)
    {
        val piecInputFile = new File(piecInputFilePath)

        for (th <- 0.5 to 0.91 by 0.2)
        {
            val thRound = BigDecimal(th).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
            println(s"Working on $piecInputFilePath Prob-EC result file, threshold: $thRound...")

            var fpf = 0
            var fpmt = 0
            var fpmv = 0

            val dataSetType = piecInputFilePath.split("/")(7).split("_")(0)
            val dst = if (dataSetType == "enhanced") "enh" else "orig"
            val gammaValue = piecInputFilePath.split("/")(8)
            val runNo = if (gammaValue == "0.0") "0" else piecInputFilePath.split("/")(9).split("_")(3)
            val noiseAmount = if (gammaValue == "0.0") "none" else piecInputFilePath.split("/")(10).split("_")(1)

            activitiesMap.clear()

            if (dst == "enh")
            {
                activitiesMap += (("fighting(id1,id2)", Array.fill(25170)(0.0)))
                activitiesMap += (("fighting(id2,id6)", Array.fill(25170)(0.0)))
                activitiesMap += (("fighting(id3,id4)", Array.fill(25170)(0.0)))
                activitiesMap += (("fighting(id4,id5)", Array.fill(25170)(0.0)))
                activitiesMap += (("fighting(id5,id6)", Array.fill(25170)(0.0)))
                activitiesMap += (("fighting(id6,id7)", Array.fill(25170)(0.0)))
                activitiesMap += (("fighting(id2,id1)", Array.fill(25170)(0.0)))
                activitiesMap += (("fighting(id6,id2)", Array.fill(25170)(0.0)))
                activitiesMap += (("fighting(id4,id3)", Array.fill(25170)(0.0)))
                activitiesMap += (("fighting(id5,id4)", Array.fill(25170)(0.0)))
                activitiesMap += (("fighting(id6,id5)", Array.fill(25170)(0.0)))
                activitiesMap += (("fighting(id7,id6)", Array.fill(25170)(0.0)))
            }

            if (dst == "orig")
            {
                activitiesMap += (("meeting(id0,id1)", Array.fill(25170)(0.0)))
                activitiesMap += (("meeting(id1,id2)", Array.fill(25170)(0.0)))
                activitiesMap += (("meeting(id1,id3)", Array.fill(25170)(0.0)))
                activitiesMap += (("meeting(id4,id5)", Array.fill(25170)(0.0)))
                activitiesMap += (("meeting(id5,id6)", Array.fill(25170)(0.0)))
                activitiesMap += (("meeting(id2,id6)", Array.fill(25170)(0.0)))
                activitiesMap += (("meeting(id1,id0)", Array.fill(25170)(0.0)))
                activitiesMap += (("meeting(id2,id1)", Array.fill(25170)(0.0)))
                activitiesMap += (("meeting(id3,id1)", Array.fill(25170)(0.0)))
                activitiesMap += (("meeting(id5,id4)", Array.fill(25170)(0.0)))
                activitiesMap += (("meeting(id6,id5)", Array.fill(25170)(0.0)))
                activitiesMap += (("meeting(id6,id2)", Array.fill(25170)(0.0)))

                activitiesMap += (("moving(id0,id1)", Array.fill(25170)(0.0)))
                activitiesMap += (("moving(id0,id2)", Array.fill(25170)(0.0)))
                activitiesMap += (("moving(id0,id3)", Array.fill(25170)(0.0)))
                activitiesMap += (("moving(id1,id2)", Array.fill(25170)(0.0)))
                activitiesMap += (("moving(id1,id3)", Array.fill(25170)(0.0)))
                activitiesMap += (("moving(id2,id3)", Array.fill(25170)(0.0)))
                activitiesMap += (("moving(id3,id4)", Array.fill(25170)(0.0)))
                activitiesMap += (("moving(id4,id5)", Array.fill(25170)(0.0)))
                activitiesMap += (("moving(id5,id6)", Array.fill(25170)(0.0)))
                activitiesMap += (("moving(id1,id0)", Array.fill(25170)(0.0)))
                activitiesMap += (("moving(id2,id0)", Array.fill(25170)(0.0)))
                activitiesMap += (("moving(id3,id0)", Array.fill(25170)(0.0)))
                activitiesMap += (("moving(id2,id1)", Array.fill(25170)(0.0)))
                activitiesMap += (("moving(id3,id1)", Array.fill(25170)(0.0)))
                activitiesMap += (("moving(id3,id2)", Array.fill(25170)(0.0)))
                activitiesMap += (("moving(id4,id3)", Array.fill(25170)(0.0)))
                activitiesMap += (("moving(id5,id4)", Array.fill(25170)(0.0)))
                activitiesMap += (("moving(id6,id5)", Array.fill(25170)(0.0)))
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

                if (!activitiesMap.contains(hle))
                {
                    if (pred == "meeting" && prob >= thRound)
                    {
                        fpmt = fpmt + 1
                    }
                    if (pred == "moving" && prob >= thRound)
                    {
                        fpmv = fpmv + 1
                    }
                    if (pred == "fighting" && prob >= thRound)
                    {
                        fpf = fpf + 1
                    }
                }
                else
                {
                    activitiesMap(hle)(time) = prob
                }
            }

            /**
              * Running the PIEC algorithm for all activities of interest,
              * collecting results and calculating stats.
              */
            if (dst == "orig")
            {
                var (wmt, xmt, ymt, zmt) = (0, fpmt, 0, 0)
                var (dmt, amt, bmt, cmt) = (0, fpmt, 0, 0)
                var (tmt, umt, pmt, smt) = (0, fpmt, 0, 0)

                val results_mt_01_r = evaluateResults(groundTruthMap("meeting(id0,id1)"), probec_intervals(activitiesMap("meeting(id0,id1)"), thRound))
                val results_mt_01_p0 = evaluateResults(groundTruthMap("meeting(id0,id1)"), piec(activitiesMap("meeting(id0,id1)"), thRound, false))
                activitiesMap("meeting(id0,id1)") = activitiesMap("meeting(id0,id1)").map(x => x + thRound)
                val results_mt_01_p1 = evaluateResults(groundTruthMap("meeting(id0,id1)"), piec(activitiesMap("meeting(id0,id1)"), thRound, true))

                wmt = wmt + results_mt_01_r._1
                xmt = xmt + results_mt_01_r._2
                ymt = ymt + results_mt_01_r._3
                zmt = zmt + results_mt_01_r._4

                dmt = dmt + results_mt_01_p0._1
                amt = amt + results_mt_01_p0._2
                bmt = bmt + results_mt_01_p0._3
                cmt = cmt + results_mt_01_p0._4

                tmt = tmt + results_mt_01_p1._1
                umt = umt + results_mt_01_p1._2
                pmt = pmt + results_mt_01_p1._3
                smt = smt + results_mt_01_p1._4

                val results_mt_12_r = evaluateResults(groundTruthMap("meeting(id1,id2)"), probec_intervals(activitiesMap("meeting(id1,id2)"), thRound))
                val results_mt_12_p0 = evaluateResults(groundTruthMap("meeting(id1,id2)"), piec(activitiesMap("meeting(id1,id2)"), thRound, false))
                activitiesMap("meeting(id1,id2)") = activitiesMap("meeting(id1,id2)").map(x => x + thRound)
                val results_mt_12_p1 = evaluateResults(groundTruthMap("meeting(id1,id2)"), piec(activitiesMap("meeting(id1,id2)"), thRound, true))

                wmt = wmt + results_mt_12_r._1
                xmt = xmt + results_mt_12_r._2
                ymt = ymt + results_mt_12_r._3
                zmt = zmt + results_mt_12_r._4

                dmt = dmt + results_mt_12_p0._1
                amt = amt + results_mt_12_p0._2
                bmt = bmt + results_mt_12_p0._3
                cmt = cmt + results_mt_12_p0._4

                tmt = tmt + results_mt_12_p1._1
                umt = umt + results_mt_12_p1._2
                pmt = pmt + results_mt_12_p1._3
                smt = smt + results_mt_12_p1._4

                val results_mt_13_r = evaluateResults(groundTruthMap("meeting(id1,id3)"), probec_intervals(activitiesMap("meeting(id1,id3)"), thRound))
                val results_mt_13_p0 = evaluateResults(groundTruthMap("meeting(id1,id3)"), piec(activitiesMap("meeting(id1,id3)"), thRound, false))
                activitiesMap("meeting(id1,id3)") = activitiesMap("meeting(id1,id3)").map(x => x + thRound)
                val results_mt_13_p1 = evaluateResults(groundTruthMap("meeting(id1,id3)"), piec(activitiesMap("meeting(id1,id3)"), thRound, true))

                wmt = wmt + results_mt_13_r._1
                xmt = xmt + results_mt_13_r._2
                ymt = ymt + results_mt_13_r._3
                zmt = zmt + results_mt_13_r._4

                dmt = dmt + results_mt_13_p0._1
                amt = amt + results_mt_13_p0._2
                bmt = bmt + results_mt_13_p0._3
                cmt = cmt + results_mt_13_p0._4

                tmt = tmt + results_mt_13_p1._1
                umt = umt + results_mt_13_p1._2
                pmt = pmt + results_mt_13_p1._3
                smt = smt + results_mt_13_p1._4

                val results_mt_45_r = evaluateResults(groundTruthMap("meeting(id4,id5)"), probec_intervals(activitiesMap("meeting(id4,id5)"), thRound))
                val results_mt_45_p0 = evaluateResults(groundTruthMap("meeting(id4,id5)"), piec(activitiesMap("meeting(id4,id5)"), thRound, false))
                activitiesMap("meeting(id4,id5)") = activitiesMap("meeting(id4,id5)").map(x => x + thRound)
                val results_mt_45_p1 = evaluateResults(groundTruthMap("meeting(id4,id5)"), piec(activitiesMap("meeting(id4,id5)"), thRound, true))

                wmt = wmt + results_mt_45_r._1
                xmt = xmt + results_mt_45_r._2
                ymt = ymt + results_mt_45_r._3
                zmt = zmt + results_mt_45_r._4

                dmt = dmt + results_mt_45_p0._1
                amt = amt + results_mt_45_p0._2
                bmt = bmt + results_mt_45_p0._3
                cmt = cmt + results_mt_45_p0._4

                tmt = tmt + results_mt_45_p1._1
                umt = umt + results_mt_45_p1._2
                pmt = pmt + results_mt_45_p1._3
                smt = smt + results_mt_45_p1._4

                val results_mt_26_r = evaluateResults(groundTruthMap("meeting(id2,id6)"), probec_intervals(activitiesMap("meeting(id2,id6)"), thRound))
                val results_mt_26_p0 = evaluateResults(groundTruthMap("meeting(id2,id6)"), piec(activitiesMap("meeting(id2,id6)"), thRound, false))
                activitiesMap("meeting(id2,id6)") = activitiesMap("meeting(id2,id6)").map(x => x + thRound)
                val results_mt_26_p1 = evaluateResults(groundTruthMap("meeting(id2,id6)"), piec(activitiesMap("meeting(id2,id6)"), thRound, true))

                wmt = wmt + results_mt_26_r._1
                xmt = xmt + results_mt_26_r._2
                ymt = ymt + results_mt_26_r._3
                zmt = zmt + results_mt_26_r._4

                dmt = dmt + results_mt_26_p0._1
                amt = amt + results_mt_26_p0._2
                bmt = bmt + results_mt_26_p0._3
                cmt = cmt + results_mt_26_p0._4

                tmt = tmt + results_mt_26_p1._1
                umt = umt + results_mt_26_p1._2
                pmt = pmt + results_mt_26_p1._3
                smt = smt + results_mt_26_p1._4

                val results_mt_10_r = evaluateResults(groundTruthMap("meeting(id1,id0)"), probec_intervals(activitiesMap("meeting(id1,id0)"), thRound))
                val results_mt_10_p0 = evaluateResults(groundTruthMap("meeting(id1,id0)"), piec(activitiesMap("meeting(id1,id0)"), thRound, false))
                activitiesMap("meeting(id1,id0)") = activitiesMap("meeting(id1,id0)").map(x => x + thRound)
                val results_mt_10_p1 = evaluateResults(groundTruthMap("meeting(id1,id0)"), piec(activitiesMap("meeting(id1,id0)"), thRound, true))

                wmt = wmt + results_mt_10_r._1
                xmt = xmt + results_mt_10_r._2
                ymt = ymt + results_mt_10_r._3
                zmt = zmt + results_mt_10_r._4

                dmt = dmt + results_mt_10_p0._1
                amt = amt + results_mt_10_p0._2
                bmt = bmt + results_mt_10_p0._3
                cmt = cmt + results_mt_10_p0._4

                tmt = tmt + results_mt_10_p1._1
                umt = umt + results_mt_10_p1._2
                pmt = pmt + results_mt_10_p1._3
                smt = smt + results_mt_10_p1._4

                val results_mt_21_r = evaluateResults(groundTruthMap("meeting(id2,id1)"), probec_intervals(activitiesMap("meeting(id2,id1)"), thRound))
                val results_mt_21_p0 = evaluateResults(groundTruthMap("meeting(id2,id1)"), piec(activitiesMap("meeting(id2,id1)"), thRound, false))
                activitiesMap("meeting(id2,id1)") = activitiesMap("meeting(id2,id1)").map(x => x + thRound)
                val results_mt_21_p1 = evaluateResults(groundTruthMap("meeting(id2,id1)"), piec(activitiesMap("meeting(id2,id1)"), thRound, true))

                wmt = wmt + results_mt_21_r._1
                xmt = xmt + results_mt_21_r._2
                ymt = ymt + results_mt_21_r._3
                zmt = zmt + results_mt_21_r._4

                dmt = dmt + results_mt_21_p0._1
                amt = amt + results_mt_21_p0._2
                bmt = bmt + results_mt_21_p0._3
                cmt = cmt + results_mt_21_p0._4

                tmt = tmt + results_mt_21_p1._1
                umt = umt + results_mt_21_p1._2
                pmt = pmt + results_mt_21_p1._3
                smt = smt + results_mt_21_p1._4

                val results_mt_31_r = evaluateResults(groundTruthMap("meeting(id3,id1)"), probec_intervals(activitiesMap("meeting(id3,id1)"), thRound))
                val results_mt_31_p0 = evaluateResults(groundTruthMap("meeting(id3,id1)"), piec(activitiesMap("meeting(id3,id1)"), thRound, false))
                activitiesMap("meeting(id3,id1)") = activitiesMap("meeting(id3,id1)").map(x => x + thRound)
                val results_mt_31_p1 = evaluateResults(groundTruthMap("meeting(id3,id1)"), piec(activitiesMap("meeting(id3,id1)"), thRound, true))

                wmt = wmt + results_mt_31_r._1
                xmt = xmt + results_mt_31_r._2
                ymt = ymt + results_mt_31_r._3
                zmt = zmt + results_mt_31_r._4

                dmt = dmt + results_mt_31_p0._1
                amt = amt + results_mt_31_p0._2
                bmt = bmt + results_mt_31_p0._3
                cmt = cmt + results_mt_31_p0._4

                tmt = tmt + results_mt_31_p1._1
                umt = umt + results_mt_31_p1._2
                pmt = pmt + results_mt_31_p1._3
                smt = smt + results_mt_31_p1._4

                val results_mt_54_r = evaluateResults(groundTruthMap("meeting(id5,id4)"), probec_intervals(activitiesMap("meeting(id5,id4)"), thRound))
                val results_mt_54_p0 = evaluateResults(groundTruthMap("meeting(id5,id4)"), piec(activitiesMap("meeting(id5,id4)"), thRound, false))
                activitiesMap("meeting(id5,id4)") = activitiesMap("meeting(id5,id4)").map(x => x + thRound)
                val results_mt_54_p1 = evaluateResults(groundTruthMap("meeting(id5,id4)"), piec(activitiesMap("meeting(id5,id4)"), thRound, true))

                wmt = wmt + results_mt_54_r._1
                xmt = xmt + results_mt_54_r._2
                ymt = ymt + results_mt_54_r._3
                zmt = zmt + results_mt_54_r._4

                dmt = dmt + results_mt_54_p0._1
                amt = amt + results_mt_54_p0._2
                bmt = bmt + results_mt_54_p0._3
                cmt = cmt + results_mt_54_p0._4

                tmt = tmt + results_mt_54_p1._1
                umt = umt + results_mt_54_p1._2
                pmt = pmt + results_mt_54_p1._3
                smt = smt + results_mt_54_p1._4

                val results_mt_62_r = evaluateResults(groundTruthMap("meeting(id6,id2)"), probec_intervals(activitiesMap("meeting(id6,id2)"), thRound))
                val results_mt_62_p0 = evaluateResults(groundTruthMap("meeting(id6,id2)"), piec(activitiesMap("meeting(id6,id2)"), thRound, false))
                activitiesMap("meeting(id6,id2)") = activitiesMap("meeting(id6,id2)").map(x => x + thRound)
                val results_mt_62_p1 = evaluateResults(groundTruthMap("meeting(id6,id2)"), piec(activitiesMap("meeting(id6,id2)"), thRound, true))

                wmt = wmt + results_mt_62_r._1
                xmt = xmt + results_mt_62_r._2
                ymt = ymt + results_mt_62_r._3
                zmt = zmt + results_mt_62_r._4

                dmt = dmt + results_mt_62_p0._1
                amt = amt + results_mt_62_p0._2
                bmt = bmt + results_mt_62_p0._3
                cmt = cmt + results_mt_62_p0._4

                tmt = tmt + results_mt_62_p1._1
                umt = umt + results_mt_62_p1._2
                pmt = pmt + results_mt_62_p1._3
                smt = smt + results_mt_62_p1._4

                val prec_mt_probec = if ((zmt + xmt) == 0) 0
                                     else BigDecimal(zmt / (zmt + xmt).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val rec_mt_probec = if ((zmt + ymt) == 0) 0
                                    else BigDecimal(zmt / (zmt + ymt).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val f1_mt_probec = if ((prec_mt_probec + rec_mt_probec) == 0.0) 0
                                   else BigDecimal((2 * prec_mt_probec * rec_mt_probec) / (prec_mt_probec + rec_mt_probec)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                println(s"precision_meeting_prob-ec : $prec_mt_probec")
                println(s"recall_meeting_prob-ec : $rec_mt_probec")
                println(s"f1-score_meeting_prob-ec : $f1_mt_probec")

                val prec_mt_piec1 = if ((cmt + amt) == 0) 0
                                    else BigDecimal(cmt / (cmt + amt).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val rec_mt_piec1 = if ((cmt + bmt) == 0) 0
                                   else BigDecimal(cmt / (cmt + bmt).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val f1_mt_piec1 = if ((prec_mt_piec1 + rec_mt_piec1) == 0.0) 0
                                  else BigDecimal((2 * prec_mt_piec1 * rec_mt_piec1) / (prec_mt_piec1 + rec_mt_piec1)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                println(s"precision_meeting_piec : $prec_mt_piec1")
                println(s"recall_meeting_piec : $rec_mt_piec1")
                println(s"f1-score_meeting_piec : $f1_mt_piec1")

                val prec_mt_piec2 = if ((smt + umt) == 0) 0
                                    else BigDecimal(smt / (smt + umt).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val rec_mt_piec2 = if ((smt + pmt) == 0) 0
                                   else BigDecimal(smt / (smt + pmt).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val f1_mt_piec2 = if ((prec_mt_piec2 + rec_mt_piec2) == 0.0) 0
                                  else BigDecimal((2 * prec_mt_piec2 * rec_mt_piec2) / (prec_mt_piec2 + rec_mt_piec2)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                println(s"precision_meeting_piec_alt_cred : $prec_mt_piec2")
                println(s"recall_meeting_piec_alt_cred : $rec_mt_piec2")
                println(s"f1-score_meeting_piec_alt_cred : $f1_mt_piec2\n\n")

                val prout1_mt = new File(s"./eval/statistics/PROBEC/$noiseAmount/meeting_${noiseAmount}-${dst}-t${thRound}.meeting-precision.data")
                val recout1_mt = new File(s"./eval/statistics/PROBEC/$noiseAmount/meeting_${noiseAmount}-${dst}-t${thRound}.meeting-recall.data")
                val f1out1_mt = new File(s"./eval/statistics/PROBEC/$noiseAmount/meeting_${noiseAmount}-${dst}-t${thRound}.meeting-fmeasure.data")

                if (!prout1_mt.getParentFile.exists()) prout1_mt.getParentFile.mkdirs()
                if (!prout1_mt.exists()) prout1_mt.createNewFile()
                if (!recout1_mt.exists()) recout1_mt.createNewFile()
                if (!f1out1_mt.exists()) f1out1_mt.createNewFile()

                val prout21_mt = new File(s"./eval/statistics/PIEC1/$noiseAmount/meeting_${noiseAmount}-${dst}-t${thRound}.meeting-precision.data")
                val recout21_mt = new File(s"./eval/statistics/PIEC1/$noiseAmount/meeting_${noiseAmount}-${dst}-t${thRound}.meeting-recall.data")
                val f1out21_mt = new File(s"./eval/statistics/PIEC1/$noiseAmount/meeting_${noiseAmount}-${dst}-t${thRound}.meeting-fmeasure.data")

                if (!prout21_mt.getParentFile.exists()) prout21_mt.getParentFile.mkdirs()
                if (!prout21_mt.exists()) prout21_mt.createNewFile()
                if (!recout21_mt.exists()) recout21_mt.createNewFile()
                if (!f1out21_mt.exists()) f1out21_mt.createNewFile()

                val prout22_mt = new File(s"./eval/statistics/PIEC2/$noiseAmount/meeting_${noiseAmount}-${dst}-t${thRound}.meeting-precision.data")
                val recout22_mt = new File(s"./eval/statistics/PIEC2/$noiseAmount/meeting_${noiseAmount}-${dst}-t${thRound}.meeting-recall.data")
                val f1out22_mt = new File(s"./eval/statistics/PIEC2/$noiseAmount/meeting_${noiseAmount}-${dst}-t${thRound}.meeting-fmeasure.data")

                if (!prout22_mt.getParentFile.exists()) prout22_mt.getParentFile.mkdirs()
                if (!prout22_mt.exists()) prout22_mt.createNewFile()
                if (!recout22_mt.exists()) recout22_mt.createNewFile()
                if (!f1out22_mt.exists()) f1out22_mt.createNewFile()

                val fw11_mt = new FileWriter(prout1_mt, true)
                val fw12_mt = new FileWriter(recout1_mt, true)
                val fw13_mt = new FileWriter(f1out1_mt, true)

                val fw211_mt = new FileWriter(prout21_mt, true)
                val fw212_mt = new FileWriter(recout21_mt, true)
                val fw213_mt = new FileWriter(f1out21_mt, true)

                val fw221_mt = new FileWriter(prout22_mt, true)
                val fw222_mt = new FileWriter(recout22_mt, true)
                val fw223_mt = new FileWriter(f1out22_mt, true)

                fw11_mt.write(s"$gammaValue:$runNo:$prec_mt_probec\n")
                fw12_mt.write(s"$gammaValue:$runNo:$rec_mt_probec\n")
                fw13_mt.write(s"$gammaValue:$runNo:$f1_mt_probec\n")

                fw211_mt.write(s"$gammaValue:$runNo:$prec_mt_piec1\n")
                fw212_mt.write(s"$gammaValue:$runNo:$rec_mt_piec1\n")
                fw213_mt.write(s"$gammaValue:$runNo:$f1_mt_piec1\n")

                fw221_mt.write(s"$gammaValue:$runNo:$prec_mt_piec2\n")
                fw222_mt.write(s"$gammaValue:$runNo:$rec_mt_piec2\n")
                fw223_mt.write(s"$gammaValue:$runNo:$f1_mt_piec2\n")

                fw11_mt.close()
                fw12_mt.close()
                fw13_mt.close()

                fw211_mt.close()
                fw212_mt.close()
                fw213_mt.close()

                fw221_mt.close()
                fw222_mt.close()
                fw223_mt.close()

                /****************************************************2*************************************************/

                var (wmv, xmv, ymv, zmv) = (0, fpmv, 0, 0)
                var (dmv, amv, bmv, cmv) = (0, fpmv, 0, 0)
                var (tmv, umv, pmv, smv) = (0, fpmv, 0, 0)

                val results_mv_01_r = evaluateResults(groundTruthMap("moving(id0,id1)"), probec_intervals(activitiesMap("moving(id0,id1)"), thRound))
                val results_mv_01_p0 = evaluateResults(groundTruthMap("moving(id0,id1)"), piec(activitiesMap("moving(id0,id1)"), thRound, false))
                activitiesMap("moving(id0,id1)") = activitiesMap("moving(id0,id1)").map(x => x + thRound)
                val results_mv_01_p1 = evaluateResults(groundTruthMap("moving(id0,id1)"), piec(activitiesMap("moving(id0,id1)"), thRound, true))

                wmv = wmv + results_mv_01_r._1
                xmv = xmv + results_mv_01_r._2
                ymv = ymv + results_mv_01_r._3
                zmv = zmv + results_mv_01_r._4

                dmv = dmv + results_mv_01_p0._1
                amv = amv + results_mv_01_p0._2
                bmv = bmv + results_mv_01_p0._3
                cmv = cmv + results_mv_01_p0._4

                tmv = tmv + results_mv_01_p1._1
                umv = umv + results_mv_01_p1._2
                pmv = pmv + results_mv_01_p1._3
                smv = smv + results_mv_01_p1._4

                val results_mv_02_r = evaluateResults(groundTruthMap("moving(id0,id2)"), probec_intervals(activitiesMap("moving(id0,id2)"), thRound))
                val results_mv_02_p0 = evaluateResults(groundTruthMap("moving(id0,id2)"), piec(activitiesMap("moving(id0,id2)"), thRound, false))
                activitiesMap("moving(id0,id2)") = activitiesMap("moving(id0,id2)").map(x => x + thRound)
                val results_mv_02_p1 = evaluateResults(groundTruthMap("moving(id0,id2)"), piec(activitiesMap("moving(id0,id2)"), thRound, true))

                wmv = wmv + results_mv_02_r._1
                xmv = xmv + results_mv_02_r._2
                ymv = ymv + results_mv_02_r._3
                zmv = zmv + results_mv_02_r._4

                dmv = dmv + results_mv_02_p0._1
                amv = amv + results_mv_02_p0._2
                bmv = bmv + results_mv_02_p0._3
                cmv = cmv + results_mv_02_p0._4

                tmv = tmv + results_mv_02_p1._1
                umv = umv + results_mv_02_p1._2
                pmv = pmv + results_mv_02_p1._3
                smv = smv + results_mv_02_p1._4

                val results_mv_03_r = evaluateResults(groundTruthMap("moving(id0,id3)"), probec_intervals(activitiesMap("moving(id0,id3)"), thRound))
                val results_mv_03_p0 = evaluateResults(groundTruthMap("moving(id0,id3)"), piec(activitiesMap("moving(id0,id3)"), thRound, false))
                activitiesMap("moving(id0,id3)") = activitiesMap("moving(id0,id3)").map(x => x + thRound)
                val results_mv_03_p1 = evaluateResults(groundTruthMap("moving(id0,id3)"), piec(activitiesMap("moving(id0,id3)"), thRound, true))

                wmv = wmv + results_mv_03_r._1
                xmv = xmv + results_mv_03_r._2
                ymv = ymv + results_mv_03_r._3
                zmv = zmv + results_mv_03_r._4

                dmv = dmv + results_mv_03_p0._1
                amv = amv + results_mv_03_p0._2
                bmv = bmv + results_mv_03_p0._3
                cmv = cmv + results_mv_03_p0._4

                tmv = tmv + results_mv_03_p1._1
                umv = umv + results_mv_03_p1._2
                pmv = pmv + results_mv_03_p1._3
                smv = smv + results_mv_03_p1._4

                val results_mv_12_r = evaluateResults(groundTruthMap("moving(id1,id2)"), probec_intervals(activitiesMap("moving(id1,id2)"), thRound))
                val results_mv_12_p0 = evaluateResults(groundTruthMap("moving(id1,id2)"), piec(activitiesMap("moving(id1,id2)"), thRound, false))
                activitiesMap("moving(id1,id2)") = activitiesMap("moving(id1,id2)").map(x => x + thRound)
                val results_mv_12_p1 = evaluateResults(groundTruthMap("moving(id1,id2)"), piec(activitiesMap("moving(id1,id2)"), thRound, true))

                wmv = wmv + results_mv_12_r._1
                xmv = xmv + results_mv_12_r._2
                ymv = ymv + results_mv_12_r._3
                zmv = zmv + results_mv_12_r._4

                dmv = dmv + results_mv_12_p0._1
                amv = amv + results_mv_12_p0._2
                bmv = bmv + results_mv_12_p0._3
                cmv = cmv + results_mv_12_p0._4

                tmv = tmv + results_mv_12_p1._1
                umv = umv + results_mv_12_p1._2
                pmv = pmv + results_mv_12_p1._3
                smv = smv + results_mv_12_p1._4

                val results_mv_13_r = evaluateResults(groundTruthMap("moving(id1,id3)"), probec_intervals(activitiesMap("moving(id1,id3)"), thRound))
                val results_mv_13_p0 = evaluateResults(groundTruthMap("moving(id1,id3)"), piec(activitiesMap("moving(id1,id3)"), thRound, false))
                activitiesMap("moving(id1,id3)") = activitiesMap("moving(id1,id3)").map(x => x + thRound)
                val results_mv_13_p1 = evaluateResults(groundTruthMap("moving(id1,id3)"), piec(activitiesMap("moving(id1,id3)"), thRound, true))

                wmv = wmv + results_mv_13_r._1
                xmv = xmv + results_mv_13_r._2
                ymv = ymv + results_mv_13_r._3
                zmv = zmv + results_mv_13_r._4

                dmv = dmv + results_mv_13_p0._1
                amv = amv + results_mv_13_p0._2
                bmv = bmv + results_mv_13_p0._3
                cmv = cmv + results_mv_13_p0._4

                tmv = tmv + results_mv_13_p1._1
                umv = umv + results_mv_13_p1._2
                pmv = pmv + results_mv_13_p1._3
                smv = smv + results_mv_13_p1._4

                val results_mv_23_r = evaluateResults(groundTruthMap("moving(id2,id3)"), probec_intervals(activitiesMap("moving(id2,id3)"), thRound))
                val results_mv_23_p0 = evaluateResults(groundTruthMap("moving(id2,id3)"), piec(activitiesMap("moving(id2,id3)"), thRound, false))
                activitiesMap("moving(id2,id3)") = activitiesMap("moving(id2,id3)").map(x => x + thRound)
                val results_mv_23_p1 = evaluateResults(groundTruthMap("moving(id2,id3)"), piec(activitiesMap("moving(id2,id3)"), thRound, true))

                wmv = wmv + results_mv_23_r._1
                xmv = xmv + results_mv_23_r._2
                ymv = ymv + results_mv_23_r._3
                zmv = zmv + results_mv_23_r._4

                dmv = dmv + results_mv_23_p0._1
                amv = amv + results_mv_23_p0._2
                bmv = bmv + results_mv_23_p0._3
                cmv = cmv + results_mv_23_p0._4

                tmv = tmv + results_mv_23_p1._1
                umv = umv + results_mv_23_p1._2
                pmv = pmv + results_mv_23_p1._3
                smv = smv + results_mv_23_p1._4

                val results_mv_34_r = evaluateResults(groundTruthMap("moving(id3,id4)"), probec_intervals(activitiesMap("moving(id3,id4)"), thRound))
                val results_mv_34_p0 = evaluateResults(groundTruthMap("moving(id3,id4)"), piec(activitiesMap("moving(id3,id4)"), thRound, false))
                activitiesMap("moving(id3,id4)") = activitiesMap("moving(id3,id4)").map(x => x + thRound)
                val results_mv_34_p1 = evaluateResults(groundTruthMap("moving(id3,id4)"), piec(activitiesMap("moving(id3,id4)"), thRound, true))

                wmv = wmv + results_mv_34_r._1
                xmv = xmv + results_mv_34_r._2
                ymv = ymv + results_mv_34_r._3
                zmv = zmv + results_mv_34_r._4

                dmv = dmv + results_mv_34_p0._1
                amv = amv + results_mv_34_p0._2
                bmv = bmv + results_mv_34_p0._3
                cmv = cmv + results_mv_34_p0._4

                tmv = tmv + results_mv_34_p1._1
                umv = umv + results_mv_34_p1._2
                pmv = pmv + results_mv_34_p1._3
                smv = smv + results_mv_34_p1._4

                val results_mv_45_r = evaluateResults(groundTruthMap("moving(id4,id5)"), probec_intervals(activitiesMap("moving(id4,id5)"), thRound))
                val results_mv_45_p0 = evaluateResults(groundTruthMap("moving(id4,id5)"), piec(activitiesMap("moving(id4,id5)"), thRound, false))
                activitiesMap("moving(id4,id5)") = activitiesMap("moving(id4,id5)").map(x => x + thRound)
                val results_mv_45_p1 = evaluateResults(groundTruthMap("moving(id4,id5)"), piec(activitiesMap("moving(id4,id5)"), thRound, true))

                wmv = wmv + results_mv_45_r._1
                xmv = xmv + results_mv_45_r._2
                ymv = ymv + results_mv_45_r._3
                zmv = zmv + results_mv_45_r._4

                dmv = dmv + results_mv_45_p0._1
                amv = amv + results_mv_45_p0._2
                bmv = bmv + results_mv_45_p0._3
                cmv = cmv + results_mv_45_p0._4

                tmv = tmv + results_mv_45_p1._1
                umv = umv + results_mv_45_p1._2
                pmv = pmv + results_mv_45_p1._3
                smv = smv + results_mv_45_p1._4

                val results_mv_56_r = evaluateResults(groundTruthMap("moving(id5,id6)"), probec_intervals(activitiesMap("moving(id5,id6)"), thRound))
                val results_mv_56_p0 = evaluateResults(groundTruthMap("moving(id5,id6)"), piec(activitiesMap("moving(id5,id6)"), thRound, false))
                activitiesMap("moving(id5,id6)") = activitiesMap("moving(id5,id6)").map(x => x + thRound)
                val results_mv_56_p1 = evaluateResults(groundTruthMap("moving(id5,id6)"), piec(activitiesMap("moving(id5,id6)"), thRound, true))

                wmv = wmv + results_mv_56_r._1
                xmv = xmv + results_mv_56_r._2
                ymv = ymv + results_mv_56_r._3
                zmv = zmv + results_mv_56_r._4

                dmv = dmv + results_mv_56_p0._1
                amv = amv + results_mv_56_p0._2
                bmv = bmv + results_mv_56_p0._3
                cmv = cmv + results_mv_56_p0._4

                tmv = tmv + results_mv_56_p1._1
                umv = umv + results_mv_56_p1._2
                pmv = pmv + results_mv_56_p1._3
                smv = smv + results_mv_56_p1._4

                val results_mv_10_r = evaluateResults(groundTruthMap("moving(id1,id0)"), probec_intervals(activitiesMap("moving(id1,id0)"), thRound))
                val results_mv_10_p0 = evaluateResults(groundTruthMap("moving(id1,id0)"), piec(activitiesMap("moving(id1,id0)"), thRound, false))
                activitiesMap("moving(id1,id0)") = activitiesMap("moving(id1,id0)").map(x => x + thRound)
                val results_mv_10_p1 = evaluateResults(groundTruthMap("moving(id1,id0)"), piec(activitiesMap("moving(id1,id0)"), thRound, true))

                wmv = wmv + results_mv_10_r._1
                xmv = xmv + results_mv_10_r._2
                ymv = ymv + results_mv_10_r._3
                zmv = zmv + results_mv_10_r._4

                dmv = dmv + results_mv_10_p0._1
                amv = amv + results_mv_10_p0._2
                bmv = bmv + results_mv_10_p0._3
                cmv = cmv + results_mv_10_p0._4

                tmv = tmv + results_mv_10_p1._1
                umv = umv + results_mv_10_p1._2
                pmv = pmv + results_mv_10_p1._3
                smv = smv + results_mv_10_p1._4

                val results_mv_20_r = evaluateResults(groundTruthMap("moving(id2,id0)"), probec_intervals(activitiesMap("moving(id2,id0)"), thRound))
                val results_mv_20_p0 = evaluateResults(groundTruthMap("moving(id2,id0)"), piec(activitiesMap("moving(id2,id0)"), thRound, false))
                activitiesMap("moving(id2,id0)") = activitiesMap("moving(id2,id0)").map(x => x + thRound)
                val results_mv_20_p1 = evaluateResults(groundTruthMap("moving(id2,id0)"), piec(activitiesMap("moving(id2,id0)"), thRound, true))

                wmv = wmv + results_mv_20_r._1
                xmv = xmv + results_mv_20_r._2
                ymv = ymv + results_mv_20_r._3
                zmv = zmv + results_mv_20_r._4

                dmv = dmv + results_mv_20_p0._1
                amv = amv + results_mv_20_p0._2
                bmv = bmv + results_mv_20_p0._3
                cmv = cmv + results_mv_20_p0._4

                tmv = tmv + results_mv_20_p1._1
                umv = umv + results_mv_20_p1._2
                pmv = pmv + results_mv_20_p1._3
                smv = smv + results_mv_20_p1._4

                val results_mv_30_r = evaluateResults(groundTruthMap("moving(id3,id0)"), probec_intervals(activitiesMap("moving(id3,id0)"), thRound))
                val results_mv_30_p0 = evaluateResults(groundTruthMap("moving(id3,id0)"), piec(activitiesMap("moving(id3,id0)"), thRound, false))
                activitiesMap("moving(id3,id0)") = activitiesMap("moving(id3,id0)").map(x => x + thRound)
                val results_mv_30_p1 = evaluateResults(groundTruthMap("moving(id3,id0)"), piec(activitiesMap("moving(id3,id0)"), thRound, true))

                wmv = wmv + results_mv_30_r._1
                xmv = xmv + results_mv_30_r._2
                ymv = ymv + results_mv_30_r._3
                zmv = zmv + results_mv_30_r._4

                dmv = dmv + results_mv_30_p0._1
                amv = amv + results_mv_30_p0._2
                bmv = bmv + results_mv_30_p0._3
                cmv = cmv + results_mv_30_p0._4

                tmv = tmv + results_mv_30_p1._1
                umv = umv + results_mv_30_p1._2
                pmv = pmv + results_mv_30_p1._3
                smv = smv + results_mv_30_p1._4

                val results_mv_21_r = evaluateResults(groundTruthMap("moving(id2,id1)"), probec_intervals(activitiesMap("moving(id2,id1)"), thRound))
                val results_mv_21_p0 = evaluateResults(groundTruthMap("moving(id2,id1)"), piec(activitiesMap("moving(id2,id1)"), thRound, false))
                activitiesMap("moving(id2,id1)") = activitiesMap("moving(id2,id1)").map(x => x + thRound)
                val results_mv_21_p1 = evaluateResults(groundTruthMap("moving(id2,id1)"), piec(activitiesMap("moving(id2,id1)"), thRound, true))

                wmv = wmv + results_mv_21_r._1
                xmv = xmv + results_mv_21_r._2
                ymv = ymv + results_mv_21_r._3
                zmv = zmv + results_mv_21_r._4

                dmv = dmv + results_mv_21_p0._1
                amv = amv + results_mv_21_p0._2
                bmv = bmv + results_mv_21_p0._3
                cmv = cmv + results_mv_21_p0._4

                tmv = tmv + results_mv_21_p1._1
                umv = umv + results_mv_21_p1._2
                pmv = pmv + results_mv_21_p1._3
                smv = smv + results_mv_21_p1._4

                val results_mv_31_r = evaluateResults(groundTruthMap("moving(id3,id1)"), probec_intervals(activitiesMap("moving(id3,id1)"), thRound))
                val results_mv_31_p0 = evaluateResults(groundTruthMap("moving(id3,id1)"), piec(activitiesMap("moving(id3,id1)"), thRound, false))
                activitiesMap("moving(id3,id1)") = activitiesMap("moving(id3,id1)").map(x => x + thRound)
                val results_mv_31_p1 = evaluateResults(groundTruthMap("moving(id3,id1)"), piec(activitiesMap("moving(id3,id1)"), thRound, true))

                wmv = wmv + results_mv_31_r._1
                xmv = xmv + results_mv_31_r._2
                ymv = ymv + results_mv_31_r._3
                zmv = zmv + results_mv_31_r._4

                dmv = dmv + results_mv_31_p0._1
                amv = amv + results_mv_31_p0._2
                bmv = bmv + results_mv_31_p0._3
                cmv = cmv + results_mv_31_p0._4

                tmv = tmv + results_mv_31_p1._1
                umv = umv + results_mv_31_p1._2
                pmv = pmv + results_mv_31_p1._3
                smv = smv + results_mv_31_p1._4

                val results_mv_32_r = evaluateResults(groundTruthMap("moving(id3,id2)"), probec_intervals(activitiesMap("moving(id3,id2)"), thRound))
                val results_mv_32_p0 = evaluateResults(groundTruthMap("moving(id3,id2)"), piec(activitiesMap("moving(id3,id2)"), thRound, false))
                activitiesMap("moving(id3,id2)") = activitiesMap("moving(id3,id2)").map(x => x + thRound)
                val results_mv_32_p1 = evaluateResults(groundTruthMap("moving(id3,id2)"), piec(activitiesMap("moving(id3,id2)"), thRound, true))

                wmv = wmv + results_mv_32_r._1
                xmv = xmv + results_mv_32_r._2
                ymv = ymv + results_mv_32_r._3
                zmv = zmv + results_mv_32_r._4

                dmv = dmv + results_mv_32_p0._1
                amv = amv + results_mv_32_p0._2
                bmv = bmv + results_mv_32_p0._3
                cmv = cmv + results_mv_32_p0._4

                tmv = tmv + results_mv_32_p1._1
                umv = umv + results_mv_32_p1._2
                pmv = pmv + results_mv_32_p1._3
                smv = smv + results_mv_32_p1._4

                val results_mv_43_r = evaluateResults(groundTruthMap("moving(id4,id3)"), probec_intervals(activitiesMap("moving(id4,id3)"), thRound))
                val results_mv_43_p0 = evaluateResults(groundTruthMap("moving(id4,id3)"), piec(activitiesMap("moving(id4,id3)"), thRound, false))
                activitiesMap("moving(id4,id3)") = activitiesMap("moving(id4,id3)").map(x => x + thRound)
                val results_mv_43_p1 = evaluateResults(groundTruthMap("moving(id4,id3)"), piec(activitiesMap("moving(id4,id3)"), thRound, true))

                wmv = wmv + results_mv_43_r._1
                xmv = xmv + results_mv_43_r._2
                ymv = ymv + results_mv_43_r._3
                zmv = zmv + results_mv_43_r._4

                dmv = dmv + results_mv_43_p0._1
                amv = amv + results_mv_43_p0._2
                bmv = bmv + results_mv_43_p0._3
                cmv = cmv + results_mv_43_p0._4

                tmv = tmv + results_mv_43_p1._1
                umv = umv + results_mv_43_p1._2
                pmv = pmv + results_mv_43_p1._3
                smv = smv + results_mv_43_p1._4

                val results_mv_54_r = evaluateResults(groundTruthMap("moving(id5,id4)"), probec_intervals(activitiesMap("moving(id5,id4)"), thRound))
                val results_mv_54_p0 = evaluateResults(groundTruthMap("moving(id5,id4)"), piec(activitiesMap("moving(id5,id4)"), thRound, false))
                activitiesMap("moving(id5,id4)") = activitiesMap("moving(id5,id4)").map(x => x + thRound)
                val results_mv_54_p1 = evaluateResults(groundTruthMap("moving(id5,id4)"), piec(activitiesMap("moving(id5,id4)"), thRound, true))

                wmv = wmv + results_mv_54_r._1
                xmv = xmv + results_mv_54_r._2
                ymv = ymv + results_mv_54_r._3
                zmv = zmv + results_mv_54_r._4

                dmv = dmv + results_mv_54_p0._1
                amv = amv + results_mv_54_p0._2
                bmv = bmv + results_mv_54_p0._3
                cmv = cmv + results_mv_54_p0._4

                tmv = tmv + results_mv_54_p1._1
                umv = umv + results_mv_54_p1._2
                pmv = pmv + results_mv_54_p1._3
                smv = smv + results_mv_54_p1._4

                val results_mv_65_r = evaluateResults(groundTruthMap("moving(id6,id5)"), probec_intervals(activitiesMap("moving(id6,id5)"), thRound))
                val results_mv_65_p0 = evaluateResults(groundTruthMap("moving(id6,id5)"), piec(activitiesMap("moving(id6,id5)"), thRound, false))
                activitiesMap("moving(id6,id5)") = activitiesMap("moving(id6,id5)").map(x => x + thRound)
                val results_mv_65_p1 = evaluateResults(groundTruthMap("moving(id6,id5)"), piec(activitiesMap("moving(id6,id5)"), thRound, true))

                wmv = wmv + results_mv_65_r._1
                xmv = xmv + results_mv_65_r._2
                ymv = ymv + results_mv_65_r._3
                zmv = zmv + results_mv_65_r._4

                dmv = dmv + results_mv_65_p0._1
                amv = amv + results_mv_65_p0._2
                bmv = bmv + results_mv_65_p0._3
                cmv = cmv + results_mv_65_p0._4

                tmv = tmv + results_mv_65_p1._1
                umv = umv + results_mv_65_p1._2
                pmv = pmv + results_mv_65_p1._3
                smv = smv + results_mv_65_p1._4

                val prec_mv_probec = if ((zmv + xmv) == 0) 0
                                     else BigDecimal(zmv / (zmv + xmv).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val rec_mv_probec = if ((zmv + ymv) == 0) 0
                                    else BigDecimal(zmv / (zmv + ymv).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val f1_mv_probec = if ((prec_mv_probec + rec_mv_probec) == 0.0) 0
                                   else BigDecimal((2 * prec_mv_probec * rec_mv_probec) / (prec_mv_probec + rec_mv_probec)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                println(s"precision_moving_prob-ec : $prec_mv_probec")
                println(s"recall_moving_prob-ec : $rec_mv_probec")
                println(s"f1-score_moving_prob-ec : $f1_mv_probec")

                val prec_mv_piec1 = if ((cmv + amv) == 0) 0
                                   else BigDecimal(cmv / (cmv + amv).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val rec_mv_piec1 = if ((cmv + bmv) == 0) 0
                                  else BigDecimal(cmv / (cmv + bmv).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val f1_mv_piec1 = if ((prec_mv_piec1 + rec_mv_piec1) == 0.0) 0
                                 else BigDecimal((2 * prec_mv_piec1 * rec_mv_piec1) / (prec_mv_piec1 + rec_mv_piec1)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                println(s"precision_moving_piec : $prec_mv_piec1")
                println(s"recall_moving_piec : $rec_mv_piec1")
                println(s"f1-score_moving_piec : $f1_mv_piec1")

                val prec_mv_piec2 = if ((smv + umv) == 0) 0
                                   else BigDecimal(smv / (smv + umv).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val rec_mv_piec2 = if ((smv + pmv) == 0) 0
                                  else BigDecimal(smv / (smv + pmv).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val f1_mv_piec2 = if ((prec_mv_piec2 + rec_mv_piec2) == 0.0) 0
                                 else BigDecimal((2 * prec_mv_piec2 * rec_mv_piec2) / (prec_mv_piec2 + rec_mv_piec2)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                println(s"precision_moving_piec_alt_cred : $prec_mv_piec2")
                println(s"recall_moving_piec_alt_cred : $rec_mv_piec2")
                println(s"f1-score_moving_piec_alt_cred : $f1_mv_piec2\n\n")

                val prout1_mv = new File(s"./eval/statistics/PROBEC/$noiseAmount/moving_${noiseAmount}-${dst}-t${thRound}.moving-precision.data")
                val recout1_mv = new File(s"./eval/statistics/PROBEC/$noiseAmount/moving_${noiseAmount}-${dst}-t${thRound}.moving-recall.data")
                val f1out1_mv = new File(s"./eval/statistics/PROBEC/$noiseAmount/moving_${noiseAmount}-${dst}-t${thRound}.moving-fmeasure.data")

                if (!prout1_mv.getParentFile.exists()) prout1_mv.getParentFile.mkdirs()
                if (!prout1_mv.exists()) prout1_mv.createNewFile()
                if (!recout1_mv.exists()) recout1_mv.createNewFile()
                if (!f1out1_mv.exists()) f1out1_mv.createNewFile()

                val prout21_mv = new File(s"./eval/statistics/PIEC1/$noiseAmount/moving_${noiseAmount}-${dst}-t${thRound}.moving-precision.data")
                val recout21_mv = new File(s"./eval/statistics/PIEC1/$noiseAmount/moving_${noiseAmount}-${dst}-t${thRound}.moving-recall.data")
                val f1out21_mv = new File(s"./eval/statistics/PIEC1/$noiseAmount/moving_${noiseAmount}-${dst}-t${thRound}.moving-fmeasure.data")

                if (!prout21_mv.getParentFile.exists()) prout21_mv.getParentFile.mkdirs()
                if (!prout21_mv.exists()) prout21_mv.createNewFile()
                if (!recout21_mv.exists()) recout21_mv.createNewFile()
                if (!f1out21_mv.exists()) f1out21_mv.createNewFile()

                val prout22_mv = new File(s"./eval/statistics/PIEC2/$noiseAmount/moving_${noiseAmount}-${dst}-t${thRound}.moving-precision.data")
                val recout22_mv = new File(s"./eval/statistics/PIEC2/$noiseAmount/moving_${noiseAmount}-${dst}-t${thRound}.moving-recall.data")
                val f1out22_mv = new File(s"./eval/statistics/PIEC2/$noiseAmount/moving_${noiseAmount}-${dst}-t${thRound}.moving-fmeasure.data")

                if (!prout22_mv.getParentFile.exists()) prout22_mv.getParentFile.mkdirs()
                if (!prout22_mv.exists()) prout22_mv.createNewFile()
                if (!recout22_mv.exists()) recout22_mv.createNewFile()
                if (!f1out22_mv.exists()) f1out22_mv.createNewFile()

                val fw11_mv = new FileWriter(prout1_mv, true)
                val fw12_mv = new FileWriter(recout1_mv, true)
                val fw13_mv = new FileWriter(f1out1_mv, true)

                val fw211_mv = new FileWriter(prout21_mv, true)
                val fw212_mv = new FileWriter(recout21_mv, true)
                val fw213_mv = new FileWriter(f1out21_mv, true)

                val fw221_mv = new FileWriter(prout22_mv, true)
                val fw222_mv = new FileWriter(recout22_mv, true)
                val fw223_mv = new FileWriter(f1out22_mv, true)

                fw11_mv.write(s"$gammaValue:$runNo:$prec_mv_probec\n")
                fw12_mv.write(s"$gammaValue:$runNo:$rec_mv_probec\n")
                fw13_mv.write(s"$gammaValue:$runNo:$f1_mv_probec\n")

                fw211_mv.write(s"$gammaValue:$runNo:$prec_mv_piec1\n")
                fw212_mv.write(s"$gammaValue:$runNo:$rec_mv_piec1\n")
                fw213_mv.write(s"$gammaValue:$runNo:$f1_mv_piec1\n")

                fw221_mv.write(s"$gammaValue:$runNo:$prec_mv_piec2\n")
                fw222_mv.write(s"$gammaValue:$runNo:$rec_mv_piec2\n")
                fw223_mv.write(s"$gammaValue:$runNo:$f1_mv_piec2\n")

                fw11_mv.close()
                fw12_mv.close()
                fw13_mv.close()

                fw211_mv.close()
                fw212_mv.close()
                fw213_mv.close()

                fw221_mv.close()
                fw222_mv.close()
                fw223_mv.close()
            }

            /****************************************************3*************************************************/

            if (dst == "enh")
            {
                var (wf, xf, yf, zf) = (0, fpf, 0, 0)
                var (df, af, bf, cf) = (0, fpf, 0, 0)
                var (tf, uf, pf, sf) = (0, fpf, 0, 0)

                val results_f_12_r = evaluateResults(groundTruthMap("fighting(id1,id2)"), probec_intervals(activitiesMap("fighting(id1,id2)"), thRound))
                val results_f_12_p0 = evaluateResults(groundTruthMap("fighting(id1,id2)"), piec(activitiesMap("fighting(id1,id2)"), thRound, false))
                activitiesMap("fighting(id1,id2)") = activitiesMap("fighting(id1,id2)").map(x => x + thRound)
                val results_f_12_p1 = evaluateResults(groundTruthMap("fighting(id1,id2)"), piec(activitiesMap("fighting(id1,id2)"), thRound, true))

                wf = wf + results_f_12_r._1
                xf = xf + results_f_12_r._2
                yf = yf + results_f_12_r._3
                zf = zf + results_f_12_r._4

                df = df + results_f_12_p0._1
                af = af + results_f_12_p0._2
                bf = bf + results_f_12_p0._3
                cf = cf + results_f_12_p0._4

                tf = tf + results_f_12_p1._1
                uf = uf + results_f_12_p1._2
                pf = pf + results_f_12_p1._3
                sf = sf + results_f_12_p1._4

                val results_f_21_r = evaluateResults(groundTruthMap("fighting(id2,id1)"), probec_intervals(activitiesMap("fighting(id2,id1)"), thRound))
                val results_f_21_p0 = evaluateResults(groundTruthMap("fighting(id2,id1)"), piec(activitiesMap("fighting(id2,id1)"), thRound, false))
                activitiesMap("fighting(id2,id1)") = activitiesMap("fighting(id2,id1)").map(x => x + thRound)
                val results_f_21_p1 = evaluateResults(groundTruthMap("fighting(id2,id1)"), piec(activitiesMap("fighting(id2,id1)"), thRound, true))

                wf = wf + results_f_21_r._1
                xf = xf + results_f_21_r._2
                yf = yf + results_f_21_r._3
                zf = zf + results_f_21_r._4

                df = df + results_f_21_p0._1
                af = af + results_f_21_p0._2
                bf = bf + results_f_21_p0._3
                cf = cf + results_f_21_p0._4

                tf = tf + results_f_21_p1._1
                uf = uf + results_f_21_p1._2
                pf = pf + results_f_21_p1._3
                sf = sf + results_f_21_p1._4

                val results_f_26_r = evaluateResults(groundTruthMap("fighting(id2,id6)"), probec_intervals(activitiesMap("fighting(id2,id6)"), thRound))
                val results_f_26_p0 = evaluateResults(groundTruthMap("fighting(id2,id6)"), piec(activitiesMap("fighting(id2,id6)"), thRound, false))
                activitiesMap("fighting(id2,id6)") = activitiesMap("fighting(id2,id6)").map(x => x + thRound)
                val results_f_26_p1 = evaluateResults(groundTruthMap("fighting(id2,id6)"), piec(activitiesMap("fighting(id2,id6)"), thRound, true))

                wf = wf + results_f_26_r._1
                xf = xf + results_f_26_r._2
                yf = yf + results_f_26_r._3
                zf = zf + results_f_26_r._4

                df = df + results_f_26_p0._1
                af = af + results_f_26_p0._2
                bf = bf + results_f_26_p0._3
                cf = cf + results_f_26_p0._4

                tf = tf + results_f_26_p1._1
                uf = uf + results_f_26_p1._2
                pf = pf + results_f_26_p1._3
                sf = sf + results_f_26_p1._4

                val results_f_62_r = evaluateResults(groundTruthMap("fighting(id6,id2)"), probec_intervals(activitiesMap("fighting(id6,id2)"), thRound))
                val results_f_62_p0 = evaluateResults(groundTruthMap("fighting(id6,id2)"), piec(activitiesMap("fighting(id6,id2)"), thRound, false))
                activitiesMap("fighting(id6,id2)") = activitiesMap("fighting(id6,id2)").map(x => x + thRound)
                val results_f_62_p1 = evaluateResults(groundTruthMap("fighting(id6,id2)"), piec(activitiesMap("fighting(id6,id2)"), thRound, true))

                wf = wf + results_f_62_r._1
                xf = xf + results_f_62_r._2
                yf = yf + results_f_62_r._3
                zf = zf + results_f_62_r._4

                df = df + results_f_62_p0._1
                af = af + results_f_62_p0._2
                bf = bf + results_f_62_p0._3
                cf = cf + results_f_62_p0._4

                tf = tf + results_f_62_p1._1
                uf = uf + results_f_62_p1._2
                pf = pf + results_f_62_p1._3
                sf = sf + results_f_62_p1._4

                val results_f_34_r = evaluateResults(groundTruthMap("fighting(id3,id4)"), probec_intervals(activitiesMap("fighting(id3,id4)"), thRound))
                val results_f_34_p0 = evaluateResults(groundTruthMap("fighting(id3,id4)"), piec(activitiesMap("fighting(id3,id4)"), thRound, false))
                activitiesMap("fighting(id3,id4)") = activitiesMap("fighting(id3,id4)").map(x => x + thRound)
                val results_f_34_p1 = evaluateResults(groundTruthMap("fighting(id3,id4)"), piec(activitiesMap("fighting(id3,id4)"), thRound, true))

                wf = wf + results_f_34_r._1
                xf = xf + results_f_34_r._2
                yf = yf + results_f_34_r._3
                zf = zf + results_f_34_r._4

                df = df + results_f_34_p0._1
                af = af + results_f_34_p0._2
                bf = bf + results_f_34_p0._3
                cf = cf + results_f_34_p0._4

                tf = tf + results_f_34_p1._1
                uf = uf + results_f_34_p1._2
                pf = pf + results_f_34_p1._3
                sf = sf + results_f_34_p1._4

                val results_f_43_r = evaluateResults(groundTruthMap("fighting(id4,id3)"), probec_intervals(activitiesMap("fighting(id4,id3)"), thRound))
                val results_f_43_p0 = evaluateResults(groundTruthMap("fighting(id4,id3)"), piec(activitiesMap("fighting(id4,id3)"), thRound, false))
                activitiesMap("fighting(id4,id3)") = activitiesMap("fighting(id4,id3)").map(x => x + thRound)
                val results_f_43_p1 = evaluateResults(groundTruthMap("fighting(id4,id3)"), piec(activitiesMap("fighting(id4,id3)"), thRound, true))

                wf = wf + results_f_43_r._1
                xf = xf + results_f_43_r._2
                yf = yf + results_f_43_r._3
                zf = zf + results_f_43_r._4

                df = df + results_f_43_p0._1
                af = af + results_f_43_p0._2
                bf = bf + results_f_43_p0._3
                cf = cf + results_f_43_p0._4

                tf = tf + results_f_43_p1._1
                uf = uf + results_f_43_p1._2
                pf = pf + results_f_43_p1._3
                sf = sf + results_f_43_p1._4

                val results_f_45_r = evaluateResults(groundTruthMap("fighting(id4,id5)"), probec_intervals(activitiesMap("fighting(id4,id5)"), thRound))
                val results_f_45_p0 = evaluateResults(groundTruthMap("fighting(id4,id5)"), piec(activitiesMap("fighting(id4,id5)"), thRound, false))
                activitiesMap("fighting(id4,id5)") = activitiesMap("fighting(id4,id5)").map(x => x + thRound)
                val results_f_45_p1 = evaluateResults(groundTruthMap("fighting(id4,id5)"), piec(activitiesMap("fighting(id4,id5)"), thRound, true))

                wf = wf + results_f_45_r._1
                xf = xf + results_f_45_r._2
                yf = yf + results_f_45_r._3
                zf = zf + results_f_45_r._4

                df = df + results_f_45_p0._1
                af = af + results_f_45_p0._2
                bf = bf + results_f_45_p0._3
                cf = cf + results_f_45_p0._4

                tf = tf + results_f_45_p1._1
                uf = uf + results_f_45_p1._2
                pf = pf + results_f_45_p1._3
                sf = sf + results_f_45_p1._4

                val results_f_54_r = evaluateResults(groundTruthMap("fighting(id5,id4)"), probec_intervals(activitiesMap("fighting(id5,id4)"), thRound))
                val results_f_54_p0 = evaluateResults(groundTruthMap("fighting(id5,id4)"), piec(activitiesMap("fighting(id5,id4)"), thRound, false))
                activitiesMap("fighting(id5,id4)") = activitiesMap("fighting(id5,id4)").map(x => x + thRound)
                val results_f_54_p1 = evaluateResults(groundTruthMap("fighting(id5,id4)"), piec(activitiesMap("fighting(id5,id4)"), thRound, true))

                wf = wf + results_f_54_r._1
                xf = xf + results_f_54_r._2
                yf = yf + results_f_54_r._3
                zf = zf + results_f_54_r._4

                df = df + results_f_54_p0._1
                af = af + results_f_54_p0._2
                bf = bf + results_f_54_p0._3
                cf = cf + results_f_54_p0._4

                tf = tf + results_f_54_p1._1
                uf = uf + results_f_54_p1._2
                pf = pf + results_f_54_p1._3
                sf = sf + results_f_54_p1._4

                val results_f_67_r = evaluateResults(groundTruthMap("fighting(id6,id7)"), probec_intervals(activitiesMap("fighting(id6,id7)"), thRound))
                val results_f_67_p0 = evaluateResults(groundTruthMap("fighting(id6,id7)"), piec(activitiesMap("fighting(id6,id7)"), thRound, false))
                activitiesMap("fighting(id6,id7)") = activitiesMap("fighting(id6,id7)").map(x => x + thRound)
                val results_f_67_p1 = evaluateResults(groundTruthMap("fighting(id6,id7)"), piec(activitiesMap("fighting(id6,id7)"), thRound, true))

                wf = wf + results_f_67_r._1
                xf = xf + results_f_67_r._2
                yf = yf + results_f_67_r._3
                zf = zf + results_f_67_r._4

                df = df + results_f_67_p0._1
                af = af + results_f_67_p0._2
                bf = bf + results_f_67_p0._3
                cf = cf + results_f_67_p0._4

                tf = tf + results_f_67_p1._1
                uf = uf + results_f_67_p1._2
                pf = pf + results_f_67_p1._3
                sf = sf + results_f_67_p1._4

                val results_f_76_r = evaluateResults(groundTruthMap("fighting(id7,id6)"), probec_intervals(activitiesMap("fighting(id7,id6)"), thRound))
                val results_f_76_p0 = evaluateResults(groundTruthMap("fighting(id7,id6)"), piec(activitiesMap("fighting(id7,id6)"), thRound, false))
                activitiesMap("fighting(id7,id6)") = activitiesMap("fighting(id7,id6)").map(x => x + thRound)
                val results_f_76_p1 = evaluateResults(groundTruthMap("fighting(id7,id6)"), piec(activitiesMap("fighting(id7,id6)"), thRound, true))

                wf = wf + results_f_76_r._1
                xf = xf + results_f_76_r._2
                yf = yf + results_f_76_r._3
                zf = zf + results_f_76_r._4

                df = df + results_f_76_p0._1
                af = af + results_f_76_p0._2
                bf = bf + results_f_76_p0._3
                cf = cf + results_f_76_p0._4

                tf = tf + results_f_76_p1._1
                uf = uf + results_f_76_p1._2
                pf = pf + results_f_76_p1._3
                sf = sf + results_f_76_p1._4

                val prec_f_probec = if ((zf + xf) == 0) 0
                                    else BigDecimal(zf / (zf + xf).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val rec_f_probec = if ((zf + yf) == 0) 0
                                   else BigDecimal(zf / (zf + yf).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val f1_f_probec = if ((prec_f_probec + rec_f_probec) == 0.0) 0
                                  else BigDecimal((2 * prec_f_probec * rec_f_probec) / (prec_f_probec + rec_f_probec)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                println(s"precision_fighting_prob-ec : $prec_f_probec")
                println(s"recall_fighting_prob-ec : $rec_f_probec")
                println(s"f1-score_fighting_prob-ec : $f1_f_probec")

                val prec_f_piec1 = if ((cf + af) == 0) 0
                                  else BigDecimal(cf / (cf + af).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val rec_f_piec1 = if ((cf + bf) == 0) 0
                                 else BigDecimal(cf / (cf + bf).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val f1_f_piec1 = if ((prec_f_piec1 + rec_f_piec1) == 0.0) 0
                                else BigDecimal((2 * prec_f_piec1 * rec_f_piec1) / (prec_f_piec1 + rec_f_piec1)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                println(s"precision_fighting_piec : $prec_f_piec1")
                println(s"recall_fighting_piec : $rec_f_piec1")
                println(s"f1-score_fighting_piec : $f1_f_piec1")

                val prec_f_piec2 = if ((sf + uf) == 0) 0
                                  else BigDecimal(sf / (sf + uf).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val rec_f_piec2 = if ((sf + pf) == 0) 0
                                 else BigDecimal(sf / (sf + pf).toDouble).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
                val f1_f_piec2 = if ((prec_f_piec2 + rec_f_piec2) == 0.0) 0
                                else BigDecimal((2 * prec_f_piec2 * rec_f_piec2) / (prec_f_piec2 + rec_f_piec2)).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

                println(s"precision_fighting_piec_alt_cred : $prec_f_piec2")
                println(s"recall_fighting_piec_alt_cred : $rec_f_piec2")
                println(s"f1-score_fighting_piec_alt_cred : $f1_f_piec2\n\n")

                val prout1_f = new File(s"./eval/statistics/PROBEC/$noiseAmount/fighting_${noiseAmount}-${dst}-t${thRound}.fighting-precision.data")
                val recout1_f = new File(s"./eval/statistics/PROBEC/$noiseAmount/fighting_${noiseAmount}-${dst}-t${thRound}.fighting-recall.data")
                val f1out1_f = new File(s"./eval/statistics/PROBEC/$noiseAmount/fighting_${noiseAmount}-${dst}-t${thRound}.fighting-fmeasure.data")

                if (!prout1_f.getParentFile.exists()) prout1_f.getParentFile.mkdirs()
                if (!prout1_f.exists()) prout1_f.createNewFile()
                if (!recout1_f.exists()) recout1_f.createNewFile()
                if (!f1out1_f.exists()) f1out1_f.createNewFile()

                val prout21_f = new File(s"./eval/statistics/PIEC1/$noiseAmount/fighting_${noiseAmount}-${dst}-t${thRound}.fighting-precision.data")
                val recout21_f = new File(s"./eval/statistics/PIEC1/$noiseAmount/fighting_${noiseAmount}-${dst}-t${thRound}.fighting-recall.data")
                val f1out21_f = new File(s"./eval/statistics/PIEC1/$noiseAmount/fighting_${noiseAmount}-${dst}-t${thRound}.fighting-fmeasure.data")

                if (!prout21_f.getParentFile.exists()) prout21_f.getParentFile.mkdirs()
                if (!prout21_f.exists()) prout21_f.createNewFile()
                if (!recout21_f.exists()) recout21_f.createNewFile()
                if (!f1out21_f.exists()) f1out21_f.createNewFile()

                val prout22_f = new File(s"./eval/statistics/PIEC2/$noiseAmount/fighting_${noiseAmount}-${dst}-t${thRound}.fighting-precision.data")
                val recout22_f = new File(s"./eval/statistics/PIEC2/$noiseAmount/fighting_${noiseAmount}-${dst}-t${thRound}.fighting-recall.data")
                val f1out22_f = new File(s"./eval/statistics/PIEC2/$noiseAmount/fighting_${noiseAmount}-${dst}-t${thRound}.fighting-fmeasure.data")

                if (!prout22_f.getParentFile.exists()) prout22_f.getParentFile.mkdirs()
                if (!prout22_f.exists()) prout22_f.createNewFile()
                if (!recout22_f.exists()) recout22_f.createNewFile()
                if (!f1out22_f.exists()) f1out22_f.createNewFile()

                val fw11_f = new FileWriter(prout1_f, true)
                val fw12_f = new FileWriter(recout1_f, true)
                val fw13_f = new FileWriter(f1out1_f, true)

                val fw211_f = new FileWriter(prout21_f, true)
                val fw212_f = new FileWriter(recout21_f, true)
                val fw213_f = new FileWriter(f1out21_f, true)

                val fw221_f = new FileWriter(prout22_f, true)
                val fw222_f = new FileWriter(recout22_f, true)
                val fw223_f = new FileWriter(f1out22_f, true)

                fw11_f.write(s"$gammaValue:$runNo:$prec_f_probec\n")
                fw12_f.write(s"$gammaValue:$runNo:$rec_f_probec\n")
                fw13_f.write(s"$gammaValue:$runNo:$f1_f_probec\n")

                fw211_f.write(s"$gammaValue:$runNo:$prec_f_piec1\n")
                fw212_f.write(s"$gammaValue:$runNo:$rec_f_piec1\n")
                fw213_f.write(s"$gammaValue:$runNo:$f1_f_piec1\n")

                fw221_f.write(s"$gammaValue:$runNo:$prec_f_piec2\n")
                fw222_f.write(s"$gammaValue:$runNo:$rec_f_piec2\n")
                fw223_f.write(s"$gammaValue:$runNo:$f1_f_piec2\n")

                fw11_f.close()
                fw12_f.close()
                fw13_f.close()

                fw211_f.close()
                fw212_f.close()
                fw213_f.close()

                fw221_f.close()
                fw222_f.close()
                fw223_f.close()
            }
        }

        println(".-\n\n")
    }
}
