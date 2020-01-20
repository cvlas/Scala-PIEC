package piec

import java.io.{File, FileWriter}

import scala.io.Source

/**
  * The following code performs the comparison between PIEC and Prob-EC.
  *
  * @author Christos G. Vlassopoulos (cvlas@{iit.demokritos.gr, di.uoa.gr})
  *
  *         2019-01-07
  */
object StatsGatherer extends App
{
    val dataSetType = Seq[String]("enh", "orig")
    val gammas = 0.0 to 8.0 by 0.5
    val noiseAmount = Seq[String]("smooth", "intermediate")
    val lta = Seq[(String, String)](("meeting", "meet"), ("moving", "move"), ("fighting", "fight"))
    val measures = Seq[String]("precision", "recall", "fmeasure")
    val runs = 1 to 5
    val thresholds = 0.5 to 0.91 by 0.2

    for (na <- noiseAmount)
    {
        for (hle_t <- lta)
        {
            val hle_long = hle_t._1
            val hle_short = hle_t._2

            for (dt <- dataSetType)
            {
                for (th <- thresholds)
                {
                    for (ms <- measures)
                    {
                        /**
                          * A group of files to read from
                          */
                        val spr1 = new File(s"./eval/statistics/PROBEC/none/${hle_long}_none-$dt-t${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}.$hle_long-$ms.data")
                        val spr2 = new File(s"./eval/statistics/PROBEC/$na/${hle_long}_$na-$dt-t${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}.$hle_long-$ms.data")
                        val sp11 = new File(s"./eval/statistics/PIEC1/none/${hle_long}_none-$dt-t${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}.$hle_long-$ms.data")
                        val sp12 = new File(s"./eval/statistics/PIEC1/$na/${hle_long}_$na-$dt-t${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}.$hle_long-$ms.data")
                        val sp21 = new File(s"./eval/statistics/PIEC2/none/${hle_long}_none-$dt-t${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}.$hle_long-$ms.data")
                        val sp22 = new File(s"./eval/statistics/PIEC2/$na/${hle_long}_$na-$dt-t${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}.$hle_long-$ms.data")

                        if (sp12.exists() && sp22.exists() && spr2.exists())
                        {
                            val snp1 = new File(s"./eval/statistics_new/PIEC1/${na.capitalize}/${hle_short}_$na-$dt-t${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}.$hle_short-$ms.data")
                            if (!snp1.getParentFile.exists()) snp1.getParentFile.mkdirs()
                            if (!snp1.exists()) snp1.createNewFile()

                            val fwp1 = new FileWriter(snp1,true)

                            fwp1.write(s"# grp\tPIEC1-Min_${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}\tPIEC1-Max_${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}\tPIEC1-Avg_${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}\n")

                            val snp2 = new File(s"./eval/statistics_new/PIEC2/${na.capitalize}/${hle_short}_$na-$dt-t${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}.$hle_short-$ms.data")
                            if (!snp2.getParentFile.exists()) snp2.getParentFile.mkdirs()
                            if (!snp2.exists()) snp2.createNewFile()

                            val fwp2 = new FileWriter(snp2,true)

                            fwp2.write(s"# grp\tPIEC2-Min_${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}\tPIEC2-Max_${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}\tPIEC2-Avg_${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}\n")

                            val snpr = new File(s"./eval/statistics_new/PROBEC/${na.capitalize}/${hle_short}_$na-$dt-t${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}.$hle_short-$ms.data")
                            if (!snpr.getParentFile.exists()) snpr.getParentFile.mkdirs()
                            if (!snpr.exists()) snpr.createNewFile()

                            val fwpr = new FileWriter(snpr,true)

                            fwpr.write(s"# grp\tPROBEC-Min_${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}\tPROBEC-Max_${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}\tPROBEC-Avg_${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}\n")

                            for (ga <- gammas)
                            {
                                if (ga == 0.0)
                                {
                                    val valueStrP1 = Source.fromFile(sp11).getLines().toList.head.split(":")(2)

                                    if (valueStrP1 != "N/A")
                                    {
                                        val value = valueStrP1.toDouble
                                        val (min, max, avg) = (value, value, value)

                                        fwp1.write(f"$ga\t$min%1.7f\t$max%1.7f\t$avg%1.7f\n")
                                    }

                                    val valueStrP2 = Source.fromFile(sp21).getLines().toList.head.split(":")(2)

                                    if (valueStrP2 != "N/A")
                                    {
                                        val value = valueStrP2.toDouble
                                        val (min, max, avg) = (value, value, value)

                                        fwp2.write(f"$ga\t$min%1.7f\t$max%1.7f\t$avg%1.7f\n")
                                    }

                                    val valueStrPr = Source.fromFile(spr1).getLines().toList.head.split(":")(2)

                                    if (valueStrPr != "N/A")
                                    {
                                        val value = valueStrPr.toDouble
                                        val (min, max, avg) = (value, value, value)

                                        fwpr.write(f"$ga\t$min%1.7f\t$max%1.7f\t$avg%1.7f\n")
                                    }
                                }
                                else
                                {
                                    val matchingLinesP1 = Source.fromFile(sp12).getLines().filter(_.startsWith(s"$ga"))

                                    if (matchingLinesP1.isEmpty)
                                    {
                                        val (min, max, avg) = (0.0, 0.0, 0.0)

                                        fwp1.write(f"$ga\t$min%1.7f\t$max%1.7f\t$avg%1.7f\n")
                                    }
                                    else
                                    {
                                        var (min, max, avg, sum, ctr) = (1.0, 0.0, 0.0, 0.0, 0)

                                        for (line <- matchingLinesP1)
                                        {
                                            val lastPart = line.split(":")(2)
                                            if (lastPart != "N/A")
                                            {
                                                val prob = line.split(":")(2).toDouble
                                                ctr = ctr + 1
                                                sum = sum + prob
                                                avg = sum/ctr
                                                if (prob < min) min = prob
                                                if (prob > max) max = prob
                                            }
                                        }

                                        fwp1.write(f"$ga\t$min%1.7f\t$max%1.7f\t$avg%1.7f\n")
                                    }

                                    val matchingLinesP2 = Source.fromFile(sp22).getLines().filter(_.startsWith(s"$ga"))

                                    if (matchingLinesP2.isEmpty)
                                    {
                                        val (min, max, avg) = (0.0, 0.0, 0.0)

                                        fwp2.write(f"$ga\t$min%1.7f\t$max%1.7f\t$avg%1.7f\n")
                                    }
                                    else
                                    {
                                        var (min, max, avg, sum, ctr) = (1.0, 0.0, 0.0, 0.0, 0)

                                        for (line <- matchingLinesP2)
                                        {
                                            val lastPart = line.split(":")(2)
                                            if (lastPart != "N/A")
                                            {
                                                val prob = line.split(":")(2).toDouble
                                                ctr = ctr + 1
                                                sum = sum + prob
                                                avg = sum/ctr
                                                if (prob < min) min = prob
                                                if (prob > max) max = prob
                                            }
                                        }

                                        fwp2.write(f"$ga\t$min%1.7f\t$max%1.7f\t$avg%1.7f\n")
                                    }

                                    val matchingLinesPr = Source.fromFile(spr2).getLines().filter(_.startsWith(s"$ga"))

                                    if (matchingLinesPr.isEmpty)
                                    {
                                        val (min, max, avg) = (0.0, 0.0, 0.0)

                                        fwpr.write(f"$ga\t$min%1.7f\t$max%1.7f\t$avg%1.7f\n")
                                    }
                                    else
                                    {
                                        var (min, max, avg, sum, ctr) = (1.0, 0.0, 0.0, 0.0, 0)

                                        for (line <- matchingLinesPr)
                                        {
                                            val lastPart = line.split(":")(2)
                                            if (lastPart != "N/A")
                                            {
                                                val prob = line.split(":")(2).toDouble
                                                ctr = ctr + 1
                                                sum = sum + prob
                                                avg = sum/ctr
                                                if (prob < min) min = prob
                                                if (prob > max) max = prob
                                            }
                                        }

                                        fwpr.write(f"$ga\t$min%1.7f\t$max%1.7f\t$avg%1.7f\n")
                                    }
                                }
                            }

                            fwp1.close()
                            fwp2.close()
                            fwpr.close()
                        }
                    }
                }
            }
        }
    }
}
