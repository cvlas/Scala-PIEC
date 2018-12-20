package piec

import java.io.{File, FileWriter}

import scala.io.Source

object StatsGatherer extends App
{
    val dataSetType = Seq[String]("enh", "orig")
    val gammas = 0.0 to 8.0 by 0.5
    //val noiseAmount = Seq[String]("smooth", "intermediate", "strong")
    val noiseAmount = Seq[String]("smooth", "intermediate")
    //val lta = Seq[(String, String)](("meeting", "meet"), ("moving", "move"), ("leaving_object", "leaving_object"), ("fighting", "fight"))
    val lta = Seq[(String, String)](("meeting", "meet"), ("moving", "move"), ("fighting", "fight"))
    val measures = Seq[String]("precision", "recall", "fmeasure")
    //val noiseType = Seq[String]("clean", "noisy")
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
                        var barb1s = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PROBEC/none/${hle_long}_none-${dt}-t${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}.${hle_long}-${ms}.data")
                        var barb2s = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PROBEC/${na}/${hle_long}_${na}-${dt}-t${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}.${hle_long}-${ms}.data")
                        var bab1s = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PIEC/none/${hle_long}_none-${dt}-t${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}.${hle_long}-${ms}.data")
                        var bab2s = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics/PIEC/${na}/${hle_long}_${na}-${dt}-t${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}.${hle_long}-${ms}.data")

                        if (bab2s.exists() && barb2s.exists())
                        {
                            var labis = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics_new/PIEC/${na.capitalize}/${hle_short}_${na}-${dt}-t${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}.${hle_short}-${ms}.data")
                            if (!labis.getParentFile.exists()) labis.getParentFile.mkdirs()
                            if (!labis.exists()) labis.createNewFile()

                            val fw1 = new FileWriter(labis,true)

                            fw1.write(s"# grp\tPIEC-Min_${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}\tPIEC-Max_${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}\tPIEC-Avg_${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}\n")

                            var larbis = new File(s"/home/cgvlas/Demokritos/TPLP-toy/statistics_new/PROBEC/${na.capitalize}/${hle_short}_${na}-${dt}-t${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}.${hle_short}-${ms}.data")
                            if (!larbis.getParentFile.exists()) larbis.getParentFile.mkdirs()
                            if (!larbis.exists()) larbis.createNewFile()

                            val fwr1 = new FileWriter(larbis,true)

                            fwr1.write(s"# grp\tPROBEC-Min_${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}\tPROBEC-Max_${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}\tPROBEC-Avg_${BigDecimal(th).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}\n")

                            for (ga <- gammas)
                            {
                                if (ga == 0.0)
                                {
                                    val valueStr = Source.fromFile(bab1s).getLines().toList.head.split(":")(2)

                                    if (valueStr != "N/A")
                                    {
                                        val value = valueStr.toDouble
                                        val (min, max, avg) = (value, value, value)

                                        fw1.write(f"${ga}\t${min}%1.7f\t${max}%1.7f\t${avg}%1.7f\n")
                                    }

                                    val valueStrr = Source.fromFile(barb1s).getLines().toList.head.split(":")(2)

                                    if (valueStrr != "N/A")
                                    {
                                        val value = valueStrr.toDouble
                                        val (min, max, avg) = (value, value, value)

                                        fwr1.write(f"${ga}\t${min}%1.7f\t${max}%1.7f\t${avg}%1.7f\n")
                                    }
                                    //println("JUST WROTE!!!")
                                }
                                else
                                {
                                    var matchingLines = Source.fromFile(bab2s).getLines().filter(_.startsWith(s"${ga}"))

                                    if (matchingLines.isEmpty)
                                    {
                                        val (min, max, avg) = (0.0, 0.0, 0.0)

                                        fw1.write(f"${ga}\t${min}%1.7f\t${max}%1.7f\t${avg}%1.7f\n")
                                        //println("JUST WROTE!!!")
                                    }
                                    else
                                    {
                                        var (min, max, avg, sum, ctr) = (1.0, 0.0, 0.0, 0.0, 0)

                                        for (line <- matchingLines)
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

                                        fw1.write(f"${ga}\t${min}%1.7f\t${max}%1.7f\t${avg}%1.7f\n")
                                        //println("JUST WROTE!!!")
                                    }

                                    var matchingLinesr = Source.fromFile(barb2s).getLines().filter(_.startsWith(s"${ga}"))

                                    if (matchingLinesr.isEmpty)
                                    {
                                        val (min, max, avg) = (0.0, 0.0, 0.0)

                                        fwr1.write(f"${ga}\t${min}%1.7f\t${max}%1.7f\t${avg}%1.7f\n")
                                        //println("JUST WROTE!!!")
                                    }
                                    else
                                    {
                                        var (min, max, avg, sum, ctr) = (1.0, 0.0, 0.0, 0.0, 0)

                                        for (line <- matchingLinesr)
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

                                        fwr1.write(f"${ga}\t${min}%1.7f\t${max}%1.7f\t${avg}%1.7f\n")
                                        //println("JUST WROTE!!!")
                                    }
                                }
                            }

                            fw1.close()
                            fwr1.close()
                        }
                    }
                }
            }
        }
    }
}
