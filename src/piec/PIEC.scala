package piec

/**
  * @author Christos G. Vlassopoulos (cvlas@iit.demokritos.gr)
  *
  * 2018-07-27
  */

object PIEC extends App {

  def piec(a: Array[Double], t: Double): Unit = {

    val l = new Array[Double](a.size)
    for (i <- 0 until a.size) l(i) = a(i) - t
    for (i <- 0 until a.size) println(l(i))
  }

  val aa = Array(0.8, 0.6, 0.1, 0.0, 0.9, 0.6, 0.8, 0.5, 0.2, 0.1, 0.6)
  piec(aa, 0.5)
}
