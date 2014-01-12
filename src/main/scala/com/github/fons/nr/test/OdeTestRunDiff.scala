package com.github.fons.nr.test

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/23/13
 * Time: 10:55 AM
 * To change this template use File | Settings | File Templates.
 */
object OdeTestRunDiff {
  private def header = {
    val h1xy = "description"
    val h2xy = "left"
    val h3xy = "right"
    val h4xy = "diff"
    f"$h1xy%-15s   $h2xy%-20s   $h3xy%-20s $h4xy%-20s"
  }

  def show(results: List[OdeTestRunDiff]) {
    println(header)
    for (d <- results) {
      println(d.pp)
    }
  }
}

case class OdeTestRunDiff(val desc: String, val left: Double, val right: Double) {
  val diff = left - right

  def pp = {
    val s2 = f"$desc%-15s   $left%-+8.12e   $right%-+8.12e   $diff%-+8.12e  "
    //s1 + "\n" + s2
    s2
  }

  lazy override val toString = "com.mhsw.com.github.fons.nr.test.TestResult(" + desc + ", " + left + ", " + right + ", " + diff + ")"
}
