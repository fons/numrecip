package com.github.fons.nr.test

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/23/13
 * Time: 5:35 PM
 * To change this template use File | Settings | File Templates.
 */

import scala.util.{Try, Success, Failure}


object OdeTestResult {
  def show(v: Try[OdeTestResult]) {
    v match {
      case Success(result) => show(result)
      case Failure(f) => show(f)
    }
  }

  def show(v: OdeTestResult) {
    println(v.Desc)
    OdeTestRunDiff.show(v.Diffs)
    println("average difference   : ", v.average, "  relative difference         : ", v.relative)
    val abs_avg = v.average(math.abs)
    val abs_rel = v.relative(math.abs)
    println("average of abs diffs : ", abs_avg, "  average of abs relative diffs :", abs_rel)
  }

  def show(f: Throwable) {
    println("test failed with error :" + f.toString)
  }
}

case class OdeTestResult(val Desc: String, val Diffs: List[OdeTestRunDiff]) {
  def average(): Double = average((x: Double) => x)

  def relative(): Double = relative((x: Double) => x)

  //exclude the dependend variable which should be small
  def average(f: (Double) => Double): Double = (Diffs.foldLeft(0.0)((a: Double, t: OdeTestRunDiff) => a + f(t.diff))) / (1.0 * (Diffs.length - 1.0))

  def relative(f: (Double) => Double): Double = (Diffs.foldLeft(0.0)((a: Double, t: OdeTestRunDiff) => a + f(2.0 * t.diff / (t.left + t.right)))) / (1.0 * (Diffs.length - 1.0))

  override def toString = {
    Desc + "\n" //+ Diffs.reduce(_ + "\n" + _)
  }
}
