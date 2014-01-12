package com.github.fons.nr.test

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/10/14
 * Time: 9:10 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.util.{Either, Right, Left}
import com.github.fons.nr.matrix.{Matrix, Solver}

case class MatrixTestResult(val solver: Solver.Value, desc: String, val runtime: Double, result: Either[Double, (Matrix, Matrix, Matrix)]) {

  def failed = {
    result match {
      case Left(d) => false
      case _ => true
    }
  }

  def success = !failed

}
