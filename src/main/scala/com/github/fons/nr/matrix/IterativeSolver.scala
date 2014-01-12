package com.github.fons.nr.matrix

import scala.annotation.tailrec
import com.github.fons.nr.util.Accuracy

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/30/13
 * Time: 1:08 PM
 * To change this template use File | Settings | File Templates.
 */
abstract class IterativeSolver(acc: Accuracy = Accuracy(), max: Int = 200, probe: Int = 10, converge: Int = 10) extends LinearSystemsSolverT with CompletePivot {
  private def diff(e1: Matrix, e2: Matrix) = {
    val v = (e1 - e2).values((r: Int, c: Int) => true).foldLeft(0.0)((agg: Double, y: Double) => agg + scala.math.abs(y))
    v / e1.rows
  }

  private def converges(diffs: List[Double]) = {
    if (diffs.length < converge) true
    else {
      diffs.zip(diffs.tail).foldLeft(true)((agg: Boolean, ell: (Double, Double)) => agg && (ell._1 <= ell._2))
    }
  }

  protected def nextEstimate(est: Matrix, b: Matrix, d: Matrix): Option[Matrix]

  @tailrec private def iterate(max: Int, diffs: List[Double], est: Matrix, b: Matrix, d: Matrix): Option[Matrix] = {
    nextEstimate(est, b, d) match {
      case Some(next) => {
        val df = diff(est, next)
        val ndiffs = (df +: diffs).take(probe)
        val n = max - 1
        val conv = if ((max - n) < converge) true else converges(ndiffs)
        //println((conv, (n < 1), acc(df), df))
        (conv, (n < 1), acc(df)) match {
          case (false, _, _) => None
          case (true, true, false) => None
          case (true, true, true) => Some(next)
          case (true, false, true) => Some(next)
          case (true, false, false) => iterate(n, ndiffs, next, b, d)
        }

      }
      case _ => None
    }
  }

  @tailrec
  private def dominate(cc: Int, m: Matrix, c: Matrix, perm: Matrix): (Option[Matrix], Option[Matrix], Option[Matrix]) = {
    val ncc = cc + 1
    val piv = pivot(cc, m)
    (piv.flatMap(_(m)), piv.flatMap(_ rowPermutation (c)), piv.flatMap(_ columnPermutation (perm))) match {
      case (Some(_m_), Some(_c_), Some(_p_)) => if (ncc == m.rows) (Some(_m_), Some(_c_), Some(_p_)) else dominate(ncc, _m_, _c_, _p_)
      case _ => (None, None, None)
    }
  }

  /////////////

  def apply(m: Matrix, c: Matrix): Option[Matrix] = {
    def relative(row: Int, col: Int, value: Double) = {
      val el = c(row, col) match {
        case None => 0.0
        case Some(e) => e
      }
      if (el != 0.0) (value / el) else 0.0
    }
    //
    // put the largest element on the diagonal in an attempt to make the value dominant
    //
    val (mM, cC, pP) = dominate(0, m, c, Matrix.Unit(m.rows, m.colls))

    //get values on the diagonal
    val vals = mM.map(_ values ((r: Int, c: Int) => (r == c))) match {
      case None => Vector()
      case Some(v) => v
    }
    //
    // scale the elements; remove diagonal elements
    //
    val D = mM.flatMap(_.flatMap((r: Int, c: Int, v: Double) => (if (r == c) 0 else v / vals(r))))
    val B = cC.flatMap(_.flatMap((r: Int, c: Int, v: Double) => (v / vals(r))))
    val start = Matrix.Zero(m.rows, 1)
    (B, D, pP) match {
      case (Some(b), Some(d), Some(perm)) => iterate(max, List(), start, b, d).map((perm ^) * _)
      case _ => None
    }

  }

}
