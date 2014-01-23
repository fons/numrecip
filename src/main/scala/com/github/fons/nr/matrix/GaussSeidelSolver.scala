package com.github.fons.nr.matrix

import scala.annotation.tailrec
import com.github.fons.nr.util.Accuracy

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/30/13
 * Time: 12:23 PM
 * To change this template use File | Settings | File Templates.
 */
case class GaussSeidelSolver(acc: Accuracy = Accuracy(), max: Int = 200, probe: Int = 10, converge: Int = 10) extends IterativeSolver(acc, max, probe, converge) {

  private def merge(cr: Int, next: Matrix, est: Matrix) = {
    cr match {
      case cr if cr == 0 => next :|>:+> est.takeRows(cr + 1, est.rows)
      case cr if cr == (est.rows - 1) => est.takeRows(0, cr).map(_ :|>:+ next)
      case cr => est.takeRows(0, cr).map(_ :|>:+ next).flatMap(_ :|>:+> est.takeRows(cr + 1, est.rows))
    }
  }

  @tailrec final def relax(cr: Int, estimate: Matrix, b: Matrix, d: Matrix): Option[Matrix] = {
    val ncr = cr + 1
    val Dn = d.takeRow(cr).map(_ * estimate)
    b.takeRow(cr).flatMap(_ -> Dn).flatMap(merge(cr, _, estimate)) match {
      case Some(next) => if (ncr == d.rows) Some(next) else relax(ncr, next, b, d)
      case None => None
    }
  }

  final def nextEstimate(est: Matrix, b: Matrix, d: Matrix): Option[Matrix] = relax(0, est, b, d)

  override
  def solverName = className(this)
}
