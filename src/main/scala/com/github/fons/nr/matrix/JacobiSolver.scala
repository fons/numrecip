package com.github.fons.nr.matrix

import com.github.fons.nr.util.Accuracy

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/30/13
 * Time: 9:38 AM
 * To change this template use File | Settings | File Templates.
 */
case class JacobiSolver(acc: Accuracy = Accuracy(), max: Int = 200, probe: Int = 10, converge: Int = 10) extends IterativeSolver(acc, max, probe, converge) {

  final def nextEstimate(est: Matrix, b: Matrix, d: Matrix): Option[Matrix] = Some(b - d * est)

  override def apply(m: Matrix, c: Matrix): Option[Matrix] = None //solve(m,c)

  override
  def solverName = className(this)
}