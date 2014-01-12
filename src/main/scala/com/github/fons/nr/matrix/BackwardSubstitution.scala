package com.github.fons.nr.matrix

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/13/13
 * Time: 3:51 PM
 * To change this template use File | Settings | File Templates.
 */
object BackwardSubstitution extends LinearSystemsSolverT {
  override def apply(m: Matrix, c: Matrix): Option[Matrix] = Substitute.backward(m, c)
}
