package com.github.fons.nr.matrix

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/13/13
 * Time: 3:50 PM
 * To change this template use File | Settings | File Templates.
 */
object ForwardSubstitution extends LinearSystemsSolverT {
  override def apply(m: Matrix, c: Matrix): Option[Matrix] = Substitute.forward(m, c)
}
