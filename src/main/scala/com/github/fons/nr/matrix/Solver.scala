package com.github.fons.nr.matrix

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/8/14
 * Time: 6:40 PM
 * To change this template use File | Settings | File Templates.
 */
object Solver extends Enumeration {
  type Type = Value
  val GaussJordanFullPivot = Value(0, "GausJordanFullPivot")
  val GaussJordanRowPivot = Value(1, "GausJordanRowPivot")
  val IterativeGaussSeidel = Value(2, "IterariveGaussSeidel")
  val IterativeJacobi = Value(3, "IterativeJacobi")
  val LUSolver = Value(4, "LUSolver")

}
