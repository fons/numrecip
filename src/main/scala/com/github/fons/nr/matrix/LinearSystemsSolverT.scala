package com.github.fons.nr.matrix

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/6/13
 * Time: 4:48 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.util.{Try, Success, Failure}

abstract class LinearSystemsSolverT {
  def apply(m: Matrix, c: Matrix): Option[Matrix]

}
