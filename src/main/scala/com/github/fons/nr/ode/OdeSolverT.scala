package com.github.fons.nr.ode

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/23/13
 * Time: 8:46 AM
 * To change this template use File | Settings | File Templates.
 */

import scala.util.{Try, Success, Failure}

abstract class OdeSolverT {
  def apply(t: Double): Try[((Double, List[Double]), MemoizeT)]
}
