package com.github.fons.nr.ode

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/4/13
 * Time: 6:10 AM
 * To change this template use File | Settings | File Templates.
 */

import scala.util.{Try, Success, Failure}

trait OdeStepT {

  def next(step: Double, Args: (Double, List[Double]),
           Func: List[(Double, Double *) => Double]): Try[(Double, List[Double])] = {
    Failure(new NoSuchMethodError(stepperName + " has no implementation for next method"))
  }

  protected def className[A](a: A)(implicit m: Manifest[A]) = m.toString

  def stepperName = className(this)
}


