package com.github.fons.nr.ode

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 10/27/13
 * Time: 8:20 AM
 * To change this template use File | Settings | File Templates.
 */

object Integrator {
  def to_multi_arg(func: (Double, Double) => Double)(z: Double, y: Double*) = func(z, y(0))
}

class Integrator(val stepz: Double, val initz: (Double, Double), val func: (Double, Double) => Double) extends OdeSolver(stepz, (initz._1, List(initz._2)), List[(Double, Double *) => Double](Integrator.to_multi_arg(func))) {

}

