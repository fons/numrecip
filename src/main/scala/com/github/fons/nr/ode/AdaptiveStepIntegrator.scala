package com.github.fons.nr.ode

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/14/13
 * Time: 6:21 PM
 * To change this template use File | Settings | File Templates.
 */

class AdaptiveStepIntegrator(val stepz: Double, val initz: (Double, Double), val func: (Double, Double) => Double)
  extends AdaptiveStepOdeSolver(stepz, (initz._1, List(initz._2)), List[(Double, Double *) => Double](Integrator.to_multi_arg(func))) {

}
