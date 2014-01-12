package com.github.fons.nr.ode

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/10/13
 * Time: 11:17 AM
 * To change this template use File | Settings | File Templates.
 */

import scala.util.{Try, Success, Failure}

trait OdeStepWithErrorEstimateT extends OdeStepT {
  def next_with_error(step: Double, Args: (Double, List[Double]),
                      Func: List[(Double, Double *) => Double]): (Try[(Double, List[Double])], Option[List[Double]]) = {
    (Failure(new RuntimeException(stepperName + " has no implementation for next_with_error method")), None)
  }

  override def stepperName = "com.mhsw.com.github.fons.nr.ode.OdeStepWithErrorEstimateT " // causes recursive call ?? + super.stepperName
}
