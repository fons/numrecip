package com.github.fons.nr.ode

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/10/13
 * Time: 2:40 PM
 * To change this template use File | Settings | File Templates.
 */

import scala.math

trait AdaptiveStep extends AdaptiveStepStrategyT {

  val required_accuracy: Double
  private val safety = 0.90
  private val pgrow = -0.20
  private val pshrink = -0.25
  private val maxdelta = 0.1
  private val maxgrow = 5
  private val errcon = scala.math.pow((5.0 / safety), (1.0 / pgrow))

  private def increase_step(ratio: Double, step: Double) = {

    val new_step = if (ratio > errcon) safety * step * scala.math.pow(ratio, pgrow) else 5.0 * step
    //println("increase step" , new_step)
    new_step
  }

  private def decrease_step(ratio: Double, step: Double) = {
    //println("decrease step")
    val hn = safety * step * scala.math.pow(ratio, pshrink)
    if (step > 0) scala.math.max(hn, 0.1 * step) else scala.math.min(hn, 0.1 * step)
  }

  private def next_step_size(step: Double, errors: List[Double]): (AdaptiveStepTransition.Value, Double) = {
    //println("errors : " + errors)
    val maxerr = errors reduceLeft ((x, y) => if (x > y) x else y)
    //println("current step size : ", step)
    //println("maxerr :" + maxerr.toString + "  required : " + required_accuracy.toString)
    val ratio = scala.math.abs(maxerr / required_accuracy)
    //println("ratio :", ratio)
    if (ratio <= 1.0) (AdaptiveStepTransition.CONTINUE, increase_step(ratio, step))
    else {
      (AdaptiveStepTransition.RETRY, decrease_step(ratio, step))
    }
  }

  override def next_step_size(argold: (Double, List[Double]),
                              argnew: (Double, List[Double]),
                              step: Double,
                              errors: Option[List[Double]]): (AdaptiveStepTransition.Value, Double) = {
    // without errors there is not much to do but to continue..
    //
    errors match {
      case Some(errs) => next_step_size(step, errs)
      case None => (AdaptiveStepTransition.CONTINUE, step)
    }
  }
}
