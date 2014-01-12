package com.github.fons.nr.ode

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/10/13
 * Time: 9:40 AM
 * To change this template use File | Settings | File Templates.
 */
trait AdaptiveStepStrategyT {
  def next_step_size(argold: (Double, List[Double]),
                     argnew: (Double, List[Double]),
                     step: Double,
                     errors: Option[List[Double]]): (AdaptiveStepTransition.Value, Double) = (AdaptiveStepTransition.CONTINUE, step)
}
