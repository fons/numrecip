package com.github.fons.nr.ode

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 10/23/13
 * Time: 9:18 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.util.{Try, Success, Failure}

trait Euler extends OdeStepWithErrorEstimateT {


  override def next_with_error(Step: Double, Args: (Double, List[Double]),
                               Func: List[(Double, Double *) => Double]): (Try[(Double, List[Double])], Option[List[Double]]) = {

    val res1 = next(Step, Args, Func)
    val res2 = next(Step * 0.5, Args, Func)
    val res3 = res2.flatMap(next(Step * 0.5, _, Func))
    val diff = res1 match {
      case Failure(f) => None
      case Success(r1) => {
        res3 match {
          case Failure(g) => None
          case Success(r3) => Some((r1._2, r3._2).zipped map (_ - _))
        }
      }
    }
    (res3, diff)
  }

  override def next(Step: Double, Args: (Double, List[Double]),
                    Func: List[(Double, Double *) => Double]): Try[(Double, List[Double])] = {
    val (time, args) = Args
    val delta = Func map (_(time, args: _*) * Step)
    val newargs = (args, delta).zipped map (_ + _)
    Success((time + Step, newargs))
  }

  override def stepperName = className(this) + " extends " + super.stepperName + " (no tableau)"
}


