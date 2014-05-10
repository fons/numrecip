package com.github.fons.nr.ode

import scala.annotation.tailrec
import com.github.fons.nr.interpolation.DataSet

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/10/13
 * Time: 10:29 AM
 * To change this template use File | Settings | File Templates.
 */

import scala.util.{Try, Success, Failure}
import scala.math.abs

class AdaptiveStepOdeSolver(val step: Double, val init: (Double, List[Double]), val Func: List[(Double, Double *) => Double])
  extends OdeSolverT with OdeStepWithErrorEstimateT with AdaptiveStepStrategyT with MemoizeT {

  val max_retry = 100
  val step_offset = 1.0E-16 //should be minimum /smallest Double -1

  private def reached(Current: Double, Target: Double): Boolean = {
    Current + step_offset >= Target
  }

  // for now only check to see if the dependend variable is stuck
  private def trapped(old_value: (Double, List[Double]), new_value: (Double, List[Double])) = {
    val (old_time, old_results) = old_value
    val (new_time, new_results) = new_value
    val crit1 = math.abs(old_time - new_time) > step_offset
    //val  c1 = (old_results, new_results).zipped map ((x:Double, y:Double)=> (math.abs(x - y) > step_offset))
    //val  crit2 = c1.foldLeft(true)(_ && _)
    //println("==> trapped : " ,crit1, old_results, new_results)
    !(crit1) //&& crit2)
  }

  private def revise_step_size(new_step_size: Double, target: Double, new_value: (Double, List[Double])) = {
    val (new_time, _) = new_value
    val revised_step_size = if (new_time + new_step_size > target) (target - new_time) else new_step_size
    //println("==>continuing with revised step size ", revised_step_size, new_time, target)
    revised_step_size
  }

  @tailrec
  private def run(target: Double, retry: Int, fstep: Double, value: (Double, List[Double]), m: MemoizeT): (Try[(Double, List[Double])], MemoizeT) = {
    //println("==> value ", value, target, fstep)
    value match {
      case (current_time, _) if reached(current_time, target) => (Success(value), m)
      case (current_time, _) => {
        var (nv, error) = next_with_error(fstep, value, Func)
        //println ("new value :", nv)
        nv match {
          case Failure(f) => (Failure(f), m)
          case Success(new_value) => {
            trapped(value, new_value) match {
              case true => (Failure(new RuntimeException("results are trapped in " + className(this))), m)
              case false => next_step_size(value, new_value, fstep, error) match {
                case (AdaptiveStepTransition.HALT, _) => (Failure(new RuntimeException("Result failed to converge")), m)
                case (AdaptiveStepTransition.CONTINUE, new_step_size) => run(target, 0, revise_step_size(new_step_size, target, new_value), new_value, m.store(new_value))
                case (AdaptiveStepTransition.RETRY, new_step_size) => {
                  //println("retry with target", target, " step : ", new_step_size, "rtry :", retry, " val :" , value)
                  retry match {
                    case `max_retry` => (Failure(new RuntimeException("max number of retries " + max_retry + " exceeded")), m)
                    case _ => run(target, (retry + 1), new_step_size, value, m)
                  }
                }

              }

            }
          }

        }
      }
    }
  }


  def apply(t: Double): Try[OdeResult] = {
    if (t == init._1) {
      Success(OdeResult(init, init, DataSet(store(init).toList), this.toString))
    }
    else {
      val span = (t - init._1);
      val n = (0.5 + (span / step).abs).toInt
      val fstep = (span / (1.0 * n))
      //println("revised step ", fstep)
      run(t, 0, fstep, init, store(init)) match {
        case (Success(res), m) => Success(OdeResult(init,res, DataSet(m.toList), this.toString))
        case (Failure(e), m) => Failure(e)
      }
    }
  }

  override lazy val toString = className(this) + " : " + stepperName

}
