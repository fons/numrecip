package com.github.fons.nr.ode

import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/4/13
 * Time: 6:10 AM
 * To change this template use File | Settings | File Templates.
 */

import scala.util.{Try, Success, Failure}

class OdeSolver(private val step: Double, private val init: (Double, List[Double]), private val Func: List[(Double, Double *) => Double]) extends OdeSolverT with OdeStepT with MemoizeT {

  @tailrec private def run(count: Int, fstep: Double, xtuple: (Double, List[Double]), m: MemoizeT): (Try[(Double, List[Double])], MemoizeT) = {
    count match {
      case 0 => (Success(xtuple), m)
      case _ => {
        next(fstep, xtuple, Func) match {
          case Success(n) => run(count - 1, fstep, n, m.store(n))
          case Failure(e) => (Failure(e), m)
        }
      }
    }
  }

  def apply(t: Double): Try[((Double, List[Double]), MemoizeT)] = {
    if (t == init._1) {
      Success((init, store(init)))
    }
    else {
      val span = (t - init._1);
      val n = (0.5 + (span / step).abs).toInt
      val fstep = (span / (1.0 * n))
      run(n, fstep, init, store(init)) match {
        case (Success(res), m) => Success((res, m))
        case (Failure(e), m) => Failure(e)
      }
    }
  }

  //private def className[A](a: A)(implicit m: Manifest[A]) = m.toString

  override lazy val toString = className(this) + "( step : " + step + ", init : " + init + "," + Func.toString + ")" + " : " + stepperName
}

