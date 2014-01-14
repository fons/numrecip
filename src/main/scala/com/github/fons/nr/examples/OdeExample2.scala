package com.github.fons.nr.examples

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/13/14
 * Time: 6:16 PM
 * To change this template use File | Settings | File Templates.
 */

import scala.math._
import scala.util.{Success, Try}

import com.github.fons.nr.ode.{Solver, OdeSolverT, Factory}

object OdeExample2 {
  def run {
    /// ode to solve
    def f(t: Double, x: Double*): Double = -Pi * x(1)
    def g(t: Double, x: Double*): Double = Pi * x(0)
    //exact solution
    def fx(t: Double) = cos(Pi * t)
    def gx(t: Double) = sin(Pi * t)

    /**
        // Method    : Solver.EU; Euler method
        // step size : 0.01
        // initial conditions : (0.0, List(1.0, 0.0))
        //                      0.0 : initial independent variable
        // set of ode's       : List(f,g)
        //
      **/

    val ode : Try[OdeSolverT] = Factory(Solver.EU, 0.01, (0.0, List(1.0, 0.0)), List(f, g))
    ode match {
      case Success(ode) => {
        ode(0.15) match {
          case Success(((tn,results:List[Double]), _)) => {
            println("method : " + Solver.EU + " for " + tn)
            println("returned : " + results(0) + ", " + results(1))
            println("exact    : " + fx(tn) + ", " + gx(tn))
          }
          case _ => println("failed")
        }
      }
      case _ => println("failed")
    }

  }

}
