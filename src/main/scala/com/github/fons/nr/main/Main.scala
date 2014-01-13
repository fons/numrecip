package com.github.fons.nr.main
import com.github.fons.nr.test.MatrixTestRunner

import com.github.fons.nr.ode.{Solver, OdeSolverT, Factory}
import scala.math.{Pi, sin, cos}
import scala.util.{Try, Success, Failure}

object Main extends App {

  //MatrixTestRunner.run()

  /// ode to solve
  def f(t: Double, x: Double*): Double = -Pi * x(1)
  def g(t: Double, x: Double*): Double = Pi * x(0)
  //exact solution
  def fx(t: Double) = cos(Pi * t)
  def gx(t: Double) = sin(Pi * t)

  val ode : Try[OdeSolverT] = Factory(Solver.RKE56, 0.2, (0.0, List(1.0, 0.0)), List(f, g), 0.00001)
  ode match {
    case Success(ode) => {
        ode(0.15) match {
        case Success(((tn,results:List[Double]), _)) => {
          println("method : " + Solver.RKE56 + " for " + tn)
          println("returned : " + results(0) + ", " + results(1))
          println("exact    : " + fx(tn) + ", " + gx(tn))
        }
        case _ => println("failed")
      }
  }
    case _ => println("failed")
  }


}
