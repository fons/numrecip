package com.github.fons.nr.test

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/9/14
 * Time: 6:37 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.Some
import scala.util.{Either, Right, Left}
import com.github.fons.nr.matrix.{Matrix, Solver, LinearSystemsSolverT, Factory}

case class MatrixTest(Type: Solver.Type, exact: Matrix, m: Matrix) {
  private
  val solver: Option[LinearSystemsSolverT] = Factory(Type)

  private
  val type_name: String = Type.toString

  private
  def run_solver(solver: LinearSystemsSolverT) = {
    val coeff = m * exact
    val t1 = System.nanoTime()
    solver(m, coeff) match {
      case None => MatrixTestResult(Type, type_name + " : no solution found", 1.0e-9 * (System.nanoTime() - t1), Right((exact, coeff, m)))
      case Some(result) => {
        (result - exact).coll(0).map(_ reduce (_ + _)) match {
          case Some(v) => MatrixTestResult(Type, type_name + " : success", 1.0e-9 * (System.nanoTime() - t1), Left(v))
          case _ => MatrixTestResult(Type, type_name + " : solution out-of-bounds", 1.0e-9 * (System.nanoTime() - t1), Right((exact, coeff, m)))
        }
      }
    }
  }

  def run() = {
    solver match {
      case Some(_solver_) => run_solver(_solver_)
      case _ => MatrixTestResult(Type, type_name + " : no solver found", 0.0, Right((exact, m * exact, m)))
    }
  }

}
