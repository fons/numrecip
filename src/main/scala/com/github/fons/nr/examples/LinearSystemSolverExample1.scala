package com.github.fons.nr.examples

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/13/14
 * Time: 6:46 PM
 * To change this template use File | Settings | File Templates.
 */
import com.github.fons.nr.matrix.{Factory, LinearSystemsSolverT, Solver, Matrix}
object LinearSystemSolverExample1 {
    def run {
      /**
      // System :

             x_0 + 2*x_1 + 3 * x_3 = 10
         2 * x_0 + 3*x_1 +     x_3 = 11
         3 * x_0 +   x_1 + 2 * x_3 = 12

      // Initialize a Matrix with the left and right hand side coefffiecients of
      //

      **/
      val m = Matrix(List(List(1.0,2.0,3.0), List(2.0,3.0,1.0), List(3.0,1.0,2.0)))
      val c = Matrix(List(List(10.0), List(11.0), List(12.0)))
      val solver : Option[LinearSystemsSolverT] = Factory(Solver.LUSolver)
      solver.flatMap(_ (m,c)) match {
        case Some(r) => {
          r.pp
          (m*r - c).pp
        }
        case _ => println("error")
      }
    }
}
