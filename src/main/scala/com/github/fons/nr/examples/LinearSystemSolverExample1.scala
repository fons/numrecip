/*
 * Copyright (c) 2014.
 *
 * This file LinearSystemSolverExample1.scala is part of numrecip (numrecip)
 *
 *     numrecip / LinearSystemSolverExample1.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / LinearSystemSolverExample1.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

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

      // Initialize a Matrix with the left and right hand side coefficients of
      //

      * */
    val m = Matrix(List(List(1.0, 2.0, 3.0), List(2.0, 3.0, 1.0), List(3.0, 1.0, 2.0)))
    val c = Matrix(List(List(10.0), List(11.0), List(12.0)))
    val solver: Option[LinearSystemsSolverT] = Factory(Solver.LUSolver)
    solver.flatMap(_(m, c)) match {
      case Some(r) => {
        r.pp
        (m * r - c).pp
      }
      case _ => println("error")
    }
  }
}
