/*
 * Copyright (c) 2014.
 *
 * This file OdeMemoizeSpline.scala is part of numrecip (numrecip)
 *
 *     numrecip / OdeMemoizeSpline.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / OdeMemoizeSpline.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.examples

import scala.math._
import com.github.fons.nr.ode.{Memoize, ExplicitRungeKutta, OdeSolver}
import com.github.fons.nr.ode.butcher.tableau.RKE56Tableau
import scala.util.Success
import com.github.fons.nr.interpolation.{NormalSpline, CubicSpline, Interpolator, DataSet}
import com.github.fons.nr.matrix.LUSolver

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/25/14
 * Time: 9:14 AM
 * To change this template use File | Settings | File Templates.
 */
object OdeMemoizeSpline {
           def run {
             def f(t: Double, x: Seq[Double]): Double = -Pi * x(1)

             def g(t: Double, x: Seq[Double]): Double = Pi * x(0)

             val step = 0.01
             val init = (1.0, List(1.0,0.0))
             val ode = new OdeSolver(step, init, List(f,g)) with ExplicitRungeKutta with RKE56Tableau with Memoize

             val result = ode(1.25)
             result match {
               case Success(((tn, results), mem)) => {
                 val ds = DataSet(mem.toList)
                 ds.pp
                 //println(ds)
                 val cs = new Interpolator(ds) with CubicSpline with NormalSpline with LUSolver
                 val i = cs(1.134)
                 println(i)
               }
               case _ => println("error")
             }
           }
}
