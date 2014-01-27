/*
 * Copyright (c) 2014.
 *
 * This file Main.scala is part of numrecip (numrecip)
 *
 *     numrecip / Main.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / Main.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.main

import com.github.fons.nr.examples._
import com.github.fons.nr.ode._
import com.github.fons.nr.interpolation._
import com.github.fons.nr.ode.butcher.tableau._
import scala.math._
import scala.util.{Success, Try}
import com.github.fons.nr.matrix.LUSolver

trait Testy[U, T <: Iterable[U]] {
  val Coffee : T
  override
  val toString = "testy " + Coffee
}

object Main extends App {



  val xvals = Vector(0.1,0.2,0.3,0.4,0.5)
  val yvals = Vector(-1.6228,-0.8218,-0.3027,0.1048,0.4542)
  val x = 0.15
//  val xvals = Vector(2.0,3.0,5.0,8.0)
//  val yvals = Vector(3.0,8.0,4.0,2.0)
//  val x = 4.0

  val inter = new {val Degree = 4} with Interpolator(DataSet(xvals, yvals)) with LagrangeNeville with BasicNevilleStrategy
  println(inter)
  val resx = inter(x)
  println(resx)

  //val c = new {val Coffee = List(45)} with Testy[Int, List[Int]]
  //println(c)
}
