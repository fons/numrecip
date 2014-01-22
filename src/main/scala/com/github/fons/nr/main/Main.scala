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


object Main extends App {


  def reduce1(v: Double, x: Vector[Double], f: Vector[Double]): Vector[Double] = {
    val l = x.zip(f)
    val k = l.zip(l tail)
    val newl = for (((x1, f1), (x2, f2)) <- k) yield {
      1.0 / (x1 - x2) * ((v - x2) * f2 - (v - x1) * f1)
    }
    val newx = x.head +: (x.tail.tail)

    newl match {
      case Vector() => newl
      case _ => reduce1(v, newx, newl)
    }
  }
   //CubicSplineExample1.run
   CubicSplineExample3.run
//  val m1 = Map('a'->10, 'b' -> 20)
//  println(m1)
//  val m2 = m1 + ('a'->100, 'b'->67)
//  println(m2)
}
