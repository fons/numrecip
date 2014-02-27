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
import com.github.fons.nr.interpolation._
import com.github.fons.nr.ode._
import scala.math._

import scala.util.{Success, Try}

import com.github.fons.nr.ode.butcher.tableau.RKE56Tableau


import scala.util.Success
import com.github.fons.nr.util.Accuracy
import com.github.fons.nr.interpolation.Interpolator
import scala.Some
import com.github.fons.nr.matrix.LUSolver
import scala.annotation.tailrec
import com.github.fons.nr.diff.DiffFunc

object Polynomial {

  def apply(coeffs : Map[Int,Double]):Polynomial  = {
     Polynomial(coeffs, (x:Double)=>x)
    }

  def apply(coeffs : Map[Int,Double], f:(Double)=>Double)  = {
    val maxkey = coeffs.keys.max
    val list = (for (index <- Range(0, maxkey+1)) yield {
      coeffs.get(index) match {
        case None => 0
        case Some(c) => c
      }
    }).toList.reverse
    new Polynomial(list, f)
  }

}


case class Polynomial(coeff:List[Double], f : (Double)=>Double=(x:Double)=>x) {

  private def horner(x: Double, coeff:List[Double], accum:Double):Double = {
    coeff match {
      case List() => accum
      case c::cs  => horner(x, cs, (accum * f(x) + c))
    }
  }

  def apply(x : Double) : Option[Double]= {
    coeff match {
      case List()  => None
      case List(c) => Some(c)
      case c::cs   => Some(horner(x, cs, c))
    }
  }
}
object Main extends App {
  val cp = Polynomial(List(1.0, 1.0, 1.0, 1.0))
  val r = cp(2)
  println("r => ", r)

  val cp1 = Polynomial(List(1.0, 1.0, 1.0, 1.0), (x:Double)=>x-2.5)
  val r1 = cp1(2)
  println("r1 => ", r1)

  val cp2 = Polynomial(Map((0->0.0), (4->1.0)), (x:Double)=>x)
  val r2 = cp2(2)
  println("r2 => ", r2)
}
