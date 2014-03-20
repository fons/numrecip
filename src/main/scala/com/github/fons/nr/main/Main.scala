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


import com.github.fons.nr.examples.PolynomialExample3


object Main extends App {

  //  def collect(n : Int, start : Int, max : Int, accum :List[(Int, Int)]) : List[(Int,Int)] = {
  //    val r = n - start
  //    if  (r > max) accum else collect(n, start - 1, max, (start, r)::accum)
  //  }
  //
  //  def multiply (lhs:Vector[Double], rhs : Vector[Double] ) = {
  //    val lhs_order = lhs.length - 1
  //    val rhs_order = rhs.length - 1
  //    val n = lhs_order + rhs_order
  //    for (k <- Range(0, n + 1)) yield {
  //      val start = if ( k < lhs_order) k else lhs_order
  //      val max   = if ( k < rhs_order) k else rhs_order
  //      collect(k, start, max, List()).map{case(x,y)=>lhs(x)*rhs(y)}.reduce(_ + _)
  //    }
  //  }

  //val p1 = PowerSeries(List(3.0, 1.0,2.0))
  //val p2 = PowerSeries(List(10.0,0.0, 9.0,1.0,2.0))
  //val p3 = p1 + p2
  //println(p3, p3.coeff, p3(2.0))
  //val p4 = p3.deriv
  //println(p4.coeff, p4(2.0))
  //  val k1 = Polynomial(List(10.0,0.0, 9.0,1.0,2.0))
  //  println(p2(2.0), k1(2.0))
  //
  //  println(p2.deriv(2.0), p2.coeff, p2.deriv.coeff)
  //  println(p2.deriv.deriv(2.0), p2.deriv.deriv.coeff)
  //  println(
  //    p2.∂.∂.∂(2.0) , p2.deriv.deriv.deriv.coeff, p2.deriv(p2.order()).coeff  ,p2.deriv(p2.order())(2.0)
  //  )
  //  println(k1.deriv(2.0), k1.coeff)
  //  println(p2.coeff)
  //  p2.inter()
  //  val d1 = p2.inter()
  //  println(d1.coeff, p2.inter()(2.0))
  //  val d5 = PowerSeries(List(1.0, 2.0, 3.0)) + 6.0
  //  println(d5, d5.coeff)
  //  d5.∂

  //  val p1 = new PowerSeries(List(6.0,5.0,4.0,3.0,2.0,1.0))  with SimpleConvolution
  //    //println(p1(2.0), p1.exp(2.0), scala.math.exp(1.0))
  //
  //  val p2 = new PowerSeries(List(-7.0,6.0,-5.0,4.0,-3.0,2.0))  with SimpleConvolution
  //  val p3 = new PowerSeries((p1</>p1).coeff) with SimpleConvolution
  //  println(p3.coeff, p3(1239.90))
  //  println((p3<*>p1).coeff, p1.coeff)

  //val lhs = List(1.0,2.0,3.0)
  //  val rhs = List(-1.0, 1.0, -2.0)
  //  val pr = prod(lhs, rhs)
  //  println(pr)
  //
  //  println(r)

  //  def rev_prod(lhs: List[Double], rhs:List[Double]) = {
  //    lhs.zip(rhs.reverse).map(xy=>(xy._1*xy._2)).reduce(_ + _)/(lhs.length + 1.0)
  //  }
  //  def exp_list(u :List[Double], accum:List[Double]):List[Double] = {
  //      accum.length match {
  //        case 0 => exp_list(u, List(scala.math.exp(u head)))
  //        case n if n == u.length => accum
  //        case _ => exp_list(u, rev_prod(accum,u)::accum)
  //      }
  //  }

  //val kl = exp_list(lhs, List())
  //println(kl)

  //val p4 = new PowerSeries(List(sqrt(2.0)/48.0,-sqrt(2.0)/12.0,-sqrt(2.0)*0.25,sqrt(2.0)*0.5,sqrt(2.0)*0.5))  with SimpleConvolution
  //
  //  println(p4.exp.coeff)
  //  val vx1 = exp(0.5*sqrt(2.0))
  //  val vx2 = 0.5 * vx1 * sqrt(2.0)
  //  println(vx2, vx1)


  //  println(power(5, 2.0), scala.math.pow(2.0, 5.0))
  //  def p(x:Double):Double = 3.0 * x
  //  def h = powerfn(3, p)
  //
  //  println(h(4.0))
  //
  //  println(power2(8, 5.0), power2(9,5.0))

  PolynomialExample3.run
}

