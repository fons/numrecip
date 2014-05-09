/*
 * Copyright (c) 2014.
 *
 * This file PolynomialExample2.scala is part of numrecip (numrecip)
 *
 *     numrecip / PolynomialExample2.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / PolynomialExample2.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.examples

import com.github.fons.nr.function.{SimpleConvolution, PowerSeries}

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 3/12/14
 * Time: 6:37 PM
 * To change this template use File | Settings | File Templates.
 */
object PolynomialExample2 {

  private
  def banner = println("\n------------------------------------------------------------------------------------------\n")

  def run {
    /*
     * Adding two power series
     */
    banner
    println("adding two power series\n")
    val p1 = PowerSeries(List(3.0, 1.0, 2.0))
    val p2 = PowerSeries(List(10.0, 0.0, 9.0, 1.0, 2.0))
    val p3 = p1 + p2
    println("p1 : " + p1.coeff)
    println("p2 : " + p2.coeff)
    println("p3=p1+p2", p3.coeff, p3(2.0))
    println("at 2.0 : " + p1(2.0) + " + " + p2(2.0) + "=" + p3(2.0))

    /*
    * Subtracting two power series
    */
    banner
    println("subtracting two power series\n")

    val p31 = p1 - p2
    println("p1 : " + p1.coeff)
    println("p2 : " + p2.coeff)
    println("p3=p1-p2", p31.coeff, p3(2.0))
    println("at 2.0 : " + p1(2.0) + " - " + p2(2.0) + "=" + p31(2.0))

    /*
    * Multiplying two power series, preserving the order of the series
    */
    banner
    println("multiplying two power series, preserving the order\n")

    val p1m = new PowerSeries(List(3.0, 5.0, 9.0, -2.0, 10.0)) with SimpleConvolution
    val p2m = new PowerSeries(List(5.0, 10.0, -30.0, 12.0, 22.0)) with SimpleConvolution
    val p3m = p1m <*> p2m
    println("p1m : " + p1m.coeff)
    println("p2m : " + p2m.coeff)
    println("p3m=p1m*p2m", p3m.coeff, p3m(2.0))
    println("order has to be the same : p1 order :" + p1m.order() + " p2 order : " + p2m.order())
    println("at 2.0 (preserves order, so not the same ) : " + p1m(2.0) + " <*> " + p2m(2.0) + "=" + p3m(2.0))

    /*
    * regular multiplication of two power series.
    */
    banner
    println("multiplying two power series outright\n")

    val p1m1 = new PowerSeries(List(3.0, 5.0, 9.0, -2.0, 10.0)) with SimpleConvolution
    val p2m1 = new PowerSeries(List(5.0, 10.0, -30.0, 12.0, 22.0)) with SimpleConvolution
    val p3m1 = p1m1 * p2m1
    println("p1m1 : " + p1m1.coeff)
    println("p2m1 : " + p2m1.coeff)
    println("p3m1=p1m1*p2m1", p3m1.coeff, p3m1(2.0))
    println("order has to be the same : p1 order :" + p1m.order() + " p2 order : " + p2m.order())
    println("at 2.0  : " + p1m1(2.0) + " * " + p2m1(2.0) + "=" + p3m1(2.0))

    /*
     * Division of two power series , preserving the order
     */
    banner
    println("Division of two power series , preserving the order\n")
    val xp1 = new PowerSeries(List(6.0, 5.0, 4.0, 3.0, 2.0, 1.0)) with SimpleConvolution
    val xp2 = new PowerSeries(List(-7.0, 6.0, -5.0, 4.0, -3.0, 2.0)) with SimpleConvolution
    val xp3 = new PowerSeries((xp1 </> xp2).coeff) with SimpleConvolution
    val xp4 = new PowerSeries((xp3 <*> xp2).coeff) with SimpleConvolution
    println("xp1 : " + xp1.coeff + " @ 2.0 : " + xp1(2.0))
    println("xp2 : " + xp2.coeff + " @ 2.0 : " + xp2(2.0))
    println("xp3 =xp1</>xp2", xp3.coeff + " @ 2 " + xp3(2.0))
    println("xp4 =xp3<*>xp2", xp4.coeff + " @ 2 " + xp4(2.0) + " should be the same as xp1(2.0) : " + xp1(2.0))
    println("order has to be the same : xp1 order :" + xp1.order() + " xp2 order : " + xp2.order())
    println("at 2.0  : " + xp1(2.0) + " </> " + xp2(2.0) + "=" + xp3(2.0))

    //  println((p3*p2).coeff)
    //    val p4 = p3.deriv
    //    println(p4.coeff, p4(2.0))
    //      val k1 = Polynomial(List(10.0,0.0, 9.0,1.0,2.0))
    //      println(p2(2.0), k1(2.0))
    //
    //      println(p2.deriv(2.0), p2.coeff, p2.deriv.coeff)
    //      println(p2.deriv.deriv(2.0), p2.deriv.deriv.coeff)
    //      println(
    //        p2.∂.∂.∂(2.0) , p2.deriv.deriv.deriv.coeff, p2.deriv(p2.order()).coeff  ,p2.deriv(p2.order())(2.0)
    //      )
    //      println(k1.deriv(2.0), k1.coeff)
    //      println(p2.coeff)
    //      p2.inter()
    //      val d1 = p2.inter()
    //      println(d1.coeff, p2.inter()(2.0))
    //      val d5 = PowerSeries(List(1.0, 2.0, 3.0)) + 6.0
    //      println(d5, d5.coeff)
    //      d5.∂

  }

}
