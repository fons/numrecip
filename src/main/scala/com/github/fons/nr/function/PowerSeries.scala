/*
 * Copyright (c) 2014.
 *
 * This file PowerSeries.scala is part of numrecip (numrecip)
 *
 *     numrecip / PowerSeries.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / PowerSeries.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.function

import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 3/2/14
 * Time: 5:37 PM
 * To change this template use File | Settings | File Templates.
 */
object PowerSeries {

  def apply(coeffs: Map[Int, Double]): Polynomial = {
    PowerSeries(coeffs, 0.0)
  }

  def apply(coeffs: Map[Int, Double], shift: Double) = {
    val maxkey = coeffs.keys.max
    val list = (for (index <- Range(0, maxkey + 1)) yield {
      coeffs.get(index) match {
        case None => 0
        case Some(c) => c
      }
    }).toList.reverse
    new PowerSeries(list, shift)
  }

}


case class PowerSeries(override val coeff: List[Double], val shift: Double = 0.0) extends Polynomial(coeff, (x) => (x - shift)) {

  @tailrec
  private
  def power_ps_alt(n: Int, p: PowerSeries): PowerSeries = {
    n match {
      case 1 => p
      case m => power_ps_alt(n / 2, p * p)
    }
  }

  private
  def power_ps(n: Int, accum: PowerSeries): PowerSeries = {
    n match {
      case 0 => PowerSeries(List(1.0))
      case 1 => accum
      case p if p > 0 => {
        p - 2 * (p / 2) match {
          case 0 => power_ps_alt(p, accum)
          case 1 => accum * power_ps_alt(p - 1, accum)
        }
      }
    }
  }

  private
  def rev_prod(lhs: List[Double], rhs: List[Double]) = {
    lhs.zip(rhs.reverse).map(xy => (xy._1 * xy._2)).reduce(_ + _) / (lhs.length)
  }

  @tailrec
  private
  def exp_list(u: List[Double], accum: List[Double], pacc: List[Double]): List[Double] = {
    (u, accum.length) match {
      case (x :: xs, 0) => exp_list(xs, List(scala.math.exp(x)), x :: pacc)
      case (List(), _) => accum
      case (x :: xs, _) => exp_list(xs, rev_prod(accum.reverse, pacc) :: accum, x :: pacc)
    }
  }

  private
  lazy
  val errmsg = "power series should have the same shift : this shift is " + shift + " and the other is "

  private
  def check_shift(other_shift: Double) {
    if (shift != other_shift)
      throw new IllegalArgumentException(errmsg + other_shift)
  }

  def exp = PowerSeries(exp_list(coeff.reverse, List(), List()), shift)


  override
  def ^(n: Int) = power_ps(n, this)

  override
  def pow(n: Int) = power_ps(n, this)

  def +(other: PowerSeries): PowerSeries = {
    check_shift(other.shift)
    val poly = super.+(other)
    new PowerSeries(poly.coeff, shift)
  }

  def +>(other: PowerSeries): Option[PowerSeries] = {
    try {
      Some(this + other)
    }
    catch {
      case _: Throwable => None
    }
  }

  def -(other: PowerSeries): Polynomial = {
    check_shift(other.shift)
    val poly = super.-(other)
    new PowerSeries(poly.coeff, shift)
  }

  def ->(other: PowerSeries): Option[PowerSeries] = {
    try {
      Some(this + other)
    }
    catch {
      case _: Throwable => None
    }
  }

  /*
   * This multiplication preserves the order of the power series.
   * Basically it multplies two taylor series
   */

  def <*>(other: PowerSeries) = {
    check_shift(other.shift)
    val poly = super.<*>(other)
    new PowerSeries(poly.coeff, shift)
  }

  def <*>>(other: PowerSeries): Option[PowerSeries] = {
    try {
      Some(this <*> other)
    }
    catch {
      case _: Throwable => None
    }
  }

  def *(other: PowerSeries) = {
    check_shift(other.shift)
    val poly = super.*(other)
    new PowerSeries(poly.coeff, shift)
  }

  def *>(other: PowerSeries): Option[PowerSeries] = {
    try {
      Some(this * other)
    }
    catch {
      case _: Throwable => None
    }
  }


  def </>(other: PowerSeries) = {
    check_shift(other.shift)
    val poly = super.</>(other)
    new PowerSeries(poly.coeff, shift)
  }

  def </>>(other: PowerSeries): Option[PowerSeries] = {
    try {
      Some(this </> other)
    }
    catch {
      case _: Throwable => None
    }
  }

  /*
   * Derivative override. The Derivative of a power series is an other power series
   */
  override
  def deriv: PowerSeries = {
    order() match {
      case 0 => PowerSeries(List(0.0), 0)
      case n if n > 0 => {
        val dl = Range(order(), 0, -1).zip(coeff.dropRight(1)).toList.map((xy) => xy._1 * xy._2)
        PowerSeries(dl, shift)
      }
    }
  }

  def deriv(n: Int): PowerSeries = {

    n match {
      case 0 => this
      case n if n > 0 => this.deriv.deriv(n - 1)
      case n if n < 0 => throw new IllegalArgumentException("derivative has to be positive; you provided " + n)
    }
  }

  override
  def ∂ = deriv

  def ∂(n: Int) = deriv(n)

  def inter(zeroVal: Double = 0.0): PowerSeries = {
    val dl = Range(order() + 1, 0, -1).map(1.0 / _).zip(coeff).toVector.map((xy) => xy._1 * xy._2) ++ Vector(zeroVal)
    PowerSeries(dl.toList, shift)
  }

  /*
  alt b
   */
  def ∫(zeroVal: Double = 0.0) = inter(zeroVal)


  lazy
  override
  val toString = "[" + className(this) + "@" + hashCode() + "@(" + coeff.length + " elements)@{" + coeff.head + "..." + coeff.last + "}]"
}
