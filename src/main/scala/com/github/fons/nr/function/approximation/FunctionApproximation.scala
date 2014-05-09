/*
 * Copyright (c) 2014.
 *
 * This file FunctionApproximation.scala is part of numrecip (numrecip)
 *
 *     numrecip / FunctionApproximation.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / FunctionApproximation.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.function.approximation

import com.github.fons.nr.util.{Interval, liftedSub, liftedSum, liftedProd, liftedDiv}

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 4/16/14
 * Time: 6:33 PM
 * To change this template use File | Settings | File Templates.
 */
case class FunctionApproximation(val coefficients: List[Double],
                                 val approximation: (Double) => Option[Double],
                                 val interval: Interval,
                                 private val func: (Double) => Option[Double],
                                 private val appfactory: Option[ApproximationT] = None) extends PartialFunction[Double, Option[Double]] {

  private
  val coeffToString = coeff2String(coefficients)

  private
  def coeff2String(coeff: List[Double]): String = {
    if (coeff.length < 5) {
      coeff.take(4).toString()
    }
    else {
      (("..." :: (coeff.take(4).map(_.toString()).reverse)).reverse).toString()
    }
  }

  override
  def apply(v1: Double): Option[Double] = approximation(v1)

  override
  def isDefinedAt(x: Double): Boolean = interval.inin(x)

  final
  override
  def equals(other: Any) = {
    val that = other.asInstanceOf[FunctionApproximation]
    (that != null) && (coefficients == that.coefficients && appfactory == that.appfactory)
  }

  def +>(v1: FunctionApproximation): Option[FunctionApproximation] = {
    appfactory == v1.appfactory && (interval == v1.interval) match {
      case false => None
      case true => appfactory.flatMap(_ <> ((x: Double) => liftedSum(func(x), v1.func(x))))
    }
  }

  def <+>(v1: Option[FunctionApproximation]): Option[FunctionApproximation] = v1.flatMap(this +> _)

  def <->(v1: Option[FunctionApproximation]): Option[FunctionApproximation] = v1.flatMap(this -> _)

  def <*>(v1: Option[FunctionApproximation]): Option[FunctionApproximation] = v1.flatMap(this *> _)

  def </>(v1: Option[FunctionApproximation]): Option[FunctionApproximation] = v1.flatMap(this /> _)

  def ->(v1: FunctionApproximation): Option[FunctionApproximation] = {
    appfactory == v1.appfactory && (interval == v1.interval) match {
      case false => None
      case true => appfactory.flatMap(_ <> ((x: Double) => liftedSub(func(x), v1.func(x))))
    }
  }

  def *>(v1: FunctionApproximation): Option[FunctionApproximation] = {
    appfactory == v1.appfactory && (interval == v1.interval) match {
      case false => None
      case true => appfactory.flatMap(_ <> ((x: Double) => liftedProd(func(x), v1.func(x))))
    }
  }

  def />(v1: FunctionApproximation): Option[FunctionApproximation] = {
    appfactory == v1.appfactory && (interval == v1.interval) match {
      case false => None
      case true => appfactory.flatMap(_ <> ((x: Double) => liftedDiv(func(x), v1.func(x))))
    }
  }

  def deriv: Option[FunctionApproximation] = appfactory.flatMap(_.derivative(this))

  def deriv(n: Int): Option[FunctionApproximation] = {
    n match {
      case 0 => Some(this)
      case n if n > 0 => this.deriv.flatMap(_.deriv(n - 1))
      case n if n < 0 => None
    }
  }

  def ∂ = deriv

  def ∂(n: Int) = deriv(n)

  def inter(x0: Double): Option[FunctionApproximation] = appfactory.flatMap(_.integral(x0, this))

  def ∫(x: Double) = inter(x) //alt b


  def order: Double = coefficients.length - 1

  override
  lazy
  val toString = "FunctionApproximation$[order : " + order + "]@" + hashCode() + "{" + coeffToString + "," + interval + "," + appfactory + "}"

}
