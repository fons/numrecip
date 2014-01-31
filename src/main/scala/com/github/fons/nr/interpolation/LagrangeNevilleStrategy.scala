/*
 * Copyright (c) 2014.
 *
 * This file BasicNevilleStrategy.scala is part of numrecip (numrecip)
 *
 *     numrecip / BasicNevilleStrategy.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / BasicNevilleStrategy.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.interpolation

import scala.annotation.tailrec
import scala.None

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/27/14
 * Time: 1:22 PM
 * To change this template use File | Settings | File Templates.
 */
trait LagrangeNevilleStrategy extends StrategyT[InterpolationT] with Degree {

  private
  case class LagrangeNevilleInterpolation(from: Double, to: Double, xvals:Vector[Double], yvals:Vector[Double]) extends InterpolationT {

    @tailrec
    private
    def neville(v: Double, x: Vector[Double], f: Vector[Double]): Double = {
      val l      = f.zip(f tail)
      val offset = x.length - l.length
      val xp     = Range(0,l.length).map((index)=> (x(index), x(index + offset)))
      val k      = l.zip(xp)
      val newl = for (((f1, f2), (x1, x2)) <- k) yield {
        (1.0 / (x1 - x2)) * ((v - x2) * f1 - (v - x1) * f2)
      }

      newl match {
        case Vector(x) => x
        case _ => neville(v, x, newl)
      }
    }

    def apply(x: Double): Option[Double] = {
      if (x < from || x > to) None else (xvals, yvals) match {
        case (Vector(x), Vector(y)) => Some(y)
        case _ => Some(neville(x,xvals,yvals))
      }
    }
  }

  override def strategy(indep: Vector[Double], data: Vector[Double]): Option[Vector[InterpolationT]] = {

    val number_of_data_points = if (indep.length > Degree) Degree + 1 else indep.length
    val lower_half =  (number_of_data_points - 1)/2
    val upper_half =  number_of_data_points - lower_half - 1

    val v = (for (index <- Range(0, data.length-1)) yield {

      val l1   = index - lower_half
      val fromt = if (l1 < 0) 0 else l1
      val off1 = if (l1 < 0) -l1 else 0

      val h1    = index + upper_half + off1
      val to    = if (h1 < data.length) h1 else (data.length - 1)
      val off2  = if (h1 < data.length) 0 else (h1 - (data.length - 1))
      val from  = if ((fromt - off2) > 0) (fromt - off2) else 0

      LagrangeNevilleInterpolation(indep(index), indep(index + 1), indep.slice(from, to+1),  data.slice(from, to+1))
    } ).toVector
    Some(v)
  }

  override
  def strategyName = strategyClassName(this)
}
