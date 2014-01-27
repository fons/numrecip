/*
 * Copyright (c) 2014.
 *
 * This file LagrangeNeville.scala is part of numrecip (numrecip)
 *
 *     numrecip / LagrangeNeville.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / LagrangeNeville.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.interpolation

import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/25/14
 * Time: 6:36 PM
 * To change this template use File | Settings | File Templates.
 */
trait LagrangeNeville extends InterpolatorT with StrategyT[(Int,Int, Int)] with Degree {


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

  private
  def initialize_helper(indep: Vector[Double], data: Vector[Double]): Option[Vector[InterpolationT]] = {
    //TODO : scanning the data TWICE; first for the strategy; then to construct the interploators
    strategy(indep, data) match {
      case None => None
      case Some(vect) => Some(for ((index, from,to) <- vect) yield LagrangeNevilleInterpolation(indep(index), indep(index + 1),
                                                                   indep.slice(from, to+1),  data.slice(from, to+1)))
      }
    }
  override
  protected
  def initialize(dataSet: DataSet): Option[Vector[InterpolationSet]] = Some(for (y <- dataSet.dependend) yield InterpolationSet(initialize_helper(dataSet.independend, y)))

  override
  def interpolatorName: String = strategyClassName(this) + " using strategy : " + strategyName + " with Degree " + Degree
}
