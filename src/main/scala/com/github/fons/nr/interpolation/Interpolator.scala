/*
 * Copyright (c) 2014.
 *
 * This file Interpolator.scala is part of numrecip (numrecip)
 *
 *     numrecip / Interpolator.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / Interpolator.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.interpolation

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/19/14
 * Time: 2:37 PM
 * To change this template use File | Settings | File Templates.
 */

//TODO : add more constructors to make instantiation easier..

//TODO : Interpolator uses InterpolationsT !!

import scala.annotation.tailrec
import com.github.fons.nr.matrix.{Matrix, LinearSystemsSolverT}

case class Interpolator(dataSet: DataSet) extends InterpolatorT  {


  @tailrec
  private
  def find_close(guard: Int, v: Double, l: Vector[Double], probe: (Int, Int)): Option[Int] = {
    val (from, to) = probe
    (((to + from) / 2), guard) match {
      case (_, n) if n < 1 => None
      case (mid, _) if (((mid == to) || (mid == from)) && ((to - from) == 1)) => Some(from)
      case (mid, _) if v > l(mid) => find_close(guard - 1, v, l, (mid, to))
      case (mid, _) if v < l(mid) => find_close(guard - 1, v, l, (from, mid))
      case (mid, _) if guard < 1 => None
    }
  }

  private
  val Sinterpolators: Option[Vector[InterpolationSet]] = initialize(dataSet)

  override
  def apply(x: Double): Option[Vector[Option[Double]]] = {
    val Iidx = find_close(dataSet.independend.length, x, dataSet.independend, (0, dataSet.independend.length-1))
    (Iidx, Sinterpolators) match {
      case (Some(idx), Some(inter_list)) => Some(for (inter <- inter_list) yield inter(idx).flatMap(_(x)))
      case _ => None
    }
  }
}

