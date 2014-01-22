/*
 * Copyright (c) 2014.
 *
 * This file ParabolicallyTerminatedSpline.scala is part of numrecip (numrecip)
 *
 *     numrecip / ParabolicallyTerminatedSpline.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / ParabolicallyTerminatedSpline.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.interpolation

import com.github.fons.nr.matrix.{LinearSystemsSolverT, Matrix}

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/22/14
 * Time: 12:54 PM
 * To change this template use File | Settings | File Templates.
 */

trait ParabolicallyTerminatedSpline extends SplineStrategyT with LinearSystemsSolverT {

  private
  def end_points(v : Vector[Double]) : Option[Vector[Double]] = {
    val last = v.length - 1
    val m0 = v(0)
    val mN = v(last)
    Some(m0 +: v :+ mN)
  }


  override
  def strategy(indep: Vector[Double], data: Vector[Double]): Option[Vector[Double]] = {
    lazy val range_x = indep.zip(indep tail)
    lazy val delta_x = range_x.map(xy => xy._2 - xy._1)
    lazy val delta_y = data.zip(data tail).map(xy => xy._2 - xy._1)
    lazy val rat_yx = delta_y.zip(delta_x).map((x) => x._1 / x._2)

    val C = Matrix(rat_yx.zip(rat_yx.tail).map((x) => List(6.0 * (x._2 - x._1))).toList)
    val dim = (indep.length - 2)
    //TODO : This can be optimized as this is a band matrix
    val main = (for (n <- Range(0, dim);
                     m <- Range(0, dim)) yield {
      (n, m) match {
        case (k, l) if (k == l + 1) => ((n, m) -> delta_x(l + 1))
        case (k, l) if l == k => ((n, m) -> 2.0 * (delta_x(l) + delta_x(l + 1)))
        case (k, l) if (k + 1 == l) => ((n, m) -> delta_x(l))
        case _ => ((n, m) -> 0.0)
      }

      //    }
    }).toMap

    val n00   = 3.0*delta_x(0) + 2.0* delta_x(1)
    val n01   = delta_x(1)
    val n2N   = delta_x(dim-1)
    val n1N   = 2.0 * delta_x(dim - 1) + 3.0 * delta_x(dim)
    val nmain = main + ((0,0)->n00, (0,1)->n01, (dim-1, dim-2)->n2N, (dim-1,dim-1)->n1N)
    val M     = new Matrix(nmain, dim, dim)

    apply(M, C).map(_.values((x, y) => true)).flatMap(end_points(_))

  }

}
