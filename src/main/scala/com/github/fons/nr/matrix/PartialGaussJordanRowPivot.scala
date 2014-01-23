/*
 * Copyright (c) 2014.
 *
 * This file PartialGaussJordanRowPivot.scala is part of numrecip (numrecip)
 *
 *     numrecip / PartialGaussJordanRowPivot.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / PartialGaussJordanRowPivot.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.matrix

//import com.mhsw.com.github.fons.nr.matrix.{LinearSystemsSolverT, BackwardSubstitution, RowPivot, Matrix}

import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/24/13
 * Time: 8:59 AM
 * To change this template use File | Settings | File Templates.
 */

case object PartialGaussJordanRowPivot extends LinearSystemsSolverT with RowPivot {

  @tailrec
  private def gaussJordan(cc: Int, M: Matrix, C: Matrix): Option[(Matrix, Matrix)] = {

    def subtract(row: Vector[Double])(r: Int, c: Int, x: Double): Double = if (r > cc) (x - row(c)) else x

    // partial pivoting

    val Pivot = pivot(cc, M)
    val Mr = Pivot flatMap (_(M))
    val Cr = Pivot flatMap (_(C))
    //---------end of pivoting---------------------

    //scale the values in each row with the current column value in that row.
    val c0 = Mr.flatMap(_.coll(cc))
    val Ms = c0 match {
      case None => None
      case Some(v) => Mr flatMap (_.flatMap((r: Int, c: Int, x: Double) => (if (r < cc || c < cc) x else x / (1.0 * v(r)))))
    }
    val Cs = c0 match {
      case None => None
      case Some(v) => Cr flatMap (_.flatMap((r: Int, c: Int, x: Double) => (if (r < cc) x else x / (1.0 * v(r)))))
    }

    //  get the first row and subtract from the other rows

    val Mp = Ms.flatMap(_.row(cc)) match {
      case None => None
      case Some(r) => Ms flatMap (_ flatMap (subtract(r) _))
    }

    val Cp = Cs.flatMap(_.row(cc)) match {
      case None => None
      case Some(r) => Cs flatMap (_ flatMap (subtract(r) _))
    }

    (Mp, Cp) match {
      case (Some(m1), Some(c1)) => {
        //val s   = rot +: accum
        val ncc = cc + 1
        if (m1.colls == ncc) Some(m1, c1) else gaussJordan(ncc, m1, c1)
      }
      case _ => None
    }
  }

  override def apply(m: Matrix, c: Matrix): Option[Matrix] = {
    gaussJordan(0, m, c) match {
      case None => None
      case Some((_m_, _c_)) => BackwardSubstitution(_m_, _c_)
    }
  }

  override lazy val toString = productPrefix + " using " + pivotName
}

trait PartialGaussJordanRowPivot extends LinearSystemsSolverT {
  override def apply(m: Matrix, c: Matrix): Option[Matrix] = PartialGaussJordanRowPivot(m, c)
  override
  def solverName = thisName(this)
}