/*
 * Copyright (c) 2014.
 *
 * This file PartialGaussJordanFullPivot.scala is part of numrecip (numrecip)
 *
 *     numrecip / PartialGaussJordanFullPivot.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / PartialGaussJordanFullPivot.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.matrix

import scala.annotation.tailrec
import scala.Some

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/9/13
 * Time: 5:19 PM
 *
 * Transforms a matrix to upper triangular form, using pivoting and row elimination.
 * Pivots are recomputed at each step.
 * A 'rotation' is recorded and stored at each step.
 *
 * The system is then solved using back substitution.
 *
 */
case object PartialGaussJordanFullPivot extends LinearSystemsSolverT with FullPivot {


  @tailrec
  private def gaussJordan(M: Matrix, C: Matrix, accum: Vector[(MatrixTransform, Vector[Double], Vector[Double])]): Option[Vector[(MatrixTransform, Vector[Double], Vector[Double])]] = {


    def subtract(row: Vector[Double])(r: Int, c: Int, x: Double): Double = if (r < 1) x else x - row(c)

    // step 1 : find the maximum value

    val Rot = pivot(0, M)
    //println(r, c, v)

    // create a rotation and rotate (pivot) the matrix

    val Mr = Rot flatMap (_(M))
    val Cr = Rot flatMap (_(C))
    //M.pp
    //Mr map ( _ pp)
    //Cr map ( _ pp)
    //println("-----------------------------------")

    //get the first column
    val c0 = Mr.flatMap(_.coll(0))
    //println(c0)
    //println("-----------------------------------")
    //scale each row with the value in the first column
    val Ms = c0 match {
      case None => None
      case Some(v) => Mr flatMap (_.flatMap((r: Int, c: Int, x: Double) => x / (1.0 * v(r))))
    }
    val Cs = c0 match {
      case None => None
      case Some(v) => Cr flatMap (_.flatMap((r: Int, c: Int, x: Double) => x / (1.0 * v(r))))
    }

    //Ms map (_ pp)
    //Cs map (_ pp)
    //println("-----------------------------------")
    //  get the first row and subtract from the other columns
    val r0 = Ms.flatMap(_.row(0))
    val Mp = r0 match {
      case None => None
      case Some(r) => Ms flatMap (_ flatMap (subtract(r) _))
    }
    val cx = Cs.flatMap(_.row(0))
    val Cp = cx match {
      case None => None
      case Some(r) => Cs flatMap (_ flatMap (subtract(r) _))
    }
    // println("-----------------final print------------------")
    //Mp map (_ pp)
    //Cp map (_ pp)
    //println("---------------done--------------------")
    val result = (Mp flatMap (_ cut(0, 0)), Cp flatMap (_.tail), Rot, Mp flatMap (_ row (0)), Cp flatMap (_ row (0)))
    result match {
      case (Some(m1), Some(c1), Some(rot), Some(rm), Some(rc)) => {
        val s = (rot, rm, rc) +: accum
        if (m1.size() == 0) Some(s) else gaussJordan(m1, c1, s)
      }
      case _ => None
    }
  }

  private def back_substitute(res: Vector[Double], st: (MatrixTransform, Vector[Double], Vector[Double])): Option[Vector[Double]] = {
    val (rot: Rotation, row: Vector[Double], coef: Vector[Double]) = st
    //val h = row.head //this value should always be 1
    val sumprod = (row tail, res).zipped.foldLeft(0.0)((x: Double, e: (Double, Double)) => x + (e._1 * e._2))
    coef headOption match {
      case None => None
      case Some(v) => {
        val nres = (v - sumprod) +: res
        rot(nres)
      }
    }
  }

  @tailrec
  private def unwind(stack: Vector[(MatrixTransform, Vector[Double], Vector[Double])], accum: Vector[Double]): Option[Vector[Double]] = {
    stack match {
      case Vector() => Some(accum)
      case (x +: xs) => {
        back_substitute(accum, x) match {
          case Some(accum) => unwind(xs, accum)
          case None => None
        }
      }
    }
  }

  override def apply(m: Matrix, c: Matrix): Option[Matrix] = {

    val result = gaussJordan(m, c, Vector())
    val p0 = result match {
      case Some(v) => unwind(v, Vector())
      case None => None
    }
    p0 match {
      case Some(v) => Some(Matrix(List(v.toList)).^)
      case _ => None
    }
  }

  override lazy val toString = productPrefix + " using " + pivotName

}


trait PartialGaussJordanFullPivot extends LinearSystemsSolverT {
  override def apply(m: Matrix, c: Matrix): Option[Matrix] = PartialGaussJordanFullPivot(m, c)
}