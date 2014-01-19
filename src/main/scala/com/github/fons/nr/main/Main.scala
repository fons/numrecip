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


import com.github.fons.nr.matrix._

trait InterpolatorT {
  def apply(x: Double): Option[Vector[Option[Double]]]
}

abstract class InterpolationT {
  def apply(x: Double): Option[Double]
}

case class CubicSpline(from: Double, to: Double, s1: Double, s2: Double, s3: Double, y: Double) extends InterpolationT {
  private def S(w: Double) = Some(((s3 * w + s2) * w + s1) * w + y)

  def apply(x: Double): Option[Double] = {
    if (x < from || x > to) None else S((x - from))
  }
}

//TODO : add more constructors to make instantiation easier..
//TODO : have a spline strategy (for natural spline, clamped etc...
//TODO : Interpolator uses InterpolationsT !!

import scala.annotation.tailrec

case class Interpolator(DataSet: (Vector[Double], Vector[Vector[Double]])) extends InterpolatorT with LinearSystemsSolverT {
  private val (min, max, data_size) = {
    val (indep, _) = DataSet
    (indep(0), indep(indep.length - 1), indep.length)
  }

  @tailrec
  private def re_order(l: Vector[Vector[Option[InterpolationT]]], accum: Vector[Vector[Option[InterpolationT]]]): Vector[Vector[Option[InterpolationT]]] = {
    l match {
      case Vector() => accum
      case xs +: xss => re_order(xss, accum.zip(xs).map((xy) => xy._1 :+ xy._2))
    }
  }

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

  private val spline: Vector[(Double, Vector[Option[InterpolationT]])] = {
    val (xvals, yvals) = DataSet
    val splines = for (y <- yvals) yield initialize(xvals, y)
    val b = re_order(splines.tail, splines.head.map(Vector(_)))
    xvals.zip(b)
  }

  private def initialize(indep: Vector[Double], data: Vector[Double]): Vector[Option[InterpolationT]] = {

    lazy val range_x = indep.zip(indep tail)
    lazy val delta_x = range_x.map(xy => xy._2 - xy._1)
    lazy val delta_y = data.zip(data tail).map(xy => xy._2 - xy._1)
    lazy val rat_yx = delta_y.zip(delta_x).map((x) => x._1 / x._2)

    val C = Matrix(rat_yx.zip(rat_yx.tail).map((x) => List(6.0 * (x._2 - x._1))).toList)
    val dim = (indep.length - 2)
    //TODO : This can be optimzed as this is a band matrix
    val main = (for (n <- Range(0, dim); m <- Range(0, dim)) yield {
      (n, m) match {
        case (k, l) if (k == l + 1) => ((n, m) -> delta_x(l + 1))
        case (k, l) if l == k => ((n, m) -> 2.0 * (delta_x(l) + delta_x(l + 1)))
        case (k, l) if (k + 1 == l) => ((n, m) -> delta_x(l))
        case _ => ((n, m) -> 0.0)
      }

      //    }
    }).toMap
    val M = new Matrix(main, dim, dim)
    val Mm = apply(M, C).map(_ :|>:+ Matrix(List(List(0.0))) :|<:+ Matrix(List(List(0.0)))).map(_ values ((x, y) => true))

    Mm match {
      case Some(m) => (for (index <- Range(0, indep.length - 1)) yield {
        val s1 = rat_yx(index) - delta_x(index) * (2.0 * m(index) + m(index + 1)) / 6.0
        val s2 = m(index) * .5
        val s3 = (m(index + 1) - m(index)) / (6.0 * delta_x(index))
        val spl = CubicSpline(indep(index), indep(index + 1), s1, s2, s3, data(index))
        Some(spl)
      }).toVector
      case _ => Vector(None)
    }

  }

  def apply(x: Double): Option[Vector[Option[Double]]] = {
    val Iidx = find_close(data_size, x, DataSet._1, (0, data_size))
    val Sspline_pair = Iidx.map(spline(_))
    //==>Some[Vector[Some[Spline]]]
    val SVSspl = Sspline_pair.map((xy) => xy._2)
    SVSspl.map(_.map(_.flatMap(_(x))))
  }
}

object Main extends App {


  def reduce1(v: Double, x: Vector[Double], f: Vector[Double]): Vector[Double] = {
    val l = x.zip(f)
    val k = l.zip(l tail)
    val newl = for (((x1, f1), (x2, f2)) <- k) yield {
      1.0 / (x1 - x2) * ((v - x2) * f2 - (v - x1) * f1)
    }
    val newx = x.head +: (x.tail.tail)

    newl match {
      case Vector() => newl
      case _ => reduce1(v, newx, newl)
    }
  }

  val ll = Vector(0.0, 1.0, 2.0, 3.0)
  val vs = Vector(0.0, 0.5, 2.0, 1.5)
  val cs = new Interpolator((ll, Vector(vs))) with PartialGaussJordanRowPivot
  cs(2.1).map(_.map(_.map(println _)))
}
