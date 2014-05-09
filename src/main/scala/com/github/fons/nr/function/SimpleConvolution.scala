/*
 * Copyright (c) 2014.
 *
 * This file SimpleConvolution.scala is part of numrecip (numrecip)
 *
 *     numrecip / SimpleConvolution.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / SimpleConvolution.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.function

import com.github.fons.nr.matrix.Matrix
import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 3/4/14
 * Time: 7:07 PM
 * To change this template use File | Settings | File Templates.
 */

trait SimpleConvolution extends ConvolutionT {

  //TODO : zero checks need to be centralized
  private
  def zero(v: Double): Boolean = {
    v match {
      case 0.0 => true
      case n if n > 0.0 && n < 1.0e-12 => true
      case n if n < 0.0 && -n < 1.0e-12 => true
      case _ => false
    }
  }

  private
  def gen_rows(l: List[Double], pad: List[Double], accum: List[List[Double]]): List[List[Double]] = {
    l match {
      case List() => accum.reverse
      case x :: xs => {
        val n = x :: pad.dropRight(1)
        gen_rows(xs, n, n :: accum)
      }
    }
  }

  @tailrec
  private
  def gen_vec(c: List[Double], accum: List[List[Double]]): List[List[Double]] = {
    c match {
      case List() => accum.reverse
      case x :: xs => gen_vec(xs, List(x) :: accum)
    }
  }

  def prod(lhs: List[Double], rhs: List[Double]) = {
    lhs match {
      case List() => 0.0
      case _ => lhs.zip(rhs).map((xy) => xy._1 * xy._2).reduce(_ + _)
    }

  }

  @tailrec
  private
  def divide(lhs: List[Double], rhs: List[Double], accum: List[Double]): List[Double] = {
    lhs match {
      case List() => accum.reverse
      case u :: us => {
        val v0 = rhs.head
        val rn = (1.0 / v0) * (u - prod(accum, rhs.tail))
        divide(us, rhs, rn :: accum)
      }
    }


  }

  //TODO : <|*|> this can be faster
  override
  def <|*|>(lhs: List[Double], rhs: List[Double]): Option[List[Double]] = {
    val m = Matrix(gen_rows(lhs.reverse, List().padTo(lhs.length, 0.0), List()))
    val c = Matrix(gen_vec(rhs.reverse, List()))
    (m *> Some(c)).flatMap(_.coll(0)).map(_.toList.reverse)
  }

  override
  def <|/|>(lhs: List[Double], rhs: List[Double]): Option[List[Double]] = if (zero(rhs.head) == true) None
  else Some(divide(lhs.reverse, rhs.reverse, List()).reverse)

}
