/*
 * Copyright (c) 2014.
 *
 * This file Polynomial.scala is part of numrecip (numrecip)
 *
 *     numrecip / Polynomial.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / Polynomial.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.func

import scala.annotation.tailrec
import com.github.fons.nr.diff.DiffFunc

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 2/27/14
 * Time: 6:31 PM
 * To change this template use File | Settings | File Templates.
 */

object Polynomial {

  def apply(coeffs: Map[Int, Double]): Polynomial = {
    Polynomial(coeffs, (x: Double) => x)
  }

  def apply(coeffs: Map[Int, Double], f: (Double) => Double) = {
    val maxkey = coeffs.keys.max
    val list = (for (index <- Range(0, maxkey + 1)) yield {
      coeffs.get(index) match {
        case None => 0
        case Some(c) => c
      }
    }).toList.reverse
    new Polynomial(list, f)
  }

  def apply(coeff: List[Double], f: (Double) => Double = (x: Double) => x) = new Polynomial(coeff, f)
}

/*
  coeff is a list of coefficients, starting with the one of the highedt order :
  [a,b,c] <=> a x^2 + b^x + c

 */

class Polynomial(val coeff: List[Double], f: (Double) => Double = (x: Double) => x) extends Function1[Double, Option[Double]] with ConvolutionT {

  @tailrec
  private
  def collect(n: Int, start: Int, max: Int, accum: List[(Int, Int)]): List[(Int, Int)] = {
    val r = n - start
    if (r > max) accum else collect(n, start - 1, max, (start, r) :: accum)
  }

  def multiply(lhs: Vector[Double], rhs: Vector[Double]) = {
    val lhs_order = lhs.length - 1
    val rhs_order = rhs.length - 1
    val n = lhs_order + rhs_order
    for (k <- Range(0, n + 1)) yield {
      val start = if (k < lhs_order) k else lhs_order
      val max = if (k < rhs_order) k else rhs_order
      collect(k, start, max, List()).map {
        case (x, y) => lhs(x) * rhs(y)
      }.reduce(_ + _)
    }
  }

  @tailrec
  private def horner(x: Double, coeff: List[Double], accum: Double): Double = {
    coeff match {
      case List() => accum
      case c :: cs => horner(x, cs, (accum * f(x) + c))
    }
  }

  private
  def padding(n: Int) = Range(0, n).toList.map((x) => 0.0)

  private
  def combine_coeff(lhs: List[Double], rhs: List[Double]) = {
    val diff = lhs.length - rhs.length
    val pad = padding(scala.math.abs(diff))
    if (diff > 0) lhs.zip(pad ++ rhs) else (pad ++ lhs).zip(rhs)
  }

  private
  def product(lhs: List[Double], rhs: List[Double]) = {
    <|*|>(lhs, rhs) match {
      case Some(list) => list
      case None => List()
    }
  }

  private
  def divide(lhs: List[Double], rhs: List[Double]) = {
    <|/|>(lhs, rhs) match {
      case Some(list) => list
      case None => List()
    }
  }

  protected
  def className[A](a: A)(implicit m: Manifest[A]) = m.toString

  private
  def power(n: Int, x: Double) = {
    n match {
      case 0 => 1.0
      case 1 => x
      case p => {
        p - 2 * (p / 2) match {
          case 0 => poweralt(p, x)
          case 1 => x * poweralt(p - 1, x)
        }

      }
    }
  }

  @tailrec
  private
  def poweralt(n: Int, x: Double): Double = {
    n match {
      case 1 => x
      case m => poweralt(n / 2, x * x)
    }
  }

  private
  def powerfn(n: Int, f: (Double) => Double): (Double) => Double = {
    n match {
      case 0 => (x: Double) => 1.0
      case 1 => f
      case _ => (x: Double) => power(n, f(x))
    }
  }

  def apply(x: Double): Option[Double] = {
    coeff match {
      case List() => None
      case List(c) => Some(c)
      case c :: cs => Some(horner(x, cs, c))
    }
  }

  def order() = coeff.length - 1

  def ^(n: Int): Polynomial = pow(n)

  def pow(n: Int) = {
    n match {
      case 0 => Polynomial(List(1.0))
      case 1 => this
      case _ => Polynomial(coeff, powerfn(n, f))

    }
  }

  def +(other: Polynomial): Polynomial = Polynomial(combine_coeff(coeff, other.coeff).map((xy) => xy._1 + xy._2), f)

  def -(other: Polynomial): Polynomial = Polynomial(combine_coeff(coeff, other.coeff).map((xy) => xy._1 - xy._2), f)

  /*
   * order preserving multiplication. Basically each power series resembles a power series.
   */

  def <*>(other: Polynomial): Polynomial = {
    if (order == other.order()) Polynomial(product(coeff, other.coeff), f)
    else throw new IllegalArgumentException("only polynomials of the same order can be multiplied")
  }

  def *(other: Polynomial): Polynomial = {
    //if (order == other.order())
    Polynomial(multiply(coeff.toVector, other.coeff.toVector).toList, f)
    //else throw new IllegalArgumentException("only polynomials of the same order can be multiplied")
  }


  /*
  * Order preserving division
  */
  def </>(other: Polynomial): Polynomial = {
    if (order == other.order()) Polynomial(divide(coeff, other.coeff), f)
    else throw new IllegalArgumentException("only polynomials of the same order can be divided")
  }

  // The derivative may not be a polynomial. It's the product of the derivative of f at x times the derivative of the polynomial.
  def deriv = {
    order() match {
      case 0 => new Polynomial(List(0.0), (x) => 0.0)
      case n if n > 0 => {
        val dl = Range(order(), 0, -1).zip(coeff.dropRight(1)).toList.map((xy) => xy._1 * xy._2)
        val dfdx = DiffFunc(f, 0.1)
        new Function1[Double, Option[Double]] {
          def apply(x: Double) = {
            val poly = new Polynomial(dl, f)
            (dfdx(x)(), poly(x)) match {
              case (Some(u), Some(v)) => Some(u * v)
              case _ => None
            }
          }
        }
      }
    }
  }

  def ∂ = deriv


}


