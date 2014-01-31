/*
 * Copyright (c) 2014.
 *
 * This file BulirschStoerNevilleStrategy.scala is part of numrecip (numrecip)
 *
 *     numrecip / BulirschStoerNevilleStrategy.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / BulirschStoerNevilleStrategy.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.interpolation

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/27/14
 * Time: 4:18 PM
 * To change this template use File | Settings | File Templates.
 */

trait BulirschStoerNevilleStrategy extends StrategyT[InterpolationT] with Degree {

  private
  case class BulirschStoerNevilleInterpolation(from: Double, to: Double, xvals: Vector[Double], yvals: Vector[Double]) extends InterpolationT {

    private
    val sy = Range(0, xvals.length).map((x: Int) => TreeSet(x)).toList.zip(yvals).toMap

    private
    val paths_p = path(Range(0, xvals.length).map((x: Int) => TreeSet(x)).toList, List())

    @tailrec
    private
    def path(start: List[TreeSet[Int]], accum: List[List[TreeSet[Int]]]): List[List[TreeSet[Int]]] = {
      val next: List[TreeSet[Int]] = start.zip(start.tail).map((pair: (TreeSet[Int], TreeSet[Int])) => pair._1 ++ pair._2)
      next match {
        case List(x) => (next :: accum).reverse
        case _ => path(next, (next :: accum))
      }
    }

    @tailrec
    private
    def bulirschstoer(xvalue: Double, xvals: Vector[Double], yvals: Map[TreeSet[Int], Double], paths: List[List[TreeSet[Int]]]): Option[Double] = {
      val path = paths.head
      val v = for (current_index <- path) yield {
        val fd = (xvalue - xvals(current_index.head)) / (xvalue - xvals(current_index.last))
        //split key (1,2,3,4) to  (1,2,3) and (2,3,4) which are the previous iterants
        val lower = current_index.init
        val higher = current_index.tail
        //TODO : should match on Set ?
        val lb = higher.toList match {
          case List(_i_) => higher ++ Set(_i_ - 1)
          case _ => higher.init ++ Set(higher.last - 1)
        }
        val diff = yvals(higher) - yvals(lower)
        val lbv: Double = yvals.getOrElse(lb, 0.0)
        val diff2 = yvals(higher) - lbv
        val denom = fd * (1.0 - diff / diff2) - 1
        val result = yvals(higher) + diff / denom
        (current_index -> result)
      }

      paths match {
        case List(el) => v match {
          case List((k, v)) => Some(v)
          case _ => None
        }
        case _ => bulirschstoer(xvalue, xvals, yvals ++ v, paths.tail)
      }
    }

    /////////////////////////////////////////////////
    def apply(x: Double): Option[Double] = {
      if (x < from || x > to) None
      else (xvals, yvals) match {
        case (Vector(x), Vector(y)) => Some(y)
        case _ => bulirschstoer(x, xvals, sy, paths_p)
      }
    }
  }


  override
  def strategy(indep: Vector[Double], data: Vector[Double]): Option[Vector[InterpolationT]] = {

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

      BulirschStoerNevilleInterpolation(indep(index), indep(index + 1), indep.slice(from, to+1),  data.slice(from, to+1))
    } ).toVector

    Some(v)
  }

  override
  def strategyName = strategyClassName(this)
}
