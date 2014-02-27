/*
 * Copyright (c) 2014.
 *
 * This file DiffFunc.scala is part of numrecip (numrecip)
 *
 *     numrecip / DiffFunc.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / DiffFunc.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.diff

import scala.annotation.tailrec
import com.github.fons.nr.util.Accuracy

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 2/21/14
 * Time: 7:04 PM
 * To change this template use File | Settings | File Templates.
 */
case class DiffFunc(func:(Double)=>Double, stepsize:Double,  val accuracy:Accuracy = Accuracy(), maxIter:Int = 5) {
  @tailrec
  private
  def def2(p: Int, y : Int, accum:Map[(Int,Int), Double]) : Map[(Int,Int),Double] = if (y > p) accum else {
    val k1 = accum((p, y - 1))
    val k2 = accum((p - 1 , y - 1))
    val c  = scala.math.pow(4.0,y) - 1.0
    val newaccum = accum ++ Map((p,y) -> (k1 + (k1 - k2) / c))
    def2(p, y+1, newaccum)
  }

  def error(p: Int, m : Map[(Int,Int), Double]): Option[Double] = if (p < 2) None else Some(scala.math.abs(m(p,p) - m(p-1,p-1)))

  @tailrec
  private
  def diff(x:Double, f:(Double)=>Double, h:Double, p: Int, n : Int, accum: Map[(Int,Int), Double]):(Int, Map[(Int,Int),Double]) = {
    val newacc = accum ++ Map(((p,1) -> ((f(x+h) - f(x-h)) / (2.0 * h))))
    val lacc   = def2(p, 2, newacc)
    (p, error(p, lacc)) match {
      case (l, _) if (l == n) => (p, lacc)
      case (_, None) => diff(x,f,0.5*h,p+1, n, lacc)
      case (_, Some(rerr)) if rerr < accuracy.acc => (p,lacc)
      case _ => diff(x,f,0.5*h,p+1, n, lacc)
    }
  }
  def apply(x:Double) : DiffResult = {
    val(index, result) = diff(x, func, stepsize, 1, maxIter, Map[(Int,Int), Double]())
    DiffResult(index,result)
  }
}
