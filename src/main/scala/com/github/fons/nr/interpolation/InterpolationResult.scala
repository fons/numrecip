/*
 * Copyright (c) 2014.
 *
 * This file InterpolationResult.scala is part of numrecip (numrecip)
 *
 *     numrecip / InterpolationResult.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / InterpolationResult.scala is distributed in the hope that it will be useful,
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
 * Date: 2/5/14
 * Time: 9:23 PM
 * To change this template use File | Settings | File Templates.
 */
import scala.Option

case class InterpolationResult(val argument:Double, val result : Vector[Option[Double]], val interstr:String) {

  private def collect(agg: Option[Vector[Double]], value : Option[Double]):Option[Vector[Double]] = {
    (agg, value) match {
      case (Some(xs), Some(x)) => Some(xs:+x)
      case _ => None
    }
  }

  def error : Option[Double] = None

  def apply() :Option[List[Double]] = {
       result.foldLeft(Option(Vector[Double]()))(collect).map(_.toList) match {
         case Some(List()) => None
         case Some(_vV_)      => Some(_vV_)
         case None => None
       }
  }
  protected def className[A](a: A)(implicit m: Manifest[A]) = m.toString

  override
  lazy val toString = className(this) + "@"+hashCode()+ " for " + argument + " with  " + interstr
}
