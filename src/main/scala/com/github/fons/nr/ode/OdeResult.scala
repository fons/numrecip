/*
 * Copyright (c) 2014.
 *
 * This file OdeResult.scala is part of numrecip (numrecip)
 *
 *     numrecip / OdeResult.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / OdeResult.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.ode

import com.github.fons.nr.interpolation.{InterpolatorT, DataSet}

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 2/5/14
 * Time: 3:51 PM
 * To change this template use File | Settings | File Templates.
 */


case class OdeResult(init : (Double, List[Double]), end : (Double, List[Double]), override val dataSet:DataSet, private val odestr : String) extends InterpolatorT {

  private def collect(agg: Option[List[Double]], value : Option[Double]):Option[List[Double]] = {
    (agg, value) match {
      case (Some(xs), Some(x)) => Some(x::xs)
      case _ => None
    }
  }

  def this(odeResult : OdeResult) = this(odeResult.init, odeResult.end, odeResult.dataSet, odeResult.odestr)

  def first     = init._1
  def last      = end._1
  def head      = init._2
  def tail      = end._2

  def apply() : Option[List[Double]] = if (end._2.length > 0) Some(end._2) else None

  def apply(v : Double) : Option[List[Double]] = {
    v match  {
      case init._1  => Some(head)
      case end._1   => Some(tail)
      case _ => interpolate(v) match {
        case None           => None
        case Some(Vector()) => None
        case Some(_vV_)     => _vV_.reverse.foldLeft(Option(List[Double]()))(collect)
      }
    }
  }
  override
  def toString = className(this) + " " + dataSet + " " + odestr
}