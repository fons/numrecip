/*
 * Copyright (c) 2014. 
 *
 * This file LifterOperation.scala is part of numrecip (numrecip) 
 *
 *     numrecip / LifterOperation.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / LifterOperation.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.util

import scala.util.control.NonFatal

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 4/24/14
 * Time: 6:58 PM
 * To change this template use File | Settings | File Templates.
 */
object LiftedOperation {
  def apply(op: (Double, Double) => Double): (Option[Double], Option[Double]) => Option[Double] = {
    (x: Option[Double], y: Option[Double]) => {
      (x, y) match {
        case (Some(v1), Some(v2)) => {
          try {
            Some(op(v1, v2))
          } catch {
            case NonFatal(e) => None
          }
        }
        case _ => None
      }
    }
  }
}

