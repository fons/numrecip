/*
 * Copyright (c) 2014.
 *
 * This file ToFile.scala is part of numrecip (numrecip)
 *
 *     numrecip / ToFile.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / ToFile.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.util.io

import java.io._
import scala.util.control.NonFatal

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 5/10/14
 * Time: 7:31 PM
 * To change this template use File | Settings | File Templates.
 */
case class ToFile(val fileName: String) {
  val writer: Option[PrintWriter] = {
    try {
      Some(new PrintWriter(new File(fileName)))
    }
    catch {
      case NonFatal(t) => None
    }
  }

  def apply(v1: String) = {
    writer match {
      case Some(wr) => {
        wr.println(v1)
        wr.flush()
        writer
      }
      case _ => None
    }
  }
}
