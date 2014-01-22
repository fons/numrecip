/*
 * Copyright (c) 2014.
 *
 * This file LinearSystemsSolverT.scala is part of numrecip (numrecip)
 *
 *     numrecip / LinearSystemsSolverT.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / LinearSystemsSolverT.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.matrix

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/6/13
 * Time: 4:48 PM
 * To change this template use File | Settings | File Templates.
 */


//abstract class
trait LinearSystemsSolverT {
  protected def thisName[A](a: A)(implicit m: Manifest[A]) = m.toString

  def apply(m: Matrix, c: Matrix): Option[Matrix] = None

  def solverName = thisName(this)

}
