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

import com.github.fons.nr.examples._
import com.github.fons.nr.interpolation._
import com.github.fons.nr.ode._
import scala.math._

import scala.util.{Success, Try}

import com.github.fons.nr.ode.butcher.tableau.RKE56Tableau


import scala.util.Success
import com.github.fons.nr.util.Accuracy
import com.github.fons.nr.interpolation.Interpolator
import scala.Some
import com.github.fons.nr.matrix.LUSolver


object Main extends App {
  //BulirschStoerExample1.run()
  //BulirschStoerExample2.run
//OdeExample1.run
  //OdeMemoizeBurlischStoer.run()
  //NevilleExample1.run
  OdeMemoizeExample.run
}
