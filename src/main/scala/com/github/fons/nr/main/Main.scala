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


import com.github.fons.nr.util._
import com.github.fons.nr.util.io.FileChannel
import com.github.fons.nr.function.Beta


case class IfThen(state: Boolean) {

}

//def convert[A <: EvalStrategyT, B >: EvalStrategyT](a: A): B = {
//a
//}


object Main extends App {
  def func1(x: Double, y: Double) = scala.math.cos(2 * x) * scala.math.sin(x * y)

  //def func1(x: Double) = scala.math.acos(x)
  def g(x: Double, y: Double) = {
    val z = Beta(x, y)
    if (scala.math.abs(z) > 1000) 0.0 else z
  }

  val n = UniformSampler()(Interval(-1, 1), 10)
  println(n)
  //val app2 = new Approximation with ChebPolyEvalStrategy with HeuristicSolver
  //val fa2  = app2(func1, Interval(-1,1), Some(513))

  //val y = Gamma(0.56)

  //println(y, scala.math.log(y), Gamma.ln(0.56))
  //LinearSpace(Interval(-6, 6), 5000, (x:Double)=>x).map{case(x,y)=> LinearSpace.ppData(",")(x,y)}.map{x => FileChannel("/tmp/test.txt")(x)}
  val b = RectangularSpace(Interval(-1, 1), 300, Interval(-1, 1), 300, Vector(func1))
  println(b.data.length)
  //println(b.data)
  //b.pp()
  b.map {
    case (x, y, xs) => RectangularSpace.ppData(",")(x, y, xs)
  }.map {
    x => FileChannel("/tmp/test.txt")(x)
  }

  //
  //  val z = BinomialCoeff(34, 20)
  //  println(z)
  //  println(fa2)
  //  println(fa2.map(_.coefficients))
  //  println(fa2.map(_(0.1234)))
  //  println(scala.math.acos(0.1234))
  //
  //  val der = fa2.flatMap(_.deriv)
  //  val inter = fa2.flatMap(_.inter(0))
  //
  //  vectorToOption(Vector(false)).map(LinearSpace(Interval(-1,1), 500, _)).map {
  //        case d => d.foreach((x, y) => FileChannel("/tmp/test.txt")(d.ppData(" ")(x, y)))
  //  }
  //println(l)
  //println(func1(1.0))

}

