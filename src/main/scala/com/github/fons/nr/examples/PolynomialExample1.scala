package com.github.fons.nr.examples

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 2/27/14
 * Time: 6:34 PM
 * To change this template use File | Settings | File Templates.
 */


import com.github.fons.nr.function.Polynomial

object PolynomialExample1 {
  def run {
    val cp = Polynomial(List(1.0, 1.0, 1.0, 1.0))
    val r = cp(2)
    println("r => ", r)

    val cp1 = Polynomial(List(1.0, 1.0, 1.0, 1.0), (x: Double) => x - 2.5)
    val r1 = cp1(2)
    println("r1 => ", r1)

    val cp2 = Polynomial(Map((0 -> 0.0), (4 -> 1.0)), (x: Double) => x)
    val r2 = cp2(2)
    println("r2 => ", r2)
  }
}
