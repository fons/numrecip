package com.github.fons.nr.test

//function mohegan.numerics

import scala.math._

import com.github.fons.nr.ode.Solver

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/25/13
 * Time: 9:33 PM
 * To change this template use File | Settings | File Templates.
 */
object OdeTestRunner {

  def run_test(L1: List[OdeTest], to: Double) {
    for (l1 <- L1) {
      run_test(l1, to)
    }
  }

  def run_test(l1: OdeTest, to: Double) {
    println("\n--------------------------------------------------\n")
    println("running :", l1, " to : ", to)
    val r1 = l1.run(to)
    OdeTestResult.show(r1)
  }

  ////////////
  def test1(type_ : Solver.Value) = {
    /// ode to solve
    def f(t: Double, x: Double*): Double = -Pi * x(1)
    def g(t: Double, x: Double*): Double = Pi * x(0)
    //exact solution
    def fx(t: Double) = scala.math.cos(scala.math.Pi * t)
    def gx(t: Double) = scala.math.sin(scala.math.Pi * t)

    new OdeTest(List(fx, gx), type_, 0.2, (0.0, List(1.0, 0.0)), List(f, g), 0.0000001)

  }

  def test1a(type_ : Solver.Value) = {
    /// ode to solve
    def f(t: Double, x: Double*): Double = -Pi * x(1)
    def g(t: Double, x: Double*): Double = Pi * x(0)
    //exact solution
    def fx(t: Double) = scala.math.cos(scala.math.Pi * t)
    def gx(t: Double) = scala.math.sin(scala.math.Pi * t)

    new OdeTest(List(fx, gx), type_, 0.2, (0.0, List(1.0, 0.0)), List(f, g))

  }

  def test2(type_ : Solver.Value) = {

    def fn(t: Double, y: Double*): Double = 2.0 - scala.math.exp(-4.0 * t) - 2.0 * y(0)
    def fe(t: Double): Double = 1.0 + 0.5 * scala.math.exp(-4.0 * t) - 0.5 * scala.math.exp(-2 * t)

    new OdeTest(List(fe), type_, 0.1, (0.0, List(1.0)), List(fn), 0.000001)
  }

  def test3(type_ : Solver.Value) = {
    def fn(t: Double, y: Double*): Double = 1 + y(0) * y(0)
    def fe(t: Double): Double = scala.math.sin(t) / scala.math.cos(t)
    new OdeTest(List(fe), type_, 0.01, (0.0, List(0.0)), List(fn), 0.000001)
  }

  def test4(type_ : Solver.Value) = {
    def fn(t: Double, y: Double*): Double = y(0) - t * t + 1.0
    def fe(t: Double): Double = t * t + 2.0 * t + 1.0 - 0.50 * scala.math.exp(t)
    new OdeTest(List(fe), type_, 1, (0.0, List(0.5)), List(fn), 0.000001)
  }


  def run() {
    println("-----Test1-----------------------------------------------------")

    val L1 = Solver.values map (test1(_))
    run_test(L1.toList, 0.15)

    println("-----Test2------------------------------------------------------")

    val L2 = Solver.values map (test2(_))
    run_test(L2.toList, 0.15)

    println("-----Test3------------------------------------------------------")

    val L3 = Solver.values map (test3(_))
    run_test(L3.toList, 1.565)

    println("-----Test4------------------------------------------------------")
    val L4 = Solver.values map (test4(_))
    run_test(L4.toList, 5.3)

  }

}
