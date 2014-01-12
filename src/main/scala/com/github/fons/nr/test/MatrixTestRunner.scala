package com.github.fons.nr.test

import com.github.fons.nr.matrix.{Matrix, Solver}

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/10/14
 * Time: 8:39 PM
 * To change this template use File | Settings | File Templates.
 */
case object MatrixTestRunner {

  private def gen_tests(solver: Solver.Value, dim: Int, order: Double, cases: Int) = {
    val L = for (i <- Range(0, cases)) yield {
      val eint = scala.math.pow(10.0, order)
      val M0 = Matrix(dim, dim, (x: Int, Int) => util.Random.nextDouble() * util.Random.nextInt(eint.toInt) + 0.0000000001)
      val R0 = Matrix(dim, 1, (x: Int, Int) => util.Random.nextDouble() * util.Random.nextInt(eint.toInt))
      MatrixTest(solver, R0, M0)
    }
    println("running test for " + solver + "  dim : " + dim + " order : " + order + " cases : " + cases)
    val R = L.map(_.run())
    val rt = R.foldLeft(0.0)((agg: Double, el: MatrixTestResult) => agg + el.runtime)
    val art = rt / R.length
    val failed = R.foldLeft(0)((agg: Int, el: MatrixTestResult) => agg + (if (el.failed) 1 else 0))
    for (result <- R) {
      //println(result)
    }
    print("total : " + R.length + "  average run time : " + art + " ")
    println(" total run time : " + rt + "   failures : " + failed)
  }


  private def is_iterative(solver: Solver.Value) = {
    solver match {
      case (Solver.IterativeGaussSeidel) => true
      case (Solver.IterativeJacobi) => true
      case _ => false
    }
  }

  def run() {
    Solver.values.filter(!is_iterative(_)).map(gen_tests(_, 50, 4, 50))
    Solver.values.filter(!is_iterative(_)).map(gen_tests(_, 100, 4, 50))
    Solver.values.filter(!is_iterative(_)).map(gen_tests(_, 150, 4, 50))
  }

}
