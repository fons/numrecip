package com.github.fons.nr.test

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/20/13
 * Time: 7:23 PM
 *
 * Driver to compare ode solvers to exact solution.
 * default constructor is private which 'forces' the 'var' solver to be set at all times.
 *
 *
 */

import scala.util.{Try, Success, Failure}
import com.github.fons.nr.ode.{Solver, OdeSolverT, Factory}


class OdeTest private(Exact: List[(Double) => Double]) {
  private var solver: Option[OdeSolverT] = None
  private var type_name: Option[String] = None

  def this(Exact: List[(Double) => Double], Type: Solver.Type, Step: Double, Init: (Double, List[Double]), Func: List[(Double, Double *) => Double]) {
    this(Exact)
    solver = Factory(Type, Step, Init, Func) match {
      case Success(ode) => Some(ode)
      case Failure(i) => None
    }
    type_name = Some(Type.toString)
  }


  def this(Exact: List[(Double) => Double], Type: Solver.Type, Step: Double, Init: (Double, List[Double]), Func: List[(Double, Double *) => Double], Acc: Double) {
    this(Exact)

    solver = Factory(Type, Step, Init, Func, Acc) match {
      case Success(ode) => Some(ode)
      case Failure(i) => None
    }
    type_name = Some(Type.toString)
  }


  private def comp(left: (Double, List[Double]), right: (Double, List[Double])) = {
    val (lt, left_list) = left
    val (rt, right_list) = right
    val index = Range(0, left_list length)
    //toList seems to be rquired here...
    val d = ((index, left_list, right_list).zipped map ((x: Int, y: Double, z: Double) => OdeTestRunDiff("dependend [" + x.toString + "]", y, z))).toList
    val results = OdeTestRunDiff("independent", lt, rt) :: d
    results
  }

  private def exact(target: Double) = {
    (target, Exact map (_(target)))
  }

  def run(target: Double): Try[OdeTestResult] = {

    solver match {
      case Some(odesolver) => {
        val reslt = odesolver(target)
        reslt match {
          case Success((res, _)) => Success(OdeTestResult(type_name + ":" + solver.toString, comp(res, exact(target))))
          case Failure(f) => Failure(f)
        }
      }
      case None => Failure(new RuntimeException("No odeslover found for type " + type_name))
    }
  }

  override lazy val toString = "OdeTestDriver (" + type_name + ") {" + solver.toString + "}"

}
