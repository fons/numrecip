package com.github.fons.nr.ode

import scala.annotation.tailrec
import com.github.fons.nr.ode.butcher.tableau.ButcherTableauT

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/6/13
 * Time: 6:42 PM
 * To change this template use File | Settings | File Templates.
 */

import scala.util.{Try, Success, Failure}

trait ExplicitRungeKutta extends OdeStepWithErrorEstimateT with ButcherTableauT {

  // zipping with an empty/nill list is not recommended
  private def zipadd(R: List[Double], Accum: List[Double]) = {
    if (Accum == Nil) R else (Accum, R).zipped map (_ + _)
  }

  // the accumulator keeps a sum of bij*kij ; basically a vector of all reduced k's..
  @tailrec private def mult(step: Double, Klist: List[List[Double]], factors: List[Double], Accum: List[Double]): List[Double] = {
    if (Klist == Nil) Accum
    else {
      val R = (Klist head) map (x => (factors head) * step * x)
      val N = zipadd(R, Accum)
      mult(step, Klist tail, factors tail, N)
    }
  }

  @tailrec private def Kcalc(Args: (Double, List[Double]),
                             step: Double,
                             Func: List[(Double, Double *) => Double],
                             alfa: List[Double],
                             beta: List[List[Double]],
                             Accum: List[List[Double]]): List[List[Double]] = {
    if ((beta == Nil) || (beta isEmpty)) Accum
    else {
      val (t, args) = Args
      val time_step = t + (alfa head) * step
      val factors = (beta head) //reverse
      val L = mult(step, Accum, factors, Nil)
      // x_i + beta_i0*h*k0_i etc: new x values
      val arg_step = (args, L).zipped map (_ + _)
      val Kn = Func map (_(time_step, arg_step: _*))
      Kcalc(Args, step, Func, (alfa tail), (beta tail), (Kn :: Accum))
    }
  }

  private def next_step(step: Double, Args: (Double, List[Double]),
                        Func: List[(Double, Double *) => Double]): ((Double, List[Double]), List[List[Double]]) = {
    // K is a list of list of K coefficients, each seperate list represens a step in the RK scheme
    val K = Kcalc(Args, step, Func, alfa() tail, beta() tail, List(Func.map(_(Args._1, Args._2: _*))))
    // The coeff list has the factors which are applied to the elements of each step in the scheme
    // Zip the coeff with the K list=> tuple of coeff plus list.
    // Then apply the coeff to each element of the list
    val apply_coeff = (kappa(), K).zipped map ((coeff, list) => list.map(x => coeff * x))
    // now reduce the list of lists by adding each corresponding item in the list
    // k = k1 + k2 + ....
    val results = apply_coeff reduceLeft ((x, y) => (x, y).zipped map ((u, v) => u + v))
    // xi = xi-1 + step * ki
    val next = (Args._2, results).zipped map ((arg0, k) => arg0 + step * k)
    ((Args._1 + step, next), K)
  }


  private def error(step: Double, Klist: List[List[Double]], err: List[Double]): Option[List[Double]] = {
    val apply_coeff = (err, Klist).zipped map ((coeff, list) => list.map(x => coeff * x * step))
    val results = apply_coeff reduceLeft ((x, y) => (x, y).zipped map ((u, v) => (u + v)))
    Some(results)
  }

  override def next(step: Double, Args: (Double, List[Double]),
                    Func: List[(Double, Double *) => Double]): Try[(Double, List[Double])] = {
    next_step(step, Args, Func) match {
      case (res, klist) => Success(res)
    }
  }

  override def next_with_error(step: Double, Args: (Double, List[Double]),
                               Func: List[(Double, Double *) => Double]): (Try[(Double, List[Double])], Option[List[Double]]) = {
    val (res, klist) = next_step(step, Args, Func)
    //println("next_with_error : " + err())
    err() match {
      case Some(errdiff) => (Success(res), error(step, klist, errdiff))
      case None => (Success(res), None)
    }
  }

  override protected def className[A](a: A)(implicit m: Manifest[A]) = super[OdeStepWithErrorEstimateT].className(a)

  override def stepperName = className(this) + " extends " + super.stepperName + " with " + tableauName
}