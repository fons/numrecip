package com.github.fons.nr.interpolation

import com.github.fons.nr.matrix.{LinearSystemsSolverT, Matrix}

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/22/14
 * Time: 5:11 PM
 * To change this template use File | Settings | File Templates.
 */
trait CurvatureAdjustedSpline extends SplineStrategyT with LinearSystemsSolverT {

  val deriv2nd0 : Double
  val deriv2ndN : Double

  private
  def end_points(rat_yx : Vector[Double], delta_x : Vector[Double], v : Vector[Double]) : Option[Vector[Double]] = {
    val m0 = deriv2nd0
    val mN = deriv2ndN
    Some(m0 +: v :+ mN)
  }

  override
  def strategy(indep: Vector[Double], data: Vector[Double]): Option[Vector[Double]] = {
    lazy val range_x = indep.zip(indep tail)
    lazy val delta_x = range_x.map(xy => xy._2 - xy._1)
    lazy val delta_y = data.zip(data tail).map(xy => xy._2 - xy._1)
    lazy val rat_yx = delta_y.zip(delta_x).map((x) => x._1 / x._2)

    println(rat_yx)
    val nc   = rat_yx.zip(rat_yx.tail).map((x) => List(6.0 * (x._2 - x._1)))

    val c0   = nc(0)(0)
    val nc1  = nc.updated(0, nc(0).updated(0, c0 - delta_x.head * deriv2nd0 ) )
    println("===================================")
    println(nc)
    println(deriv2nd0, deriv2ndN, rat_yx(0))
    println(nc1)
    println("===================================")
    val last = nc1.length - 1
    val cN   = nc1(last)(0)
    val nc2  = nc1.updated(last, nc1(last).updated(0, cN - delta_x.last * deriv2ndN ) )
    println(last, rat_yx(last), nc2)
    val C = Matrix(nc2.toList)

    val dim = (indep.length - 2)
    //TODO : This can be optimized as this is a band matrix
    val main = (for (n <- Range(0, dim);
                     m <- Range(0, dim)) yield {
      (n, m) match {
        case (k, l) if (k == l + 1) => ((n, m) -> delta_x(l + 1))
        case (k, l) if l == k => ((n, m) -> 2.0 * (delta_x(l) + delta_x(l + 1)))
        case (k, l) if (k + 1 == l) => ((n, m) -> delta_x(l))
        case _ => ((n, m) -> 0.0)
      }

      //    }
    }).toMap

    val n00   = 2.0*(delta_x(0) + delta_x(1))
    val n01   = delta_x(1)
    val n2N   = delta_x(dim-1)
    val n1N   = 2.0 * (delta_x(dim - 1) + delta_x(dim))

    val nmain = main + ((0,0)->n00, (0,1)->n01, (dim-1, dim-2)->n2N, (dim-1,dim-1)->n1N)
    val M     = new Matrix(nmain, dim, dim)
    M.pp
    C.pp
    apply(M, C).map(_.values((x, y) => true)).flatMap(end_points(rat_yx, delta_x, _))

  }

}
