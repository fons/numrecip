package com.github.fons.nr.matrix

//import com.mhsw.com.github.fons.nr.matrix.Matrix

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/27/13
 * Time: 2:57 PM
 * To change this template use File | Settings | File Templates.
 */
trait RowPivot extends PivotT {

  private def findMax(x: Double, y: Double): Option[Either[Double, Double]] = {
    (x, y) match {
      case (l, r) if scala.math.abs(l) > scala.math.abs(r) => Some(Left(l))
      case (l, r) => Some(Right(r))
    }
  }

  def pivot(cc: Int, m: Matrix): Option[MatrixTransform] = {
    val keys = m.collKeys(cc).dropWhile((key: (Int, Int)) => key._1 < key._2)
    Matrix.find(m, keys, findMax) match {
      case Some(((r, c), v)) => Some(RowSwap(r, cc))
      case None => None
    }
  }

  override
  protected def className[A](a: A)(implicit m: Manifest[A]) = m.toString

  override
  def pivotName = className(this)
}
