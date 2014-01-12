package com.github.fons.nr.matrix

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/27/13
 * Time: 3:27 PM
 * To change this template use File | Settings | File Templates.
 */
trait FullPivot extends PivotT {

  private def findMax(x: Double, y: Double): Option[Either[Double, Double]] = {
    (x, y) match {
      case (0.0, _) => None
      case (_, 0.0) => None
      case (l, r) if scala.math.abs(l) > scala.math.abs(r) => Some(Left(l))
      case (l, r) => Some(Right(r))
    }
  }

  //Note that cc (current column is not used !
  def pivot(cc: Int, M: Matrix): Option[MatrixTransform] = {
    Matrix.find(M, findMax) match {
      case Some(((r, c), v)) => {
        Some(Rotation(r, c))
      }
      case None => None
    }
  }

  override
  protected def className[A](a: A)(implicit m: Manifest[A]) = m.toString

  override
  def pivotName = className(this)
}
