package com.github.fons.nr.matrix

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/29/13
 * Time: 10:13 AM
 * To change this template use File | Settings | File Templates.
 */
trait CompletePivot extends PivotT {
  private def findMax(x: Double, y: Double): Option[Either[Double, Double]] = {
    (x, y) match {
      case (0.0, _) => None
      case (_, 0.0) => None
      case (l, r) if scala.math.abs(l) > scala.math.abs(r) => Some(Left(l))
      case (l, r) => Some(Right(r))
    }
  }

  def pivot(cc: Int, M: Matrix): Option[MatrixTransform] = {
    //println("cc : " ,cc)
    val keys = M.keys((row: Int, col: Int) => !((row < cc) || (col < cc)))
    Matrix.find(M, keys, findMax) match {
      case Some(((r, c), v)) => {
        //println("max : ",r,c,v)
        Some(FullRotation(r, c, (cc, cc)))
      }
      case None => None
    }
  }

  override
  protected def className[A](a: A)(implicit m: Manifest[A]) = m.toString

  override
  def pivotName = className(this)
}
