package com.github.fons.nr.matrix

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/27/13
 * Time: 2:47 PM
 * To change this template use File | Settings | File Templates.
 */
trait PivotT {
  def pivot(cc: Int, m: Matrix): Option[MatrixTransform]

  protected def className[A](a: A)(implicit m: Manifest[A]) = m.toString

  def pivotName = className(this)
}
