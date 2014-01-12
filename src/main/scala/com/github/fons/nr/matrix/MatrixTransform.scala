package com.github.fons.nr.matrix

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/27/13
 * Time: 2:54 PM
 * To change this template use File | Settings | File Templates.
 */
abstract class MatrixTransform {
  def apply(m: Matrix): Option[Matrix]

  def rowPermutation(m: Matrix): Option[Matrix]

  def columnPermutation(m: Matrix): Option[Matrix]
}
