package com.github.fons.nr.ode

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/4/13
 * Time: 7:11 PM
 * To change this template use File | Settings | File Templates.
 */

trait MemoizeT {
  val list: Vector[(Double, List[Double])] = Vector[(Double, List[Double])]()

  def store(v: (Double, List[Double])) = {
    this
  }

  def toList = list.toList
}
