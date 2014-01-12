package com.github.fons.nr.ode

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/5/13
 * Time: 9:42 PM
 * To change this template use File | Settings | File Templates.
 */
trait Memoize extends MemoizeT {
  override def store(v: (Double, List[Double])) = {
    val L = list :+ v
    new Memoize {
      override val list = L
    }
  }

  override def toString = "com.mhsw.com.github.fons.nr.ode.Memoize@" + hashCode().toString + "@entries:" + (list length).toString
}
