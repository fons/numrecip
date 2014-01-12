package com.github.fons.nr.util

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/30/13
 * Time: 10:27 AM
 * To change this template use File | Settings | File Templates.
 */
case class Accuracy(acc: Double = 1.0E-10) {
  def apply(value: Double): Boolean = if (value > 0.0) value < acc else (-value) < acc
}
