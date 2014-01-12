package com.github.fons.nr.ode

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/10/13
 * Time: 11:23 AM
 * To change this template use File | Settings | File Templates.
 */
object AdaptiveStepTransition extends Enumeration {
  type RunState = Value
  val HALT, CONTINUE, RETRY = Value
}
