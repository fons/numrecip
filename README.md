#Numerical Recipes for Scala
==========
## Overview
==========

This Scala library implements a algorithms for numerical analysis.

use sbt compile to build

packages: 

* com.github.fons.nr : root of the package path
* ode : Ordinary differential equations
* ode.butcher.tableau : Butcher tableaux for ode
* matrix : matrix and linear systems
* test : tests. Useful for usage examples  


##com.github.fons.nr.ode

###Synopsis

Embedded Runge-Kutta method using an embedded error estimation.
Data types are shown for illustrative purposes only. 

```scala

import com.github.fons.nr.ode.{Solver, OdeSolverT, Factory}
import scala.math.{Pi, sin, cos}
import scala.util.{Try, Success, Failure}
 
// ode to solve
// t : independent variable. 
// x : depended variables.

  def f(t: Double, x: Double*): Double = -Pi * x(1)
  def g(t: Double, x: Double*): Double = Pi * x(0)

//exact solution
  def fx(t: Double) = cos(Pi * t)
  def gx(t: Double) = sin(Pi * t)
  
// 0.2                  : step size
// (0.0, List(1.0,0.0)) : initial values 
// List(f,g)            : system of ode's
// 0.0001               : required accurcy; 
// specifying accuracy  : use an adaptive step size method

val ode : Try[OdeSolverT] = Factory(Solver.RKE56, 0.2, (0.0, List(1.0, 0.0)), List(f, g), 0.00001)
  ode match {
    case Success(ode) => {
        ode(0.15) match {
        case Success(((tn,results:List[Double]), _)) => {
          println("method : " + Solver.RKE56 + " for " + tn)
          println("returned : " + results(0) + ", " + results(1))
          println("exact    : " + fx(tn) + ", " + gx(tn))
        }
        case _ => println("failed")
      }
  }
    case _ => println("failed")
  }

//////////////////////////////////////

method : RKEmbedded56 for 0.15<br>
returned : 0.8910063287560628, 0.4539905878227192 <br>
exact    : 0.8910065241883679, 0.45399049973954675


```

