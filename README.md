#Numerical Recipes for Scala
==========
## Overview
==========

This Scala library implements algorithms for numerical analysis.


use 
```scala
sbt compile to build
```

packages: 

* com.github.fons.nr : root of the package path
* ode : Ordinary differential equations
* ode.butcher.tableau : Butcher tableaux for ode
* matrix : matrix and linear systems
* test : tests. Useful for usage examples  


##com.github.fons.nr.ode

### Overview

Currently supports 

* explicit Runge-Kutta methods.
* embedded Runge-Kutta 
* adaptive step-size 

The ode package has a factory object which returns an ode solver function object. The function object returns the numerical approximation for the ode's.

The Solver object enumerates all the possible ode solvers :
```scala
object Solver extends Enumeration {

  type Type = Value
  val RK4 = Value(0, "ExplicitRungeKutta4")
  val EM = Value(1, "ExplicitMidPoint")
  val EU = Value(2, "Euler")
  val RK5 = Value(3, "RungeKuttaWithRk5Tableau")
  val RKF56 = Value(4, "RKEmbeddedFehlberg56")
  val RKF78 = Value(5, "RKEmbeddedFehlberg78")
  val RKDP1 = Value(6, "RKEmbeddedDormandPrince54")
  val RKDP2 = Value(7, "RKEmbeddedDormandPrince")
  val RKV = Value(8, "RKEmbeddedVerner")
  val RKCK = Value(9, "RKEmbeddedCashKarp")
  val RK42 = Value(10, "RungeKutta42")
  val BS23 = Value(12, "RKBS23fromMatLab")
  val RKE56 = Value(13, "RKEmbedded56")
  val RKE23 = Value(14, "RKEmbedded23")
}
```

###Synopsis for fixed-step methods

All methods can be run for a fixed step size.

```scala

    /// ode to solve
    def f(t: Double, x: Double*): Double = -Pi * x(1)
    def g(t: Double, x: Double*): Double = Pi * x(0)
    //exact solution
    def fx(t: Double) = cos(Pi * t)
    def gx(t: Double) = sin(Pi * t)

    /**
        // Method    : Solver.EU; Euler method
        // step size : 0.01
        // initial conditions : (0.0, List(1.0, 0.0))
        //                      0.0 : initial independent variable
        // set of ode's       : List(f,g)
        //
      **/

    val ode : Try[OdeSolverT] = Factory(Solver.EU, 0.01, (0.0, List(1.0, 0.0)), List(f, g))
    ode match {
      case Success(ode) => {
        ode(0.15) match {
          case Success(((tn,results:List[Double]), _)) => {
            println("method : " + Solver.EU + " for " + tn)
            println("returned : " + results(0) + ", " + results(1))
            println("exact    : " + fx(tn) + ", " + gx(tn))
          }
          case _ => println("failed")
        }
      }
      case _ => println("failed")
    }

  }

```

```
Output :

method : Euler for 0.15
returned : 0.8976939822313199, 0.45722274565355875
exact    : 0.8910065241883679, 0.45399049973954675

Process finished with exit code 0

```

###Synopsis for adaptive step methods

Embedded Runge-Kutta (RK) methods use an additional step to determine the accuracy of the solution at each step. The step size is adjusted to obtain the requested accuracy.
If the RK method is not embedded the step size will be adjusted and the step will be (partially) rerun at the adjusted step size.

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
  
   /**
        // Method    : Solver.RKE56; embedded Rung-Kutta
        // step size : 0.2
        // initial conditions : (0.0, List(1.0, 0.0))
        //                      0.0 : initial independent variable
        // set of ode's       : List(f,g)
        // accuracy           : 0.0001;
        // Because an accuracy is specified an adaptive step method will be used.
    **/

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

```

```

Output :

method : RKEmbedded56 for 0.15
returned : 0.8910063287560628, 0.4539905878227192 
exact    : 0.8910065241883679, 0.45399049973954675


```

##com.github.fons.nr.matrix

###Overview

Very basic matrix package geared towards solving linear systems. The Factory method in the package returns a function object. The Solver object contains a list of all possible solvers.

```scala
object Solver extends Enumeration {
  type Type = Value
  val GaussJordanFullPivot = Value(0, "GausJordanFullPivot")
  val GaussJordanRowPivot = Value(1, "GausJordanRowPivot")
  val IterativeGaussSeidel = Value(2, "IterariveGaussSeidel")
  val IterativeJacobi = Value(3, "IterativeJacobi")
  val LUSolver = Value(4, "LUSolver")

}
```

### Synopsis

```scala

     /**
      // System :

             x_0 + 2*x_1 + 3 * x_3 = 10
         2 * x_0 + 3*x_1 +     x_3 = 11
         3 * x_0 +   x_1 + 2 * x_3 = 12

      // Initialize a Matrix with the left and right hand side coefficients of
      //

      **/
      val m = Matrix(List(List(1.0,2.0,3.0), List(2.0,3.0,1.0), List(3.0,1.0,2.0)))
      val c = Matrix(List(List(10.0), List(11.0), List(12.0)))
      val solver : Option[LinearSystemsSolverT] = Factory(Solver.LUSolver)
      solver.flatMap(_ (m,c)) match {
        case Some(r) => {
          r.pp
          (m*r - c).pp
        }
        case _ => println("error")
      }
    }

```

```
matrix : (3 x 1)
+2.50000 , 
+1.50000 , 
+1.50000 , 
matrix : (3 x 1)
+0.00000 , 
+0.00000 , 
+0.00000 , 

```

