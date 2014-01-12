package com.github.fons.nr.ode

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/22/13
 * Time: 8:33 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.util.{Try, Success, Failure}
import com.github.fons.nr.ode.butcher.tableau._

object Factory {

  def apply(odet: Solver.Type, step: Double, init: (Double, List[Double]), Func: List[(Double, Double *) => Double]): Try[OdeSolverT] = {
    odet match {
      case (Solver.EM) => Try(new OdeSolver(step, init, Func) with ExplicitRungeKutta with ExplicitMidPointTableau)
      case (Solver.EU) => Try(new OdeSolver(step, init, Func) with Euler)
      case (Solver.RK4) => Try(new OdeSolver(step, init, Func) with ExplicitRungeKutta4)
      case (Solver.RK5) => Try(new OdeSolver(step, init, Func) with ExplicitRungeKutta with RK5Tableau)
      case (Solver.BS23) => Try(new OdeSolver(step, init, Func) with BS23)
      case (Solver.RKDP1) => Try(new OdeSolver(step, init, Func) with ExplicitRungeKutta with RKDormPrin54Tableau)
      case (Solver.RKDP2) => Try(new OdeSolver(step, init, Func) with ExplicitRungeKutta with RKDormPrin336BTableau)
      case (Solver.RKE56) => Try(new OdeSolver(step, init, Func) with ExplicitRungeKutta with RKE56Tableau)
      case (Solver.RKF78) => Try(new OdeSolver(step, init, Func) with ExplicitRungeKutta with RKFehlberg78Tableau)
      case (Solver.RKF56) => Try(new OdeSolver(step, init, Func) with ExplicitRungeKutta with RKFehlberg56Tableau)
      case (Solver.RKCK) => Try(new OdeSolver(step, init, Func) with ExplicitRungeKutta with CashKarpTableau)
      case (Solver.RKE23) => Try(new OdeSolver(step, init, Func) with ExplicitRungeKutta with RKE23Tableau)
      case (Solver.RK42) => Try(new OdeSolver(step, init, Func) with ExplicitRungeKutta with RK42Tableau)
      case (Solver.RKV) => Try(new OdeSolver(step, init, Func) with ExplicitRungeKutta with RKVernerTableau)
      case _ => Failure(new IllegalArgumentException("no ode factory method for solver type " + odet))
    }
  }

  def apply(odet: Solver.Type, step: Double, init: (Double, List[Double]), Func: List[(Double, Double *) => Double], accuracy: Double): Try[OdeSolverT] = {
    odet match {
      case (Solver.EM) => Try(new AdaptiveStepOdeSolver(step, init, Func) with ExplicitRungeKutta with ExplicitMidPointTableau with AdaptiveStep {
        val required_accuracy = accuracy
      })
      case (Solver.EU) => Try(new AdaptiveStepOdeSolver(step, init, Func) with Euler with AdaptiveStep {
        val required_accuracy = accuracy
      })
      case (Solver.RK4) => Try(new AdaptiveStepOdeSolver(step, init, Func) with ExplicitRungeKutta4 with AdaptiveStep {
        val required_accuracy = accuracy
      })
      case (Solver.RK5) => Try(new AdaptiveStepOdeSolver(step, init, Func) with ExplicitRungeKutta with RK5Tableau with AdaptiveStep {
        val required_accuracy = accuracy
      })
      case (Solver.BS23) => Try(new AdaptiveStepOdeSolver(step, init, Func) with BS23 with AdaptiveStep {
        val required_accuracy = accuracy
      })
      case (Solver.RKDP1) => Try(new AdaptiveStepOdeSolver(step, init, Func) with ExplicitRungeKutta with RKDormPrin54Tableau with AdaptiveStep {
        val required_accuracy = accuracy
      })
      case (Solver.RKDP2) => Try(new AdaptiveStepOdeSolver(step, init, Func) with ExplicitRungeKutta with RKDormPrin336BTableau with AdaptiveStep {
        val required_accuracy = accuracy
      })
      case (Solver.RKE56) => Try(new AdaptiveStepOdeSolver(step, init, Func) with ExplicitRungeKutta with RKE56Tableau with AdaptiveStep {
        val required_accuracy = accuracy
      })
      case (Solver.RKF78) => Try(new AdaptiveStepOdeSolver(step, init, Func) with ExplicitRungeKutta with RKFehlberg78Tableau with AdaptiveStep {
        val required_accuracy = accuracy
      })
      case (Solver.RKF56) => Try(new AdaptiveStepOdeSolver(step, init, Func) with ExplicitRungeKutta with RKFehlberg56Tableau with AdaptiveStep {
        val required_accuracy = accuracy
      })
      case (Solver.RKCK) => Try(new AdaptiveStepOdeSolver(step, init, Func) with ExplicitRungeKutta with CashKarpTableau with AdaptiveStep {
        val required_accuracy = accuracy
      })
      case (Solver.RKE23) => Try(new AdaptiveStepOdeSolver(step, init, Func) with ExplicitRungeKutta with RKE23Tableau with AdaptiveStep {
        val required_accuracy = accuracy
      })
      case (Solver.RK42) => Try(new AdaptiveStepOdeSolver(step, init, Func) with ExplicitRungeKutta with RK42Tableau with AdaptiveStep {
        val required_accuracy = accuracy
      })
      case (Solver.RKV) => Try(new AdaptiveStepOdeSolver(step, init, Func) with ExplicitRungeKutta with RKVernerTableau with AdaptiveStep {
        val required_accuracy = accuracy
      })
      case _ => Failure(new IllegalArgumentException("no ode factory method for solver type " + odet))
    }
  }


  //  def apply(odet : Solver.Type, step : Double, init : (Double, Double), Func : (Double, Double)=>Double ) : com.mhsw.com.github.fons.nr.ode.OdeSolverT = {
  //    odet match {
  //      case (Solver.EM)     => new com.mhsw.com.github.fons.nr.ode.Integrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta with com.mhsw.com.github.fons.nr.ode.butcher.tableau.ExplicitMidPointTableau
  //      case (Solver.EU)     => new com.mhsw.com.github.fons.nr.ode.Integrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.Euler
  //      case (Solver.RK4)    => new com.mhsw.com.github.fons.nr.ode.Integrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta4
  //      case (Solver.RK5)    => new com.mhsw.com.github.fons.nr.ode.Integrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta with com.mhsw.com.github.fons.nr.ode.butcher.tableau.RK5Tableau
  //      case (Solver.com.mhsw.com.github.fons.nr.ode.BS23)   => new com.mhsw.com.github.fons.nr.ode.Integrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.BS23
  //      case (Solver.RKDP1)  => new com.mhsw.com.github.fons.nr.ode.Integrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta with com.mhsw.com.github.fons.nr.ode.butcher.tableau.RKDormPrin54Tableau
  //      case (Solver.RKDP2)  => new com.mhsw.com.github.fons.nr.ode.Integrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta with com.mhsw.com.github.fons.nr.ode.butcher.tableau.RKDormPrin336BTableau
  //      case (Solver.RKE56)  => new com.mhsw.com.github.fons.nr.ode.Integrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta with com.mhsw.com.github.fons.nr.ode.butcher.tableau.RKE56Tableau
  //      case (Solver.RKF78)  => new com.mhsw.com.github.fons.nr.ode.Integrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta with com.mhsw.com.github.fons.nr.ode.butcher.tableau.RKFehlberg78Tableau
  //      case (Solver.RKF56)  => new com.mhsw.com.github.fons.nr.ode.Integrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta with com.mhsw.com.github.fons.nr.ode.butcher.tableau.RKFehlberg56Tableau
  //      case (Solver.RKCK)   => new com.mhsw.com.github.fons.nr.ode.Integrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta with com.mhsw.com.github.fons.nr.ode.butcher.tableau.CashKarpTableau
  //      case (Solver.RKE23)   => new com.mhsw.com.github.fons.nr.ode.Integrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta with com.mhsw.com.github.fons.nr.ode.butcher.tableau.RKE23Tableau
  //      case (Solver.RK42)   => new com.mhsw.com.github.fons.nr.ode.Integrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta with com.mhsw.com.github.fons.nr.ode.butcher.tableau.RK42Tableau
  //      case (Solver.RKV)    => new com.mhsw.com.github.fons.nr.ode.Integrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta with com.mhsw.com.github.fons.nr.ode.butcher.tableau.RKVernerTableau
  //      case _               => new com.mhsw.com.github.fons.nr.ode.Integrator(step, init, Func)
  //    }
  //  }

  //  def apply(odet : Solver.Type, step : Double, init : (Double, Double), Func : (Double, Double)=>Double, accuracy:Double) : com.mhsw.com.github.fons.nr.ode.OdeSolverT = {
  //    odet match {
  //      case (Solver.EM)     => Try(new com.mhsw.com.github.fons.nr.ode.AdaptiveStepIntegrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta  with com.mhsw.com.github.fons.nr.ode.butcher.tableau.ExplicitMidPointTableau with com.mhsw.com.github.fons.nr.ode.AdaptiveStep {val required_accuracy = accuracy})
  //      case (Solver.EU)     => Try(new com.mhsw.com.github.fons.nr.ode.AdaptiveStepIntegrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.Euler               with com.mhsw.com.github.fons.nr.ode.AdaptiveStep {val required_accuracy = accuracy})
  //      case (Solver.RK4)    => Try(new com.mhsw.com.github.fons.nr.ode.AdaptiveStepIntegrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta4 with com.mhsw.com.github.fons.nr.ode.AdaptiveStep {val required_accuracy = accuracy})
  //      case (Solver.RK5)    => Try(new com.mhsw.com.github.fons.nr.ode.AdaptiveStepIntegrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta  with com.mhsw.com.github.fons.nr.ode.butcher.tableau.RK5Tableau with com.mhsw.com.github.fons.nr.ode.AdaptiveStep {val required_accuracy = accuracy})
  //      case (Solver.com.mhsw.com.github.fons.nr.ode.BS23)   => Try(new com.mhsw.com.github.fons.nr.ode.AdaptiveStepIntegrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.BS23                with com.mhsw.com.github.fons.nr.ode.AdaptiveStep {val required_accuracy = accuracy})
  //      case (Solver.RKDP1)  => Try(new com.mhsw.com.github.fons.nr.ode.AdaptiveStepIntegrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta  with com.mhsw.com.github.fons.nr.ode.butcher.tableau.RKDormPrin54Tableau with com.mhsw.com.github.fons.nr.ode.AdaptiveStep {val required_accuracy = accuracy})
  //      case (Solver.RKDP2)  => Try(new com.mhsw.com.github.fons.nr.ode.AdaptiveStepIntegrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta  with com.mhsw.com.github.fons.nr.ode.butcher.tableau.RKDormPrin336BTableau with com.mhsw.com.github.fons.nr.ode.AdaptiveStep {val required_accuracy = accuracy})
  //      case (Solver.RKE56)  => Try(new com.mhsw.com.github.fons.nr.ode.AdaptiveStepIntegrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta  with com.mhsw.com.github.fons.nr.ode.butcher.tableau.RKE56Tableau with com.mhsw.com.github.fons.nr.ode.AdaptiveStep {val required_accuracy = accuracy})
  //      case (Solver.RKF78)  => Try(new com.mhsw.com.github.fons.nr.ode.AdaptiveStepIntegrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta  with com.mhsw.com.github.fons.nr.ode.butcher.tableau.RKFehlberg78Tableau with com.mhsw.com.github.fons.nr.ode.AdaptiveStep {val required_accuracy = accuracy})
  //      case (Solver.RKF56)  => Try(new com.mhsw.com.github.fons.nr.ode.AdaptiveStepIntegrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta  with com.mhsw.com.github.fons.nr.ode.butcher.tableau.RKFehlberg56Tableau with com.mhsw.com.github.fons.nr.ode.AdaptiveStep {val required_accuracy = accuracy})
  //      case (Solver.RKCK)   => Try(new com.mhsw.com.github.fons.nr.ode.AdaptiveStepIntegrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta  with com.mhsw.com.github.fons.nr.ode.butcher.tableau.CashKarpTableau with com.mhsw.com.github.fons.nr.ode.AdaptiveStep {val required_accuracy = accuracy})
  //      case (Solver.RKE23)   => Try(new com.mhsw.com.github.fons.nr.ode.AdaptiveStepIntegrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta  with com.mhsw.com.github.fons.nr.ode.butcher.tableau.RKE23Tableau  with com.mhsw.com.github.fons.nr.ode.AdaptiveStep {val required_accuracy = accuracy})
  //      case (Solver.RK42)   => Try(new com.mhsw.com.github.fons.nr.ode.AdaptiveStepIntegrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta  with com.mhsw.com.github.fons.nr.ode.butcher.tableau.RK42Tableau  with com.mhsw.com.github.fons.nr.ode.AdaptiveStep {val required_accuracy = accuracy})
  //      case (Solver.RKV)    => Try(new com.mhsw.com.github.fons.nr.ode.AdaptiveStepIntegrator(step, init, Func) with com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta  with com.mhsw.com.github.fons.nr.ode.butcher.tableau.RKVernerTableau with com.mhsw.com.github.fons.nr.ode.AdaptiveStep {val required_accuracy = accuracy})
  //      case _               => Try(new com.mhsw.com.github.fons.nr.ode.AdaptiveStepIntegrator(step, init, Func)
  //    }
  //  }

}

