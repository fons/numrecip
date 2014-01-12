package com.github.fons.nr.matrix

import javax.sql.rowset.RowSetProvider
import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/26/13
 * Time: 4:20 PM
 * To change this template use File | Settings | File Templates.
 */

object LUFactorizationDeprecated extends RowPivot {

  @tailrec private def lu(cc: Int, l: Matrix, u: Matrix, p: Matrix, I: Option[Matrix]): Option[(Matrix, Matrix, Matrix)] = {
    //Pivoting : row pivot

    val Pivot = pivot(cc, u)
    val P = Pivot.flatMap(_(p)) //permutation matrix
    val RU = Pivot.flatMap(_(u))
    val RL = Pivot.flatMap(_(l))
    ///-------------
    val fb = RU.flatMap(_ froebenius (cc))
    val U = fb.flatMap(_ *> RU)
    val L = fb.flatMap(_ -> I).map(_ map (_ * -1)).flatMap(_ +> RL)
    (L, U, P) match {
      case (Some(ln), Some(un), Some(pn)) => {
        (cc + 1) match {
          case ncc if ncc < u.colls => lu(ncc, ln, un, pn, I)
          case _ => (ln +> I) match {
            case Some(ln_) => Some((ln_, un, pn))
            case None => None
          }
        }
      }
      case _ => None
    }
  }

  def apply(m: Matrix): Option[(Matrix, Matrix, Matrix)] = {
    val zero = Matrix.Zero(m.rows, m.colls)
    val unit = Matrix.Unit(m.rows, m.colls)
    lu(0, zero, m, unit, Some(unit))
  }

}
