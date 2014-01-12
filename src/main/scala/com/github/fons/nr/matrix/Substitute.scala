package com.github.fons.nr.matrix

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/13/13
 * Time: 11:13 AM
 * To change this template use File | Settings | File Templates.
 */
object Substitute {

  private def safe_divide(num: Double, denom: Double): Option[Double] = {
    if (denom != 0.00) Some(num / denom) else None
  }

  private def add(Agg: Option[Double], V: Option[Double]): Option[Double] = {
    (Agg, V) match {
      case (Some(a), Some(v)) => Some(a + v)
      case _ => None
    }
  }

  private def process_row(nrow: Int, keys: IndexedSeq[(Int, Int, Int)], Mt: Matrix, Coeff: Matrix, Accum: Vector[Double]): Option[Double] = {

    // process current result set in Accum
    // Multiply off diagonal elements
    val values = for ((r, c, i) <- keys) yield {
      Mt(r, c) match {
        case Some(a_rc) => Some(a_rc * Accum(i))
        case None => None
      }
    }
    // subtract the sum off the off diagonal elements from the
    // RHS vector Coeff
    val Res = (values, Coeff(nrow, 0)) match {
      case (Vector(), Some(coeff)) => Some(coeff)
      case (values_, Some(coeff)) => {
        values_.reduce(add) match {
          case Some(result) => Some(coeff - result)
          case _ => None
        }
      }
      case _ => None
    }

    (Mt(nrow, nrow), Res) match {
      case (Some(diagelement), Some(res)) => safe_divide(res, diagelement)
      case _ => None
    }
  }

  private def fwd_prop(nrow: Int, Mt: Matrix, Coeff: Matrix, Accum: Vector[Double]): Option[Matrix] = {
    val keys = for (i <- Range(0, nrow)) yield (nrow, i, i)
    process_row(nrow, keys, Mt, Coeff, Accum) match {
      case None => None
      case Some(result) => {
        val nextnrow = nrow + 1
        val nextaccum = Accum :+ result
        if (nextnrow == Mt.rows) Some(Matrix(List(nextaccum.toList)).^) else fwd_prop(nextnrow, Mt, Coeff, nextaccum)
      }
    }

  }

  private def bck_prop(nrow: Int, Mt: Matrix, Coeff: Matrix, Accum: Vector[Double]): Option[Matrix] = {
    val keys = for (i <- Range(nrow + 1, Mt.colls)) yield (nrow, i, Mt.colls - i - 1)
    process_row(nrow, keys, Mt, Coeff, Accum) match {
      case None => None
      case Some(result) => {
        val nextnrow = nrow - 1
        val nextaccum = Accum :+ result
        if (nextnrow < 0) Some(Matrix(List(nextaccum.toList reverse)).^) else bck_prop(nextnrow, Mt, Coeff, nextaccum)
      }
    }

  }

  def forward(Mt: Matrix, Coeff: Matrix): Option[Matrix] = fwd_prop(0, Mt, Coeff, Vector[Double]())

  def backward(Mt: Matrix, Coeff: Matrix): Option[Matrix] = bck_prop(Mt.rows - 1, Mt, Coeff, Vector[Double]())
}
