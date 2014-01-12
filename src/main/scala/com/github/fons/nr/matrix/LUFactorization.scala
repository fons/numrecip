package com.github.fons.nr.matrix

import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/2/14
 * Time: 6:30 PM
 * To change this template use File | Settings | File Templates.
 */
case object LUFactorization extends RowPivot {

  private
  def init(a: Matrix, p: Matrix) = {
    val Pivot = pivot(0, a)
    val PA = Pivot.flatMap(_(a))
    val PP = Pivot.flatMap(_(p))
    (PP, PA, PA.flatMap(_ takeColumn (0)), PA.flatMap(_(0, 0))) match {
      case (Some(_p_), Some(_a_), Some(l), Some(d)) => l.flatMap((r: Int, c: Int, v: Double) => v / d).flatMap((m: Matrix) => Some((m, Matrix(List(List(d))), _a_, _p_)))
      case _ => None
    }
  }

  private
  def nextlu(row: Int, col: Int, value: Double, l: Matrix, u: Matrix, a: Matrix, p: Matrix) = {
    val Pivot = pivot(col, a)
    val PA = Pivot.flatMap(_(a))
    val PL = Pivot.flatMap(_(l))
    val PP = Pivot.flatMap(_(p))
    (PL.map(_ expand(0, 1)).flatMap(_(col, col, 1)), u.expand(1, 1)(0, col, value), PA, PP) match {
      case (Some(_nl_), Some(_nu_), Some(_a_), Some(_p_)) => Some((_nl_, _nu_, _a_, _p_))
      case _ => None
    }
  }

  private
  def lift_prod(vV1: Option[Double], vV2: Option[Double]) = {
    (vV1, vV2) match {
      case (Some(v1), Some(v2)) => Some(v1 * v2)
      case _ => None
    }
  }

  private
  def lift_add(vV1: Option[Double], vV2: Option[Double]) = {
    (vV1, vV2) match {
      case (Some(v1), Some(v2)) => Some(v1 + v2)
      case _ => None
    }
  }

  private
  def luprod(row: Int, col: Int, l: Matrix, u: Matrix): Option[Double] = {
    lazy val rkl = for (col_ <- Range(0, l.colls)) yield (row, col_)
    lazy val cku = for (row_ <- Range(0, u.rows)) yield (row_, col)
    lazy val com = rkl.zip(cku)
    def add(agg: Option[Double], el: ((Int, Int), (Int, Int))): Option[Double] = {
      val ((rowl, coll), (rowu, colu)) = el
      lift_add(agg, lift_prod(l(rowl, coll), u(rowu, colu)))
    }
    val z: Option[Double] = Some(0.0) // type is required
    com.foldLeft(z)(add)
  }

  private
  def nextu(row: Int, col: Int, value: Double, l: Matrix, u: Matrix): Option[Matrix] = luprod(row, col, l, u).map(value - _).flatMap(u(row, col, _))

  private
  def nextl(row: Int, col: Int, value: Double, l: Matrix, u: Matrix): Option[Matrix] = {
    u(col, col) match {
      case None => None
      case Some(v) if v == 0 => None
      case Some(v) => luprod(row, col, l, u).map((r: Double) => ((value - r) / v)).flatMap(l(row, col, _))
    }
  }


  @tailrec
  private
  def ludecomp(row: Int, col: Int, l: Matrix, u: Matrix, a: Matrix, p: Matrix): Option[(Matrix, Matrix, Matrix)] = {
    val nrow = row + 1
    val (new_row, new_col) = if (nrow < a.rows) (nrow, col) else (0, col + 1)
    val new_luap = (row, col) match {
      case (0, _) => a(0, col).flatMap(nextlu(row, col, _, l, u, a, p))
      case (r, c) if r <= c => a(r, c).flatMap(nextu(row, col, _, l, u)).map((newu: Matrix) => (l, newu, a, p))
      case (r, c) if r > c => a(r, c).flatMap(nextl(row, col, _, l, u)).map((newl: Matrix) => (newl, u, a, p))
    }
    //ludecomp(new_row, new_col, com.mhsw.com.github.fons.nr.matrix.Matrix(), com.mhsw.com.github.fons.nr.matrix.Matrix(), A)
    new_luap match {
      case Some((_l_, _u_, _a_, _p_)) if (new_col >= _a_.colls) => Some((_l_, _u_, _p_))
      case Some((_l_, _u_, _a_, _p_)) => ludecomp(new_row, new_col, _l_, _u_, _a_, _p_) //Some((l,u))
      case None => None
    }
  }

  //---------------------------------------------

  def apply(m: Matrix) = {

    init(m, Matrix.Unit(m.rows, m.colls)).flatMap((palu: (Matrix, Matrix, Matrix, Matrix)) => {
      val (l, u, a, p) = palu;
      ludecomp(0, 1, l, u, a, p)
    })
  }

  override lazy val toString = productPrefix

}
