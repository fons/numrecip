package com.github.fons.nr.matrix

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/20/13
 * Time: 3:51 PM
 * To change this template use File | Settings | File Templates.
 */
case class RowSwap(r1: Int, r2: Int) extends MatrixTransform {

  def apply(m: Matrix): Option[Matrix] = {
    if (r1 == r2) Some(m)
    else {
      val keys = m.rowKeys(r1) ++ m.rowKeys(r2)
      (m.row(r1), m.row(r2)) match {
        case (Some(row1), Some(row2)) => {
          def swp(r: Int, c: Int, v: Double) = {
            r match {
              case r if r == r1 => row2(c)
              case r if r == r2 => row1(c)
              case _ => v
            }
          }
          m.flatMap(keys, swp)
        }
        case _ => None
      }
    }
  }

  def columnPermutation(m: Matrix): Option[Matrix] = {
    Some(m)
  }

  def rowPermutation(m: Matrix): Option[Matrix] = {
    apply(m)
  }

}
