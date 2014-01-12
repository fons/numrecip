package com.github.fons.nr.matrix

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/29/13
 * Time: 10:54 AM
 * To change this template use File | Settings | File Templates.
 */
case class FullRotation(dr: Int, dc: Int, pivot: (Int, Int) = (0, 0)) extends MatrixTransform {

  private def safe_mod(x0: Int, md: Int) = {
    val x1 = x0 % md
    if (x1 < 0) x1 + md else x1
  }

  def apply(m: Matrix): Option[Matrix] = {

    def remap(row: Int, col: Int) = {
      val (row_pivot, col_pivot) = pivot

      def new_row(r: Int) = {
        val nr = r - dr + row_pivot
        if (nr < row_pivot) (nr + (m.rows - row_pivot)) else nr
      }

      def new_col(c: Int) = {
        val nc = c - dc + col_pivot
        if (nc < col_pivot) (nc + (m.colls - col_pivot)) else nc
      }

      (row, col) match {
        case (r, c) if (r < row_pivot && c < col_pivot) => (r, c)
        case (r, c) if (r < row_pivot && c >= col_pivot) => (r, new_col(c))
        case (r, c) if (r >= row_pivot && c < col_pivot) => (new_row(r), c)
        case (r, c) => (new_row(r), new_col(c))
        //case (r,c) => (r-dr, c-dc)
      }

    }
    //TODO : if pivot == (rot) => no net movement
    //TODO : should not be partial function application ?
    m.flatMap(remap _)
  }

  def columnPermutation(m: Matrix): Option[Matrix] = {
    def remap(row: Int, col: Int) = {
      val (row_pivot, col_pivot) = pivot

      def new_col(c: Int) = {
        val nc = c - dc + col_pivot
        if (nc < col_pivot) (nc + (m.colls - col_pivot)) else nc
      }

      (row, col) match {
        case (r, c) if (r < row_pivot && c < col_pivot) => (r, c)
        case (r, c) if (r < row_pivot && c >= col_pivot) => (r, new_col(c))
        case (r, c) if (r >= row_pivot && c < col_pivot) => (r, c)
        case (r, c) => (r, new_col(c))
        //case (r,c) => (r-dr, c-dc)
      }

    }
    //TODO : if pivot == (rot) => no net movement
    //TODO : should not be partial function application ?
    m.flatMap(remap _)
  }

  def rowPermutation(m: Matrix): Option[Matrix] = {

    def remap(row: Int, col: Int) = {
      val (row_pivot, col_pivot) = pivot

      def new_row(r: Int) = {
        val nr = r - dr + row_pivot
        if (nr < row_pivot) (nr + (m.rows - row_pivot)) else nr
      }

      (row, col) match {
        case (r, c) if (r < row_pivot && c < col_pivot) => (r, c)
        case (r, c) if (r < row_pivot && c >= col_pivot) => (r, c)
        case (r, c) if (r >= row_pivot && c < col_pivot) => (new_row(r), c)
        case (r, c) => (new_row(r), c)

      }

    }
    m.flatMap(remap _)
  }

  def apply(v: Vector[Double]): Option[Vector[Double]] = {
    val r = Range(0, v length)
    val m = v.length
    val nr = for (i <- r) yield (safe_mod((i - dc), m))
    val vec = for (i <- nr) yield v(i)
    Some(vec.toVector)
  }

  def reverse(m: Matrix): Option[Matrix] = {
    m.flatMap((r: Int, c: Int) => (r + dr, c + dc))
  }

}
