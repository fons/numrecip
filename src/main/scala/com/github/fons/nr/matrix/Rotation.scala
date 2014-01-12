package com.github.fons.nr.matrix

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/2/13
 * Time: 6:32 PM
 * To change this template use File | Settings | File Templates.
 */
case class Rotation(dr: Int, dc: Int, pivot: (Int, Int) = (0, 0)) extends MatrixTransform {
  private def safe_mod(x0: Int, md: Int) = {
    val x1 = x0 % md
    if (x1 < 0) x1 + md else x1
  }


  def apply(m: Matrix): Option[Matrix] = {
    val (row_pivot, col_pivot) = pivot

    def remap(row: Int, col: Int) = {

      def new_key(r: Int, c: Int) = {

        val nr = r - dr + row_pivot
        val nc = c - dc + col_pivot
        val nrr = if (nr < row_pivot) (nr + (m.rows - row_pivot)) else nr
        val ncc = if (nc < col_pivot) (nc + (m.colls - col_pivot)) else nc
        //        println("--------------")
        //        println(c,nc,ncc)
        //        println(r,com.github.fons.nr,nrr)
        //        println("--------------")
        (nrr, ncc)
      }

      (row, col) match {
        case (r, c) if (r < row_pivot || c < col_pivot) => (r, c)
        case (r, c) => new_key(r, c)
        //case (r,c) => (r-dr, c-dc)
      }

    }
    //TODO : if pivot == (rot) => no net movement
    //TODO : should not be partial function application ?
    m.flatMap(remap _)
  }

  def columnPermutation(m: Matrix): Option[Matrix] = {
    val (row_pivot, col_pivot) = pivot

    def remap(row: Int, col: Int) = {

      def new_key(r: Int, c: Int) = {
        val nc = c - dc + col_pivot
        val ncc = if (nc < col_pivot) (nc + (m.colls - col_pivot)) else nc
        (r, ncc)
      }

      (row, col) match {
        case (r, c) if (r < row_pivot || c < col_pivot) => (r, c)
        case (r, c) => new_key(r, c)
      }

    }
    //TODO : if pivot == (rot) => no net movement
    //TODO : should not be partial function application ?
    m.flatMap(remap _)
  }

  def rowPermutation(m: Matrix): Option[Matrix] = {
    val (row_pivot, col_pivot) = pivot

    def remap(row: Int, col: Int) = {

      def new_key(r: Int, c: Int) = {
        val nr = r - dr + row_pivot
        val nrr = if (nr < row_pivot) (nr + (m.rows - row_pivot)) else nr
        (nrr, c)
      }

      (row, col) match {
        case (r, c) if (r < row_pivot || c < col_pivot) => (r, c)
        case (r, c) => new_key(r, c)
        //case (r,c) => (r-dr, c-dc)
      }

    }
    //TODO : if pivot == (rot) => no net movement
    //TODO : should not be partial function application ?
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
