package com.github.fons.nr.matrix

import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/1/13
 * Time: 5:54 PM
 * To change this template use File | Settings | File Templates.
 */


object Matrix {


  /*
   * List to map to matrix
   */
  @tailrec private def tom(row: Int, coll: Int, l: List[List[Double]], m: Map[(Int, Int), Double]): (Int, Int, Map[(Int, Int), Double]) = {
    l match {
      case Nil => (row, coll, m)
      case x :: xs => {
        val keys = Range(0, x length) map ((row, _))
        val nc = if ((x length) > coll) x length else coll
        tom(row + 1, nc, xs, m ++ keys.zip(x).toMap)
      }
    }
  }

  //-----------------------------------------------------------------------------------
  //
  // Goes through the matrix column by column and runs the predicate on each element and collects a list of results.
  // It then runs the predicate on the resulting list of results
  // Used for full pivoting algo's which need to handle/not pivot 0 values
  //
  def find(M: Matrix, predicate: (Double, Double) => Option[Either[Double, Double]]): Option[((Int, Int), Double)] = {

    def reduce_coll_max(Amax: Option[((Int, Int), Double)], Cmax: Option[((Int, Int), Double)]): Option[((Int, Int), Double)] = {
      (Amax, Cmax) match {
        case (_, None) => Amax
        case (None, Some(_)) => Cmax
        case (Some(((ra, ca), am)), Some(((rc, cc), cm))) => predicate(cm, am) match {
          case None => None
          case Some(Left(cm)) => Cmax
          case Some(Right(am)) => Amax
        }
        case _ => None
      }
    }

    def search_col(col: Int, M: Matrix) = {
      def find_max(Cmax: Option[((Int, Int), Double)], row: Int): Option[((Int, Int), Double)] = {
        (Cmax, M(row, col)) match {
          //TODO : handle 0 better
          case (Some(((-1, -1), _)), Some(el)) => Some(((row, col), el))
          case (Some(((r, c), cmax)), Some(el)) => predicate(cmax, el) match {
            case None => None
            case Some(Left(cmax)) => Some(((r, c), cmax))
            case Some(Right(el)) => Some(((row, col), el))
          }
          case _ => None
        }
      }
      find_max _
    }

    (for (c <- Range(0, M.colls)) yield {
      val start: Option[((Int, Int), Double)] = Some(((-1, -1), -1.00))
      Range(0, M.rows).foldLeft(start)(search_col(c, M))
    }).reduce(reduce_coll_max)

  }

  def find(M: Matrix, keys: IndexedSeq[(Int, Int)], predicate: (Double, Double) => Option[Either[Double, Double]]): Option[((Int, Int), Double)] = {
    def search(C: Option[((Int, Int), Double)], key: (Int, Int)) = {
      val (row, col) = key
      (C, M(row, col)) match {
        case (Some(((-1, -1), _)), Some(el)) => Some(((row, col), el))
        case (Some(((r, c), cmax)), Some(el)) => predicate(cmax, el) match {
          case None => None
          case Some(Left(cmax)) => Some(((r, c), cmax))
          case Some(Right(el)) => Some(((row, col), el))
        }
        case _ => None
      }
    }
    val start: Option[((Int, Int), Double)] = Some(((-1, -1), -1))
    keys.foldLeft(start)(search)
  }


  def Unit(row: Int, coll: Int) = {
    val p = for (r <- Range(0, row); s <- Range(0, coll)) yield {
      if (r == s) ((r, s) -> 1.0) else ((r, s) -> 0.00)
    }
    new Matrix(p.toMap, row, coll)
  }

  def Zero(row: Int, coll: Int) = {
    val p = for (r <- Range(0, row); s <- Range(0, coll)) yield {
      ((r, s) -> 0.00)
    }
    new Matrix(p.toMap, row, coll)
  }

  def apply() = new Matrix(Map[(Int, Int), Double](), 0, 0)

  def apply(l: List[List[Double]]) = {
    val (r, c, m) = tom(0, 0, l, Map[(Int, Int), Double]())
    new Matrix(m, r, c)
  }

  def apply(row: Int, coll: Int, f: (Int, Int) => Double) = {
    val p = for (r <- Range(0, row); s <- Range(0, coll)) yield {
      ((r, s) -> f(r, s))
    }
    new Matrix(p.toMap, row, coll)
  }
}

/*
*
*
*
*
*/
case class Matrix(m: Map[(Int, Int), Double], val rows: Int, val colls: Int) {

  private def safe_mod(x0: Int, md: Int) = {
    val x1 = x0 % md
    if (x1 < 0) x1 + md else x1
  }

  private def safe_divide(num: Double, denom: Double): Option[Double] = {
    if (denom != 0.00) Some(num / denom) else None
  }

  private def safe_mod(k: (Int, Int)): (Int, Int) = {
    val (r0, c0) = k
    (safe_mod(r0, rows), safe_mod(c0, colls))
  }

  private def overflowthrow(v: Double) = {
    v match {
      case Double.PositiveInfinity => throw new ArithmeticException("positive overflow")
      case Double.NegativeInfinity => throw new ArithmeticException("negative overflow")
      case Double.NaN => throw new ArithmeticException("not an number")
      case _ => v
    }
  }

  /** ********************************************************************/

  def toMap = m

  def froebenius(coll0: Int): Option[Matrix] = {
    def drop(p: (Int, Int)): Boolean = {
      val (r, c) = p
      r < c
    }
    val cs = safe_mod(coll0, colls)
    val collkeys = collKeys(cs).dropWhile(drop)
    //println(ck)
    val unit = Matrix.Unit(rows, colls)
    val mu: Map[(Int, Int), Double] = unit.m
    val diag = m(collkeys head)
    diag match {
      //TODO : better overflow detection
      case diag if diag == 0 => None
      case diag => {
        val fb = for (k1 <- collkeys.tail) yield (k1 -> -m(k1) / diag)
        //println(fb.toMap)
        val mn = mu ++ fb.toMap
        Some(new Matrix(mn, rows, colls))
      }
    }
  }

  def tr(f: (Double, Double) => Boolean): ((Int, Int), Double) = {
    def h(l: ((Int, Int), Double), r: ((Int, Int), Double)) = {
      if (f(l._2, r._2)) l else r
    }
    m.reduce(h)
  }

  def size() = m.size //scala.math.abs(rows*colls)

  def cut(cp: (Int, Int)): Option[Matrix] = {
    val (rcp, ccp) = safe_mod(cp)
    def h(e: ((Int, Int), Double)): Boolean = {
      val ((r0, c0), v) = e
      (rcp != r0) && (ccp != c0)
    }
    if (size() == 0) None
    else {
      val nf = m.filter(h)
      val nm = for (((r0, c0), v) <- nf) yield {
        val r1 = if (r0 > rcp) (r0 - 1) else r0
        val c1 = if (r0 > ccp) (c0 - 1) else c0
        ((r1, c1), v)
      }
      Some(new Matrix(nm, (rows - 1), (colls - 1)))
    }
  }

  def takeRow(row: Int): Option[Matrix] = takeRows(row, row + 1)

  def takeRows(from: Int, to: Int): Option[Matrix] = {
    try {
      val g = (for (row <- Range(from, to); col <- Range(0, colls)) yield ((row - from, col) -> m(row, col))).toMap
      Some(new Matrix(g, (to - from), colls))
    }
    catch {
      case e: IllegalArgumentException => None
      case _: Throwable => None
    }
  }

  def takeColumn(col: Int): Option[Matrix] = takeColumns(col, col + 1)

  def takeColumns(from: Int, to: Int): Option[Matrix] = {
    try {
      val g = (for (row <- Range(0, rows); col <- Range(from, to)) yield ((row, col - from) -> m(row, col))).toMap
      Some(new Matrix(g, rows, to - from))
    }
    catch {
      case e: IllegalArgumentException => None
      case _: Throwable => None
    }
  }

  def head(): Option[Matrix] = {
    row(0) match {
      case None => None
      case Some(v) => Some(Matrix(List(v.toList)))
    }
  }

  def tail(): Option[Matrix] = {
    def h(e: ((Int, Int), Double)): Boolean = {
      val ((r0, c0), v) = e
      (r0 != 0)
    }
    if (size() == 0) None
    else {
      val nf = m.filter(h)
      val nm = for (((r0, c0), v) <- nf) yield {
        ((r0 - 1, c0), v)
      }
      val new_colls = if (rows == 1) 0 else colls //no rows; no colls
      Some(new Matrix(nm, (rows - 1), new_colls))
    }
  }

  def map(f: (Int, Int, Double) => Double): Matrix = {
    def h(e: ((Int, Int), Double)) = {
      val ((r0, c0), v) = e
      ((r0, c0), f(r0, c0, v))
    }
    val nm = m.map(h)
    Matrix(nm, rows, colls)
  }

  def map(f: (Double) => Double): Matrix = {
    def h(e: ((Int, Int), Double)) = {
      val ((r0, c0), v) = e
      ((r0, c0), f(v))
    }
    val nm = m.map(h)
    Matrix(nm, rows, colls)
  }

  def flatMap(f: (Int, Int, Double) => Double): Option[Matrix] = {
    def h(e: ((Int, Int), Double)) = {
      val ((r0, c0), v) = e
      val v1 = f(r0, c0, v)
      ((r0, c0), overflowthrow(f(r0, c0, v)))
    }
    try {
      val nm = m.map(h)
      Some(Matrix(nm, rows, colls))
    }
    catch {
      case e: ArithmeticException => None
    }
  }

  def flatMap(f: (Int, Int) => (Int, Int)): Option[Matrix] = {
    def h(e: ((Int, Int), Double)) = {
      val ((r0, c0), v) = e
      (safe_mod(f(r0, c0)), v)
    }
    if (size() == 0) None else Some(new Matrix(m.map(h), rows, colls))
  }

  def flatMap(keys: IndexedSeq[(Int, Int)], f: (Int, Int, Double) => Double): Option[Matrix] = {
    val nv = for (key <- keys) yield {
      val (r0, c0) = key
      val v = m((r0, c0))
      (key, overflowthrow(f(r0, c0, v)))
    }
    try {
      val nm = m ++ nv.toMap
      Some(new Matrix(nm, rows, colls))
    }
    catch {
      case e: ArithmeticException => None
    }
  }

  def values(predicate: (Int, Int) => Boolean): Vector[Double] = {
    def select(agg: Vector[Double], el: (Int, Int)) = {
      val (row, col) = el
      if (predicate(row, col) == true) agg :+ m(el) else agg
    }
    val k = for (r <- Range(0, rows); c <- Range(0, colls)) yield (r, c)
    k.foldLeft(Vector[Double]())(select)
  }

  def keys(predicate: (Int, Int) => Boolean): Vector[(Int, Int)] = {
    def select(agg: Vector[(Int, Int)], el: (Int, Int)) = {
      val (row, col) = el
      if (predicate(row, col) == true) agg :+ el else agg
    }
    val k = for (r <- Range(0, rows); c <- Range(0, colls)) yield (r, c)
    k.foldLeft(Vector[(Int, Int)]())(select)
  }

  def rowKeys(row0: Int) = {
    val row = safe_mod(row0, rows)
    for (coll <- Range(0, colls)) yield (row, coll)
  }

  def collKeys(coll0: Int) = {
    val coll = safe_mod(coll0, colls)
    for (row <- Range(0, rows)) yield (row, coll)
  }

  def rp() = {
    println(m)
  }

  def pp() = {
    println("matrix : (" + f"$rows%s x $colls%s)")
    val r = Range(0, rows)
    val c = Range(0, colls)
    for (x <- r; y <- c) {
      val v = m((x, y))
      print(f"$v%+-9.5f, ")
      if (y + 1 == colls) print("\n")
    }
  }


  def apply(r: Int, c: Int): Option[Double] = {
    val rs = safe_mod(r, rows)
    val cs = safe_mod(c, colls)
    if (m.contains((rs, cs))) Some(m(rs, cs)) else None
  }


  def apply(r: Int, c: Int, v: Double): Option[Matrix] = {
    val nm = m + (safe_mod((r, c)) -> v)
    Some(new Matrix(nm, rows, colls))
  }

  def coll(coll0: Int): Option[Vector[Double]] = {
    if (size() == 0) None
    else {
      val coll = safe_mod(coll0, colls)
      val keys = for (row <- Range(0, rows)) yield (row, coll)
      val nv = for (key <- keys) yield {
        val (r0, c0) = key
        val v = m((r0, c0))
        v
      }
      Some(nv.toVector)
    }
  }

  def row(row0: Int): Option[Vector[Double]] = {
    if (size() == 0) None
    else {
      val row = safe_mod(row0, rows)
      val keys = for (coll <- Range(0, colls)) yield (row, coll)
      val nv = for (key <- keys) yield {
        val (r0, c0) = key
        val v = m((r0, c0))
        v
      }
      Some(nv.toVector)
    }
  }

  def *>(tm: Option[Matrix]): Option[Matrix] = {
    tm match {
      case None => None
      case Some(m) => {
        try {
          Some(*(m))
        }
        catch {
          case e: IllegalArgumentException => None
          case _: Throwable => None
        }
      }
    }
  }

  def *(tm: Matrix): Matrix = {
    if (colls != tm.rows) throw new IllegalArgumentException("matrix rows incompatible")
    val base = for (r <- Range(0, rows); c <- Range(0, tm.colls)) yield (r, c)
    val g = (for ((r, c) <- base) yield {
      lazy val r0 = rowKeys(r)
      lazy val c0 = tm.collKeys(c)
      val v = (r0, c0).zipped.foldLeft(0.0)((agg: Double, index: ((Int, Int), (Int, Int))) => agg + (m(index._1) * tm.m(index._2)))
      ((r, c), v)
    }).toMap
    new Matrix(g, rows, tm.colls)
  }


  def +(tm: Matrix): Matrix = {
    if (rows != tm.rows) throw new IllegalArgumentException("matrix rows incompatible")
    if (colls != tm.colls) throw new IllegalArgumentException("matrix collumns incompatible")
    val g = for (r <- Range(0, rows); c <- Range(0, colls)) yield {
      val k = (r, c)
      val v = m(k) + tm.m(k)
      (k -> v)
    }
    new Matrix(g.toMap, rows, colls)
  }

  def +>(tm: Option[Matrix]): Option[Matrix] = {
    tm match {
      case None => None
      case Some(m) => {
        try {
          Some(this + m)
        }
        catch {
          case e: IllegalArgumentException => None
          case _: Throwable => None
        }
      }
    }
  }

  def ->(tm: Option[Matrix]): Option[Matrix] = {
    tm match {
      case None => None
      case Some(m) => {
        try {
          Some(this - m)
        }
        catch {
          case e: IllegalArgumentException => None
          case _: Throwable => None
        }
      }
    }
  }

  def -(tm: Matrix): Matrix = {
    if (rows != tm.rows) throw new IllegalArgumentException("matrix rows incompatible")
    if (colls != tm.colls) throw new IllegalArgumentException("matrix collumns incompatible")
    val g = for (r <- Range(0, rows); c <- Range(0, colls)) yield {
      val k = (r, c)
      val v = m(k) - tm.m(k)
      (k -> v)
    }
    new Matrix(g.toMap, rows, colls)
  }


  def ^ = transpose

  def transpose() = {
    val g = for ((k, v) <- m) yield {
      (k.swap, v)
    }
    new Matrix(g, colls, rows)
  }

  def :|>:+(tm: Matrix): Matrix = {
    if (colls != tm.colls) throw new IllegalArgumentException("matrix collumns incompatible")
    val g = (for (((row, col), value) <- tm.toMap) yield ((row + rows, col) -> value)).toMap
    val mn = m ++ g
    new Matrix(mn, rows + tm.rows, colls)
  }

  def :|<:+(tm: Matrix): Matrix = {
    if (colls != tm.colls) throw new IllegalArgumentException("matrix collumns incompatible")
    val g = (for (((row, col), value) <- m) yield ((row + tm.rows, col) -> value)).toMap
    val mn = tm.toMap ++ g
    new Matrix(mn, rows + tm.rows, colls)
  }

  def :->:+(tm: Matrix): Matrix = {
    if (rows != tm.rows) throw new IllegalArgumentException("matrix rows incompatible")
    val g = (for (((row, col), value) <- tm.toMap) yield ((row, col + colls) -> value)).toMap
    val mn = m ++ g
    new Matrix(mn, rows, colls + tm.colls)
  }

  def :-<:+(tm: Matrix): Matrix = {
    if (rows != tm.rows) throw new IllegalArgumentException("matrix rows incompatible")
    val g = (for (((row, col), value) <- m) yield ((row, col + tm.colls) -> value)).toMap
    val mn = tm.toMap ++ g
    new Matrix(mn, rows, colls + tm.colls)
  }

  def :|>:+>(tm: Option[Matrix]): Option[Matrix] = {
    tm match {
      case None => None
      case Some(m) => {
        try {
          Some(this :|>:+ m)
        }
        catch {
          case e: IllegalArgumentException => None
          case _: Throwable => None
        }
      }
    }
  }

  def :|<:+>(tm: Option[Matrix]): Option[Matrix] = {
    tm match {
      case None => None
      case Some(m) => {
        try {
          Some(this :|<:+ m)
        }
        catch {
          case e: IllegalArgumentException => None
          case _: Throwable => None
        }
      }
    }
  }

  def :-<:+>(tm: Option[Matrix]): Option[Matrix] = {
    tm match {
      case None => None
      case Some(m) => {
        try {
          Some(this :-<:+ m)
        }
        catch {
          case e: IllegalArgumentException => None
          case _: Throwable => None
        }
      }
    }
  }

  def :->:+>(tm: Option[Matrix]): Option[Matrix] = {
    tm match {
      case None => None
      case Some(m) => {
        try {
          Some(this :->:+ m)
        }
        catch {
          case e: IllegalArgumentException => None
          case _: Throwable => None
        }
      }
    }
  }

  def expand(extra_rows: Int, extra_cols: Int, default: Double = 0) = {
    val nrows = for (row <- Range(rows, rows + extra_rows); col <- Range(0, colls)) yield ((row, col) -> default)
    val ncols = for (row <- Range(0, rows + extra_rows); col <- Range(colls, colls + extra_cols)) yield ((row, col) -> default)
    new Matrix(m ++ nrows.toMap ++ ncols.toMap, rows + extra_rows, colls + extra_cols)
  }

}
