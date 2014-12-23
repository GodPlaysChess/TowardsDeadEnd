package datastructures

/**
 * Created by Gleb on 11/8/2014.
 *
 * Matrix of ints. 
 */
class Matrix(elems: Array[Array[Int]]) {

  // Array of Arrays of rows!
  def length = elems.length

  def heigth = if (length > 0) elems(0).length else 0

  def row(num: Int) = elems(num)

  def col(num: Int): Array[Int] = elems.map(r => r(num))

  def put(row: Int, col: Int, value: Int) = {
    elems(row)(col) = value
  }

  def get(row: Int, col: Int): Int = elems(row)(col)

  def transpose = elems.transpose

  private def rows: Array[Array[Int]] = elems

  //TODO keep two arrays columns and rows, and make them sync. Will it be better?
  private def columns: Array[Array[Int]]  = elems.transpose

  /**
   * Divide and conquer Strassen algorithm for multiplying n*n matrices in n^log 7^ time
   *
   **/
  def *(that: Matrix): Matrix = {
    require(length == heigth && that.length == that.heigth, "For multiplying non square matrices use another method")

    if (length == 1) Matrix.of(get(0, 0) * that.get(0, 0))

    else {
      val (a11, a12, a21, a22) = partition
      val (b11, b12, b21, b22) = that.partition
      val c11 = a11 * b11 + a12 * b21
      val c12 = a11 * b12 + a12 * b22
      val c21 = a21 * b11 + a22 * b21
      val c22 = a21 * b12 + a22 * b22

      combine(c11, c12, c21, c22)
    }
  }

  /**
   * @return this matrix with elements added.
   * If the matrix, provided as an argument is smaller, then it treats all lacking elements as zeros
   * If the provided matrix is larger, then stretches the base matrix to the biggest size, filling lacking elements with zeros.
   */
  def +(that: Matrix): Matrix = {
    if (length < that.length && heigth < that.heigth) that + this
    else {
      rows.zip(that.rows).foreach(r => r._1.zip(r._2).map(el => el._1 + el._2))
      this
    }
  }

  def combine(a11: Matrix, a12: Matrix, a21: Matrix, a22: Matrix): Matrix = {
    (a11 ++| a12) appendBot (a21 ++| a22)
  }

  /**
   * Appends a given matrix to the right
   */
  def ++|(that: Matrix): Matrix = {
    require(heigth == that.heigth, "Please make sure that matrices have equals height")
    val result = Matrix.ofDim(length + that.length, heigth)
    val resultElems = Array.ofDim(length + that.length, heigth)

//    rows.zip(that.rows).foreach(line => resultElems(0) = line._1 ++ line._2)
      ???

  }

  /**
   * Appends a given matrix to the bottom
   * If the length of the given matrix and appended matrix are different, then
   * the smaller matrix is filled with zeroes.
   */
  def appendBot(that: Matrix): Matrix = {
    /*rows. + that.rows)*/ ???
  }

  /**
   * Splits the matrix into 4 matrices, preferably of equal size.   
   */
  private def partition: (Matrix, Matrix, Matrix, Matrix) = {
    val (first, sec) = elems.splitAt(length / 2)
    val (a11, a12) = first.map(row => row.splitAt(row.length / 2))
      .foldLeft((Matrix.ofDim(10, 0), Matrix.ofDim(10, 0)))((acc, ab) => {
      (acc._1.addRow(ab._1), acc._2.addRow(ab._2))
    })
    val (a21, a22) = sec.map(row => row.splitAt(row.length / 2))
      .foldLeft((Matrix.ofDim(10, 0), Matrix.ofDim(10, 0)))((acc, ab) => {
      (acc._1.addRow(ab._1), acc._2.addRow(ab._2))
    })
    (a11, a12, a21, a22)
  }

  def addRow(newRow: Array[Int]): Matrix = {
    new Matrix(elems :+ newRow)
  }


  object Matrix {
    def ofDim(n: Int, m: Int) = new Matrix(Array.ofDim[Int](n, m))

    def of(elem: Int) = new Matrix(Array(Array(elem)))
  }


}
