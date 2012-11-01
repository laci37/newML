package mathext

trait Matrix {
  val rows: Int
  val cols: Int
  //def clone(): Matrix
  def apply(row: Int): RVector
  def apply(row: Int, col: Int): Double
  def update(row: Int, col: Int, value: Double)
  def +=(that: Matrix)
  def -=(that: Matrix)
  def *=(that: Double)
  def ×(that: Matrix): Matrix
  def -(that: Matrix): Matrix
  def +(that: Matrix): Matrix
  def *(that: Double): Matrix
  def toCVector(): CVector
  def toRVector(): RVector
  def transpose: Matrix
  def applyFun(f: Double ⇒ Double): Matrix
  def applyFun(f: (Int, Int, Double) ⇒ Double): Matrix

  def display() = {
    for (i <- (0 to rows - 1)) {
      for (j <- (0 to cols - 1)) print(apply(i, j) + " ")
      println()
    }
  }

  override def toString() = {
    var res = ""
    for (i <- (0 to rows - 1)) {
      for (j <- (0 to cols - 1)) res += (apply(i, j) + " ")
      res += "\n"
    }
    res
  }

}

object Matrix {
  def apply(rows: Int, cols: Int) = new MatrixBase(rows, cols)

  def apply(data: Seq[RVector]) = {
    new MatrixBase(
      (for (v <- data) yield v.toArray).toArray)
  }

  def fill(rows: Int, cols: Int)(f: ⇒ Double) = {
    new MatrixBase(
      (for (i ← 0 to rows - 1) yield (for (j ← 0 to cols - 1) yield f).toArray).toArray)
  }

  //causes ambigous references
  /*def fill(rows: Int, cols: Int)(f: (Int, Int) ⇒ Double) = {
    new MatrixBase(
      (for (i ← 0 to rows - 1) yield
        (for (j ← 0 to cols - 1) yield
        f(i, j)).toArray
      ).toArray)
  }*/
}

class MatrixBase(data: Array[Array[Double]]) extends Matrix {
  protected val arr = data

  val rows = data.size
  val cols = data(0).size

  def this(rows: Int, cols: Int) = {
    this(Array.fill(rows)(Array.fill(cols)(0d)))
  }

  override def clone() = {
    val res = new MatrixBase(rows, cols)
    for (i ← (0 to rows - 1)) {
      System.arraycopy(arr(i), 0, res.arr(i), 0, cols)
    }
    res
  }

  def apply(row: Int) = RVector(arr(row))

  def apply(row: Int, col: Int) = arr(row)(col)

  def update(row: Int, col: Int, value: Double) = arr(row)(col) = value

  def +=(that: Matrix) = {
    if (that.cols != this.cols || that.rows != this.rows)
      throw new MatrixException("Dimensions should be equal")
    for (i ← (0 to rows - 1)) for (j ← (0 to cols - 1)) {
      this(i, j) += that(i, j)
    }
    this
  }

  def -=(that: Matrix) = {
    if (that.cols != this.cols || that.rows != this.rows)
      throw new MatrixException("Dimensions should be equal")
    for (i ← (0 to rows - 1)) for (j ← (0 to cols - 1)) {
      this(i, j) -= that(i, j)
    }
    this
  }

  def *=(that: Double) = {
    for (i ← (0 to rows - 1)) for (j ← (0 to cols - 1)) {
      this(i, j) *= that
    }
    this
  }

  def ×(that: Matrix): Matrix = {
    if (that.rows != this.cols)
      throw new MatrixException("Dimensions should be equal")
    val res = new MatrixBase(this.rows, that.cols)
    for (i ← (0 to rows - 1)) for (j ← (0 to that.cols - 1)) for (k ← (0 to cols - 1)) {
      res(i, j) += this(i, k) * that(k, j)
    }
    res
  }

  def +(that: Matrix) = {
    var res = this.clone
    res += that
    res
  }

  def -(that: Matrix) = {
    var res = this.clone
    res -= that
    res
  }

  def *(that: Double) = {
    var res = this.clone
    res *= that
    res
  }

  def toCVector(): CVector = {
    if (cols != 1) throw new MatrixException("Not a CVec")
    val res = CVector(rows)
    for (i ← (0 to rows - 1)) res(i, 0) = arr(i)(0)
    res
  }

  def toRVector(): RVector = {
    if (rows != 1) throw new MatrixException("Not an RVec")
    RVector(arr(0))
  }

  def transpose: Matrix = {
    val res = Matrix(cols, rows)
    for (i ← (0 to rows - 1)) for (j ← (0 to cols - 1)) {
      res(j, i) = this(i, j)
    }
    res
  }

  def applyFun(f: Double ⇒ Double): Matrix = {
    val res = Matrix(rows, cols)
    for (i ← (0 to rows - 1)) for (j ← (0 to cols - 1)) {
      res(i, j) = f(this(i, j))
    }
    res
  }

  def applyFun(f: (Int, Int, Double) ⇒ Double): Matrix = {
    val res = Matrix(rows, cols)
    for (i ← (0 to rows - 1)) for (j ← (0 to cols - 1)) {
      res(i, j) = f(i, j, this(i, j))
    }
    res
  }
}

class MatrixException(val msg: String) extends Exception(msg)
