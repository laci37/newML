package mathext

trait CVector extends Matrix {
  val cols = 1
}

object CVector {
  def apply(rows: Int) = new CVectorBase(rows)
  def apply(data: Array[Double]) = new CVectorBase(data)
}

class CVectorBase(val rows: Int) extends CVector {
  protected val arr = new Array[Double](rows)

  def this(data: Array[Double]) = {
    this(data.size)
    System.arraycopy(data, 0, arr, 0, rows)
  }

  def transpose = RVector(arr)

  def toCVector = clone().asInstanceOf[CVector]

  def toRVector = throw new MatrixException("Not an RVec")

  override def clone() = new CVectorBase(arr)

  def apply(row: Int): RVector = RVector(Array(arr(row)))

  def apply(row: Int, col: Int): Double = {
    if (col != 0) throw new MatrixException("CVector")
    arr(row)
  }

  def update(row: Int, col: Int, value: Double) = {
    if (col != 0) throw new MatrixException("CVector")
    arr(row) = value
  }

  def +=(that: Matrix) = {
    if (that.rows != rows || that.cols != 1)
      throw new MatrixException("Dimensions should be equal")
    for (i ← (0 to rows - 1)) this.arr(i) += that(i, 0)
  }

  def -=(that: Matrix) = {
    if (that.rows != rows || that.cols != 1)
      throw new MatrixException("Dimensions should be equal")
    for (i ← (0 to rows - 1)) this.arr(i) -= that(i, 0)
  }

  def *=(that: Double) = for (i ← (0 to rows - 1)) this.arr(i) *= that

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

  def applyFun(f: Double ⇒ Double): Matrix = {
    val res = CVector(rows)
    for (i ← (0 to rows - 1))
      res(i, 0) = f(this(i, 0))
    res
  }
  
  def applyFun(f:(Int,Int,Double)=>Double):Matrix={
    val res= Matrix(rows,cols)
    for(i<-(0 to rows-1)){
      res(i,0)=f(i,0,this(i,0))
    }
    res
  }
}