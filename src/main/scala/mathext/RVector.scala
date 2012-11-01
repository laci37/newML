package mathext
trait RVector extends Matrix {
  override val rows=1
}

object RVector{
  def apply(cols:Int):RVector= new RVectorBase(cols)
  def apply(data:Array[Double]):RVector=new RVectorBase(data)
}

class RVectorBase(override val cols:Int) extends MatrixBase(1,cols) with RVector{
  def this(data:Array[Double])={
    this(data.size)
    System.arraycopy(data,0,arr(0),0,cols)
  }
  
  override def transpose()=CVector(arr(0))
}