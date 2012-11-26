package bpn

import mathext.Implicits
import breeze.linalg._
import util.DebugInfo
class Softmax(_size: Int) extends LayerInput with NetOutput with DebugInfo {
  var inputs = Seq[Connection]()

  protected var targets_cache: DenseMatrix[Double] = null
  def targets = targets_cache
  def targets_=(value: DenseMatrix[Double]) = {
    if (value.rows != size) throw new IllegalArgumentException("Bad matrix size")
    targets_cache = value
  }

  def size = _size

  protected var y_cache: DenseMatrix[Double] = null
  def y = y_cache
  // TODO: avoid exp (might produce large numbers, and unstable calculation)
  protected def y_calc: DenseMatrix[Double] = {
     val sumz: DenseMatrix[Double] = (for(i<-inputs) yield i.z).fold(DenseMatrix.zeros[Double](size,inputs(0).z.cols))((a:DenseMatrix[Double],b:DenseMatrix[Double])=> (a + b).asInstanceOf[DenseMatrix[Double]] ) 
     val expterms = sumz.values.map(math.exp _) 
     val normalizing = sum(expterms,Axis._1)
     expterms :/ DenseMatrix.tabulate(normalizing.size,expterms.cols)((r,c) =>normalizing(r))
  }

  protected var dEdz_cache: DenseMatrix[Double]  = null
  def dEdz = dEdz_cache
  protected def dEdz_calc: DenseMatrix[Double] = {
    y - targets
  }

  def avgSumErr = {
    val sumErr = (y :* targets).values.map(x=> -math.log(x)).sum 
    sumErr / y.cols
  }

  def backward(): Unit = {
    dEdz_cache = dEdz_calc
    inputs foreach { i ⇒ i.backward() }
  }

  protected var fwdCount = 0

  override def forward() = {
    fwdCount += 1
    if (fwdCount == inputs.size) {
      fwdCount = 0
      y_cache = y_calc
      super.forward()
    }
  }
}
