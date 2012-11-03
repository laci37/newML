package bpn

import mathext._
import util.DebugInfo
class Softmax(_size: Int) extends LayerInput with NetOutput with DebugInfo {
  var inputs = Seq[Connection]()

  protected var targets_cache: Matrix = null
  def targets = targets_cache
  def targets_=(value: Matrix) = {
    if (value.cols != size) throw new IllegalArgumentException("Bad matrix size")
    targets_cache = value
  }

  def size = _size

  protected var y_cache: Matrix = null
  def y = y_cache
  // TODO: revise
  protected def y_calc: Matrix = {
    val sumz = (for (i ← inputs) yield i.z).fold(Matrix(inputs(0).z.rows, size))((a, b) ⇒ (a + b))
    val interact = sumz.applyFun(math.exp _)
    val sumactrecip = RVector(inputs(0).z.rows).applyFun((i, j, d) ⇒ 1d
      / interact(j).sum).toRVector
    interact.applyFun((i, j, d) ⇒ d * sumactrecip(0, i))
  }

  protected var dEdz_cache: Matrix = null
  def dEdz = dEdz_cache
  protected def dEdz_calc: Matrix = {
    y - targets
  }

  def avgSumErr = {
    val sumErr = for (i <- (0 to targets.rows - 1)) yield -math.log((targets(i) × y(i).transpose)(0, 0))
    sumErr.sum / sumErr.size
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
