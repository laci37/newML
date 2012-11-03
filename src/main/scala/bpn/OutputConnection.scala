package bpn
import mathext._
class OutputConnection(in: LayerOutput, Err: (Double, Double) => Double, dErr: (Double, Double) ⇒ Double) extends ConnectionInput with NetOutput {
  //structure setup
  in.outputs = in.outputs :+ this

  protected var targets_cache: Matrix = null
  def targets = targets_cache
  def targets_=(value: Matrix) = {
    if (value.cols != in.size) throw new IllegalArgumentException("Bad matrix size")
    targets_cache = value
  }

  def dEdy = {
    in.y.applyFun((i, j, d) => dErr(targets(i, j), d))
  }

  def backward() = {
    if (bpn.verbosity >= 100) println(this + " backward begin")
    in.backward()
  }

  def avgSumErr = {
    (for (i <- (0 to targets.rows - 1)) yield (for (j <- 0 to targets.cols - 1) yield Err(targets(i, j), in.y(i, j))).sum).sum / targets.rows
  }

  override def forward() = {
    if (bpn.verbosity >= 80) println(this + "Error: " + avgSumErr)
  }
  super.forward()
}
