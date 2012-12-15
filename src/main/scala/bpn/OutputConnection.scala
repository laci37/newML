package bpn
import breeze.linalg._
class OutputConnection(val in: LayerOutput, Err: (Double, Double) => Double, dErr: (Double, Double) ⇒ Double) extends ConnectionInput with NetOutput {
  //structure setup
  in.outputs = in.outputs :+ this

  protected var targets_cache: DenseMatrix[Double] = null
  def targets = targets_cache
  def targets_=(value: DenseMatrix[Double]) = {
    if (value.rows != in.size) throw new IllegalArgumentException("Bad matrix size")
    targets_cache = value
  }

  def dEdy = {
    DenseMatrix.tabulate(in.y.rows, in.y.cols)((i, j) => dErr(targets(i, j), in.y(i, j)))
  }

  def backward() = {
    if (bpn.verbosity >= 100) println(this + " backward begin")
    in.backward()
  }

  def avgSumErr = {
    (for (i <- (0 to targets.rows - 1)) yield (for (j <- 0 to targets.cols - 1) yield Err(targets(i, j), in.y(i, j))).sum).sum  / targets.cols
  }

  override def forward() = {
    if (bpn.verbosity >= 80) println(this + "Error: " + avgSumErr)
    super.forward()
  }
  
  def size=in.size
}
