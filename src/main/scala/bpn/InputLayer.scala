package bpn
import mathext._
import breeze.linalg._
class InputLayer(override val size: Int) extends LayerOutput {
  protected var y_cache: DenseMatrix[Double] = null
  def y = y_cache
  def y_=(value: DenseMatrix[Double]) = {
    if (value.rows != size) throw new IllegalArgumentException("Bad matrix size")
    y_cache = value
  }

  var outputs = Seq[ConnectionInput]()
  var backwardCallback: Option[(Any) ⇒ Unit] = None
  def backward() = {
    if (bpn.verbosity >= 100) println(this + " backward end")
    if (backwardCallback.isDefined) backwardCallback.get(this)
  }

  def forward() = {
    if (bpn.verbosity >= 100) println(this + " forward begin")
    outputs foreach { o ⇒ o.forward() }
  }
  def learn() = {
    if (bpn.verbosity >= 100) println(this + " learn begin")
    outputs foreach { o ⇒ o.learn() }
  }
}
