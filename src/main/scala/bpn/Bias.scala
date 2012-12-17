package bpn
import  breeze.linalg._
class BiasLayer extends LayerOutput {
  val size = 1
  var y: Matrix[Double] = null
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

  def setBatchSize(size: Int) = {
    y = DenseVector.ones[Double](size).t
  }
}
