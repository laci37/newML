package bpn
import mathext._
import util.DebugInfo
class Layer(_size: Int, actFun: (Double ⇒ Double), dactFun: (Double ⇒ Double)) extends LayerOutput with LayerInput {

  var inputs = Seq[Connection]()

  protected var fwd_count = 0

  var outputs = Seq[ConnectionInput]()

  protected var back_count = 0
  def size = _size

  protected var y_cache: Matrix = null
  protected var sumz: Matrix = null
  def y = y_cache
  protected def y_calc: Matrix = {
    sumz = (for (i ← inputs) yield i.z).fold(Matrix(inputs(0).z.rows, size)) { (a, b) ⇒ (a + b) }
    sumz.applyFun(actFun)
  }

  protected var dEdz_cache: Matrix = null
  def dEdz = dEdz_cache
  protected def dEdz_calc: Matrix = { // check
    val sumdEdy = (for (o ← outputs) yield o.dEdy).fold(Matrix(inputs(0).z.rows, size))((a, b) ⇒ (a + b))
    val dydz = sumz.applyFun(dactFun)
    sumdEdy.applyFun { (i: Int, j: Int, d: Double) => d * dydz(i, j) }
  }

  def forward(): Unit = {
    if (bpn.verbosity >= 100) println(this + " forward call")
    fwd_count += 1
    if (fwd_count >= inputs.size) {
      fwd_count = 0
      y_cache = y_calc
      if (bpn.verbosity >= 200) println(this + " y=" + y)
      if (bpn.verbosity >= 100) println(this + " forwarding")
      outputs foreach { o ⇒ o.forward() }
    }
  }

  def backward(): Unit = {
    if (bpn.verbosity >= 100) println(this + " backward call")
    back_count += 1
    if (back_count >= outputs.size) {
      back_count = 0
      dEdz_cache = dEdz_calc
      if (bpn.verbosity >= 200) println(this + "dEdz=" + dEdz)
      if (bpn.verbosity >= 100) println(this + " backwarding")
      inputs foreach { i ⇒ i.backward() }
    }
  }

  def learn() = {
    if (bpn.verbosity >= 100) println(this + " learn")
    outputs foreach { o ⇒ o.learn() }
  }
}

class SigmoidLayer(size: Int) extends Layer(size, SigmoidLayer.sig _, SigmoidLayer.dsig _)

object SigmoidLayer {
  import math._
  def sig(x: Double) = 1d / (1d + exp(-x))
  def dsig(x: Double) = sig(x) * (1d - sig(x))
}

class LinearLayer(size: Int) extends Layer(size, identity, x => 1d)
