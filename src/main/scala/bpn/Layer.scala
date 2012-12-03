package bpn

import util.DebugInfo
import breeze.linalg._
class Layer(_size: Int, actFun: (Double ⇒ Double), dactFun: (Double ⇒ Double)) extends LayerOutput with LayerInput {

  var inputs = Seq[Connection]()

  protected var fwd_count = 0

  var outputs = Seq[ConnectionInput]()

  protected var back_count = 0
  def size = _size

  protected var y_cache: DenseMatrix[Double] = null
  protected var sumz: DenseMatrix[Double] = null
  def y = y_cache
  protected def y_calc: DenseMatrix[Double] = {
    sumz = (for (i ← inputs) yield i.z).reduceLeft(_ + _)
    sumz.values.map(actFun)
  }

  protected var dEdz_cache: DenseMatrix[Double] = null
  def dEdz = dEdz_cache
  protected def dEdz_calc: DenseMatrix[Double] = {
    val sumdEdy = (for (o ← outputs) yield o.dEdy).reduceLeft(_ + _)
    val dydz = sumz.values.map(dactFun)
    sumdEdy :* dydz
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

//some basic layer classes
class SigmoidLayer(size: Int) extends Layer(size, SigmoidLayer.sig _, SigmoidLayer.dsig _)

object SigmoidLayer {
  import math._
  def sig(x: Double) = 1d / (1d + exp(-x))
  def dsig(x: Double) = sig(x) * (1d - sig(x))
}

class LinearLayer(size: Int) extends Layer(size, identity, x => 1d)

class BinaryStochasticLayer(size:Int) extends Layer(size, BinaryStochasticLayer.act _, SigmoidLayer.dsig _)

object BinaryStochasticLayer { 
  import mathext.Implicits._
  def act(x:Double)= SigmoidLayer.sig(x) ? 1d | 0d
}
