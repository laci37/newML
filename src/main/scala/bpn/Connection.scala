package bpn
import mathext._
import util.DebugInfo
class Connection(val in: LayerOutput, val out: LayerInput, var gd: GradientDescent) extends ConnectionInput with DebugInfo {
  //setup structure
  in.outputs = in.outputs :+ this
  out.inputs = out.inputs :+ this

  var weights = Matrix.fill(in.size, out.size)(scala.util.Random.nextDouble() - 0.5)

  var z_cache: Matrix = null //z values, rows represent samples, columns represent neurons 
  def z = z_cache
  protected def z_calc = (in.y × weights)

  var dEdy_cache: Matrix = null //error derivatives for incoming outputs rows represent samples, columns neurons
  def dEdy = dEdy_cache
  protected def dEdy_calc: Matrix = (weights × out.dEdz.transpose).transpose

  var dEdw_cache: Matrix = null
  def dEdw = dEdw_cache
  protected def dEdw_calc: Matrix = {
    in.y.transpose × out.dEdz * (1d/in.y.rows)
  }

  def forward() = {
    if (bpn.verbosity >= 100) println(this + " forward")
    z_cache = z_calc
    if (bpn.verbosity >= 200) println(this + " z=" + z)
    out.forward()
  }

  def backward() = {
    if (bpn.verbosity >= 100) println(this + " backward")
    dEdy_cache = dEdy_calc
    dEdw_cache = dEdw_calc
    if (bpn.verbosity >= 200) {
      println(this + " dEdy=" + dEdy + "\ndEdw=" + dEdw)
    }
    in.backward()
  }

  def learn() = {
    if (bpn.verbosity >= 100) println(this + " learn")
    if (bpn.verbosity >= 200) println(this + " before w=" + weights)
    weights += gd.getDelta(dEdw)
    if (bpn.verbosity >= 200) println(this + " after w=" + weights)
    out.learn()
  }



}
