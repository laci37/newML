package bpn
import breeze.linalg._
import mathext.Implicits._
import util.DebugInfo
class Connection(val in: LayerOutput, val out: LayerInput, var gd: GradientDescent) extends ConnectionInput with DebugInfo {
  //setup structure
  in.outputs = in.outputs :+ this
  out.inputs = out.inputs :+ this

  var weights = DenseMatrix.tabulate(in.size,out.size)((r,c)=>rand.nextDouble()-0.5)

  var z_cache: DenseMatrix[Double] = null //z values, columns represent samples, rows represent neurons 
  def z = z_cache
  protected def z_calc = (in.y.t * weights).t

  var dEdy_cache: DenseMatrix[Double] = null //error derivatives for incoming outputs cols represent samples, rows neurons
  def dEdy = dEdy_cache
  protected def dEdy_calc: DenseMatrix[Double] = (weights * out.dEdz)

  var dEdw_cache: DenseMatrix[Double] = null
  def dEdw = dEdw_cache
  protected def dEdw_calc: DenseMatrix[Double] = {
    (in.y * out.dEdz.t) :/ in.y.cols.toDouble
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
