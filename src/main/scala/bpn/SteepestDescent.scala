package bpn
import breeze.linalg._
class SteepestDescent(var mu: Double) extends GradientDescent {
  def getDelta(deriv: Matrix[Double]) = {
     deriv *(-mu)
  }
}
