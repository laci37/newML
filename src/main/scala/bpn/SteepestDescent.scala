package bpn
import mathext._
class SteepestDescent(var mu: Double) extends GradientDescent {
  def getDelta(deriv: Matrix) = {
     deriv *(-mu)
  }
}
