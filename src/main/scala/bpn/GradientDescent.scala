package bpn
import mathext._
trait GradientDescent {
  def getDelta(deriv:Matrix):Matrix
}
