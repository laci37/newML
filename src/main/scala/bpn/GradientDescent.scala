package bpn
import breeze.linalg._
trait GradientDescent {
  def getDelta(deriv:Matrix[Double]):Matrix[Double]
}
