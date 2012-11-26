package bpn
import breeze.linalg._
trait GradientDescent {
  def getDelta(deriv:DenseMatrix[Double]):DenseMatrix[Double]
}
