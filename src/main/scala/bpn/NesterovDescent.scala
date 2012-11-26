package bpn
import breeze.linalg._
class NesterovDescent extends GradientDescent {
  var learningRate = 0.1
  var friction = 0.9
  protected var velocity: DenseMatrix[Double] = null
  def getDelta(gradient: DenseMatrix[Double]) = {
    if (velocity == null) velocity = DenseMatrix.zeros[Double](gradient.rows, gradient.cols)
    else velocity = gradient * (-learningRate) + velocity * friction
    velocity
  }
}
