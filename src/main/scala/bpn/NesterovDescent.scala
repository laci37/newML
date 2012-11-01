package bpn
import mathext._
class NesterovDescent extends GradientDescent {
  var learningRate = 0.1
  var friction = 0.9
  protected var velocity: Matrix = null
  def getDelta(gradient: Matrix) = {
    if (velocity == null) velocity = Matrix(gradient.rows, gradient.cols)
    else velocity = gradient * (-learningRate) + velocity * friction
    velocity
  }
}
