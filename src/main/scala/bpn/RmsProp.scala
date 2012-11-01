package bpn
import mathext._
import math._
class RmsProp extends GradientDescent {
  protected var meanSquare: Matrix = null
  var residual = 0.9
  def getDelta(gradient: Matrix) = {
    if (meanSquare == null) meanSquare = gradient.applyFun(x => x * x)
    else {
      meanSquare = meanSquare * residual + gradient.applyFun(x => x * x) * (1 - residual)
    }
    gradient.applyFun((i, j, d) => -d / sqrt(meanSquare(i, j)))
  }
}
