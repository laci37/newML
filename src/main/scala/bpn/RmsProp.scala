package bpn
import breeze.linalg._
import math._
class RmsProp extends GradientDescent {
  protected var meanSquare: DenseMatrix[Double] = null
  var residual = 0.9
  def getDelta(gradient: DenseMatrix[Double]) = {
    if (meanSquare == null) meanSquare = gradient.values.map(x => x * x)
    else {
      meanSquare = meanSquare :* residual :+ gradient.values.map(x => x * x) :* (1 - residual)
    }
    gradient :/ meanSquare :* -1d
  }
}
