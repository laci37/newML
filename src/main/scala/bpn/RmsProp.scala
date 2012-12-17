package bpn
import breeze.linalg._
import math._
class RmsProp extends GradientDescent {
  protected var meanSquare: Matrix[Double] = null
  var residual = 0.9
  def getDelta(gradient: Matrix[Double]) = {
    if (meanSquare == null) meanSquare = gradient.values.map(x => x * x)
    else {
      meanSquare = (meanSquare :* residual :+ gradient.values.map(x => x * x) :* (1 - residual)).values.map(x=>if(x==x && x!=0) x else 0.01d)
    }
    gradient :/ meanSquare.values.map(sqrt _) :* -1d
  }
}
