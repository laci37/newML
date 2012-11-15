package bpn.recurrent
import mathext._
/**
 * class for representing a restricted Boltzmann machine
 */
class RBM(val visibleSize: Int, val hiddenSize: Int) {
  var weights: Matrix

  var visState: RVector = RVector(visibleSize)
  var hidState: RVector = RVector(hiddenSize)
  
  def sigma(x: Double) = 1d / (1 + math.exp(-x))

  def updateVisible() = {
    visState = (hidState × weights).applyFun(sigma _).toRVector
  }

  def updateHidden() = {
    hidState = (visState × weights.transpose).applyFun(sigma _).toRVector
  }

  def sampleHidden() = hidState.applyFun { d: Double => d ? 1d | 0d }
  def sampleVisible() = visState.applyFun { d: Double => d ? 1d | 0d }

}

