package bpn.recurrent
import mathext._
/**
 * class for representing a restricted Boltzmann machine
 */
class RBM(val visibleSize: Int, val hiddenSize: Int) {
  var weights: Matrix

  protected var _visExpect: RVector = RVector(visibleSize) //expectation values for states of units
  protected var _hidExpect: RVector = RVector(hiddenSize)
  var tie: Option[RVector] = None // setting this to Some(_) ties visible state vector to the value 

  def visExpect = _tie.getOrElse(_visExpect)
  def hidExpect = _hidExpect

  def sigma(x: Double) = 1d / (1 + math.exp(-x))
  
  /**
   * updates visible vectors axpected value, resets tie
   */
  def updateVisible() = {
    _tie = None
    _visExpect = (hidExpect × weights).applyFun(sigma _).toRVector
  }

  def updateHidden() = {
    _hidExpect = (visExpect × weights.transpose).applyFun(sigma _).toRVector
  }

  def randomizeVisible() = {
    _tie = Some(Matrix.fill(1, visibleSize)(0.5 ? 1d | 0d).toRVector)
  }

  def sampleHidden() = hidExpect.applyFun { d: Double => d ? 1d | 0d }
  def sampleVisible() = visExpect.applyFun { d: Double => d ? 1d | 0d }

  def updateCycle()= updateVisible(); updateHidden()
}

