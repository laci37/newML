package bpn.recurrent
import breeze.linalg._
import mathext.Implicits._
/**
 * class for representing a restricted Boltzmann machine
 */
class RBM(val visibleSize: Int, val hiddenSize: Int) {
  var weights = DenseMatrix.rand(hiddenSize, visibleSize)

  protected var _visExpect: DenseMatrix[Double] = DenseMatrix.zeros(visibleSize,1) //expectation values for states of units
  protected var _hidExpect: DenseMatrix[Double] = DenseMatrix.zeros(hiddenSize,1)
  var tie: Option[DenseMatrix[Double]] = None // setting this to Some(_) ties visible state vector to the value 

  def visExpect = tie.getOrElse(_visExpect)
  def hidExpect = _hidExpect

  def sigma(x: Double) = 1d / (1 + math.exp(-x))

  /**
   * updates visible vectors axpected value, resets tie
   */
  def updateVisible() = {
    tie = None
    _visExpect = (weights * sampleHidden).values.map(sigma _)
  }

  def updateHidden() = {
    _hidExpect = (weights.t * sampleVisible).values.map(sigma _)
  }

  def sampleHidden() = hidExpect.values.map { d: Double => d ? 1d | 0d }
  def sampleVisible() = visExpect.values.map { d: Double => d ? 1d | 0d }

  def updateCycle() = updateVisible(); updateHidden();
}

