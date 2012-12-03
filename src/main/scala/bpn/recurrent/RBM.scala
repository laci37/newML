package bpn.recurrent
import breeze.linalg._
import mathext.Implicits._
/**
 * class for representing a restricted Boltzmann machine
 */
class RBM(val visibleSize: Int, val hiddenSize: Int) {
  //bias is included as an extra element in all expectation vector with value of 1
  var weights = DenseMatrix.rand(visibleSize+1, hiddenSize+1)

  protected var _visExpect: DenseMatrix[Double] = DenseMatrix.zeros(visibleSize, 1) //expectation values for states of units
  protected var _hidExpect: DenseMatrix[Double] = DenseMatrix.zeros(hiddenSize, 1)
  var tie: Option[DenseMatrix[Double]] = None // setting this to Some(_) ties visible state vector to the value 

  //access methods for expectations, these concatenate bias elements
  def visExpect = {
    val expect = tie.getOrElse(_visExpect)
    DenseMatrix.vertcat(expect, DenseMatrix.ones[Double](1, expect.cols))
  }
  def hidExpect = DenseMatrix.vertcat(_hidExpect,DenseMatriy.ones[Double](1,_hidExpect.cols))

  var temp=1d
  def sigma(x: Double) = 1d / (1 + math.exp(-x/temp))

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

  def goodness(visible: DenseMatrix[Double], hidden: DenseMatrix[Double])={ 
    if(visible.rows!=visibleSize || hidden.rows!=hiddenSize || visible.cols!=hidden.cols)
      throw new IllegalArgumentException
    val hiddenWithBias=DenseMatrix.vertcat(hidden,DenseMatrix.ones[Double](1,hidden.cols))
    val visibleWithBias=DenseMatrix.vertcat(visible,DenseMatrix.ones[Double](1,visible.cols))
    sum((weights * hiddenWithBias) :* visibleWithBias, Axis._0)
  }
}

