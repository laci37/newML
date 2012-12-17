package bpn
import breeze.linalg._
import data._
/**
 * Class for managing a neural network.
 */
class Net(val inputs: Seq[InputLayer], val outputs: Seq[NetOutput], val bias: BiasLayer) {

  /**
   * Sets all the inputs to the network
   */
  def setInputs(data: Seq[Matrix[Double]]) = {
    for (i ← (0 to inputs.size - 1)) inputs(i).y = data(i)
    bias.setBatchSize(data(0).cols)
  }

  def setInputs(data:Matrix[Double])={
    var off=0
    for(i<-inputs) { 
      i.y=data(off until off+i.size,::)
      off+=i.size
    }
    bias.setBatchSize(data.cols)
  }
  /**
   * Sets the target outputs of the network
   */
  def setTargets(data: Seq[Matrix[Double]]) = {
    for (i ← (0 to outputs.size - 1)) outputs(i).targets = data(i)
  }

  def setTargets(data:Matrix[Double])={ 
    var off=0
    for(o<-outputs){ 
      o.targets=data(off until off+o.size,::)
      off+=o.size
    }
  }

  def loadExample(ex: MatrixExample)={ 
    setInputs(ex.features)
    setTargets(ex.label)
  }

  /**
   * Does a forward pass on the network, calculating the output.
   */ 
  def forwardProp() = {
    inputs foreach { i ⇒ i.forward() }
    bias.forward()
    if (bpn.verbosity >= 70) {
      print("Overall error:")
      println((for (o <- outputs) yield o.avgSumErr).sum)
    }
  }
  
  /**
   * Does a backward pass on the network, calculating error derivatives
   * using outputs form the forward pass. Will fail with
   * NullReferenceException if there is no preceding forward pass.
   */
  def backProp() = {
    outputs foreach { o ⇒ o.backward() }
  }

  /**
   * Does a forward and a backward pass.
   */  
  def propCycle() = {
    forwardProp()
    backProp()
  }
  
  /**
   * Does a forward, a backward and a learning pass on the network
   */
  def fullCycle() = {
    propCycle()
    learn()
  }

  /**
   * Does a learning pass on the network, changing the weights using
   * error derivatives from the backward pass. Will fail with
   * NullReferenceException if there is no preceding backward pass.
   */ 
  def learn() = {
    inputs foreach { i ⇒ i.learn() }
    bias.learn()
  }

  def avgSumErr:Double={ 
    (for(o<-outputs) yield o.avgSumErr).sum
  }
}
