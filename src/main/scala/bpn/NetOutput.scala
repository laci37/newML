package bpn
import breeze.linalg._
/**
 * Trait for calsses representing an output from a neural network
 */
trait NetOutput {
  /**
   * target values to calculate the error and error derivatives from.
   */
  var targets: DenseMatrix[Double]
  
  /**
   * Calling this method starts a backward pass. To do the full pass,
   * backward must be called on all outputs of the network sequentially
   */
  def backward()

  /**
   * this funcion will be called if a forward pass ends at this output
   */
  var forwardCallback: Option[(Any) => Unit] = None
  
  /**
   * this funcion will be called if a learning pass ends at this output
   */
  var learnCallback: Option[(Any) => Unit] = None

  def forward() = {
    if (bpn.verbosity >= 100) println(this + " forward end")
    if (forwardCallback.isDefined) forwardCallback.get(this)
  }

  def learn() = {
    if (bpn.verbosity >= 100) println(this + " learn end")
    if (learnCallback.isDefined) learnCallback.get(this)
  }

  /**
   * The error summed over all units of this output, and avareged over
   * all training cases
   */
  def avgSumErr:Double

  def size:Int
}
