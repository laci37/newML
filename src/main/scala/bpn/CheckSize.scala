package bpn
import mathext._

/**
 * Mixin for net, that checks that all inputs and targets have the same number of examples. Checking is done before forward pass.
 */
trait CheckSize extends Net {

  /**
   * Returns true if all inputs and targets have the same number of examples
   */
  def checkSizes() = {
    val sizeGuess = inputs(0).y.rows
    (inputs forall { i: InputLayer => i.y.rows == sizeGuess }) &&
      (outputs forall { o: NetOutput => o.targets.rows == sizeGuess })
  }

  override def forwardProp() = {
    if (!checkSizes) throw new Exception("Wrong sized matrix!")
    super.forwardProp()
  }
}
