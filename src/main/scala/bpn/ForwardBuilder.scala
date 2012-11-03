package bpn
import util.DebugInfo
import collection.mutable.Map
class ForwardBuilder {
  //learning parameters
  var gd: () => GradientDescent = () => new SteepestDescent(0.1)
  var errFun: (Double, Double) => Double = (t, y) => math.pow(t - y, 2) / 2
  var derrFun: (Double, Double) => Double = (t, y) => y - t

  //genaration options
  var autoBias = true

  //bias layer
  val bias = new BiasLayer
  //metadata of the net
  val nameMap = Map[String, Any]()
  var outputs = List[NetOutput]()
  var inputs = List[InputLayer]()

  //member functions
  def build() = new Net(inputs.reverse, outputs.reverse, bias)
  implicit def layerInputByName(name: String): LayerInput = nameMap(name).asInstanceOf[LayerInput]
  implicit def layerOutputByName(name: String): LayerOutput = nameMap(name).asInstanceOf[LayerOutput]

  //member objects

  /**
   * used for 'connection from ... to ...'
   */
  object connection {
    def from(in: LayerOutput) = {
      new PartialConnection(in)
    }
  }

  object make {
    def output(layer: LayerOutput) = {
      outputs = new OutputConnection(layer, errFun, derrFun) :: outputs
    }
  }

  object create {
    def input(size: Int) = {
      val res = new InputLayerExt(new InputLayer(size))
      inputs = res.inner :: inputs
      res
    }

    def sigmoid(size: Int) = {
      val res = new LayerExt(new SigmoidLayer(size))
      if (autoBias) connection from bias to res.inner
    }

    def linear(size: Int) = {
      val res = new LayerExt(new LinearLayer(size))
      if (autoBias) connection from bias to res.inner
    }
  }

  //member classes and their implicit casts
  class PartialConnection(val in: LayerOutput) {
    /**
     * used in 'connection from ... to ...'
     */
    def to(out: LayerInput) = {
      new FullConnection(in, out)
    }

    /**
     * used to name the input layer
     */
    def named(name: String) = {
      nameMap(name) = in
      in.debug = name
      this
    }
  }

  implicit def FullConnection2Connection(meta: FullConnection): Connection = meta.inner
  class FullConnection(val in: LayerOutput, val out: LayerInput) {
    val inner = new Connection(in, out, gd())
    def named(name: String) = {
      nameMap(name) = out
      out.debug = name
      this
    }
  }

  implicit def InputLayerExt2InputLayer(e: InputLayerExt): InputLayer = e.inner
  class InputLayerExt(val inner: InputLayer) {
    def named(name: String) = {
      nameMap(name) = inner
      inner.debug = name
      this
    }
  }

  implicit def LayerExt2Layer(e: LayerExt): Layer = e.inner
  class LayerExt(val inner: Layer) {
    def named(name: String) = {
      nameMap(name) = inner
      inner.debug = name
      this
    }
  }
}
