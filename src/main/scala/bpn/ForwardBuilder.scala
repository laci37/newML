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

  //implicit casts not belonging to member classes
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
    
    def withConstraint(cg:String)={ 
      new ConnectionConstraint(cg)
    }
    
  }

  object make {
    def output(layer: LayerOutput) = {
      outputs = new OutputConnection(layer, errFun, derrFun) :: outputs
    }
  }

  object create {
    def input(size: Int) = {
      val res = new NameWrapper(new InputLayer(size))
      inputs = res.inner :: inputs
      res
    }

    def sigmoid(size: Int) = {
      val res = new NameWrapper(new SigmoidLayer(size))
      if (autoBias) connection from bias to res.inner
      res
    }

    def linear(size: Int) = {
      val res = new NameWrapper(new LinearLayer(size))
      if (autoBias) connection from bias to res.inner
      res
    }

    def binStoch(size:Int)={ 
      val res = new NameWrapper(new BinaryStochasticLayer(size))
      if (autoBias) connection from bias to res.inner
      res
    }

    def softmax(size: Int) = {
      val res = new NameWrapper(new Softmax(size))
      if (autoBias) connection from bias to res.inner
      outputs = res :: outputs
      res
    }
  }

  //member classes and their implicit casts
  class ConnectionConstraint(val cg: String) {
    def from(in: LayerOutput) = {
      val res = new PartialConnection(in)
      res.cg = Some(cg)
      res
    }
  }

  class PartialConnection(val in: LayerOutput) {
    var cg: Option[String] = None
    /**
     * used in 'connection from ... to ...'
     */
    def to(out: LayerInput) = {
      new FullConnection(in, out, cg)
    }

  }

  implicit def FullConnection2Connection(meta: FullConnection): Connection = meta.inner

  class FullConnection(val in: LayerOutput, val out: LayerInput, val cg: Option[String]) {
    val inner = if (cg.isEmpty) new Connection(in, out, gd())
    else {
      import collection.mutable.Set
      val res = new ConstraintedConnection(in, out, gd())
      val cgSet = nameMap(cg.get).asInstanceOf[Set[ConstraintedConnection]]
      cgSet foreach { con => con addConstraint res }
      cgSet += res
      res
    }
  }

  implicit def NameWrapperExtract[T <: DebugInfo](nw: NameWrapper[T]): T = nw.inner

  class NameWrapper[T <: DebugInfo](val inner: T) {
    def named(name: String):T = {
      nameMap(name) = inner
      inner.debug = name
      inner
    }
  }
}
