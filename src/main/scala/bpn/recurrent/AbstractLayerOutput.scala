package bpn.recurrent

trait AbstractLayerOutput{ 
  var outputs: Set[AbstractLayerInput]
  val size: Int
}
