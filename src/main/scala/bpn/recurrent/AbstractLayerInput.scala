package bpn.recurrent

trait AbstractLayerInput{ 
 var inputs: Set[AbstractLayerOutput]
 val size: Int
}
