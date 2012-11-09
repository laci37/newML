package bpn.recurrent
import bpn._
class AbstractNet(inputs: Seq[AbstractInputLayer], outputs: Seq[AbstractLayerOutput]) {
  def buildForwardNet(gd: () => GradientDescent, timeDepth: Int): Net = {
    val bdr = new ForwardBuilder
    bdr.gd=gd
    for(i<-(1 to timeDepth)){ 
      import bdr._
      
    }
    bdr.build
  }
}
