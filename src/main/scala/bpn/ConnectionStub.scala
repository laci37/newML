package bpn
import breeze.linalg._

//compiler can't find package private classes
/*private[bpn]*/ trait ConnectionStubSupervisor{ 
  def forward()
  def learn()
  def getWeightSeq:List[Matrix[Double]]
  def loadWeights(data:List[Matrix[Double]]):List[Matrix[Double]]
}

/*private[bpn]*/ class ConnectionStub(in:LayerOutput, sup:ConnectionStubSupervisor) extends ConnectionInput{ 
  in.outputs = in.outputs :+ this
  var dEdy:DenseMatrix[Double]=null
  def forward()= sup.forward()
  def learn()=sup.learn()
  def getWeightSeq= sup.getWeightSeq
  def loadWeights(data:List[Matrix[Double]]) = sup.loadWeights(data)
}
