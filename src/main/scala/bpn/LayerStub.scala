package bpn
import breeze.linalg._

//somewhy the compiler can't find package private classes
/*private[bpn]*/ trait LayerStubSupervisor{ 
  def backward()
}

/*private[bpn]*/ class LayerStub(val size:Int,sup:LayerStubSupervisor) extends LayerOutput{ 
  var y:DenseMatrix[Double]=null
  var outputs:Seq[ConnectionInput]=Seq()
  var sumdEdy:DenseMatrix[Double]=null
  def backward()={
    sumdEdy = outputs.map(_.dEdy).reduceLeft(_ + _)
    sup.backward
  }
  def forward()= outputs foreach (_.forward())
  def learn()= outputs foreach (_.learn())
  def getWeightSeq:List[Matrix[Double]]= (outputs flatMap (_.getWeightSeq)).toList
  def loadWeights(data:List[Matrix[Double]]):List[Matrix[Double]]= outputs.foldLeft(data)((d,o)=>o.loadWeights(d))
}
