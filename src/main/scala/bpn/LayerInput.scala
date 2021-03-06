package bpn
import mathext._
import util.DebugInfo
import breeze.linalg._
trait LayerInput extends DebugInfo{
  def size: Int
  def dEdz: DenseMatrix[Double]
  var inputs:Seq[Connection]
  def forward()
  def learn()
  def backward()
  def getWeightSeq:List[Matrix[Double]]
  def loadWeights(data:List[Matrix[Double]]):List[Matrix[Double]]
}
