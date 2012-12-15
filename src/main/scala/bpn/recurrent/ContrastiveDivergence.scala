package bpn.recurrent
import breeze.linalg._
import breeze.data._
import mathext.Implicits._
import annotation.tailrec
import bpn._
import data._

class ContrastiveDivergence(_n: Int, val rbm: RBM, val data: Seq[VectorObservation]) {
  var batchSize = data.size
  val dataset = new Dataset(data) with CatableDataset[MatrixObservation,VectorObservation]
  def n = _n
  def learnCycle() = {
    rbm.tie = Some(dataset.nextMulti(batchSize).features)
    rbm.updateHidden
    val dataVis = rbm.visExpect
    val dataHid = rbm.hidExpect
    for (i <- (1 to n)) rbm.updateCycle()
    val modelVis = rbm.visExpect
    val modelHid = rbm.hidExpect
    val deltaW = (dataVis * dataHid.t) - (modelVis * modelHid.t)
    rbm.weights += deltaW 
  }

  @tailrec final def learn(iters:Int):Unit={ 
    if(iters<=0) return
    learnCycle()
    learn(iters-1)
  }
}
