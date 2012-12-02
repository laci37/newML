package bpn
import breeze.linalg._
import breeze.data._
import annotation.tailrec
class Teacher(n: Net, dataset: Seq[BpnExample]) {
  var batchSize = dataset.size
  protected var off = 0 //the current offset in the dataset

  //shows an example to the network, but does no learning pass
  def showExample(ex:BpnMultiExample)={ 
    n.loadExample(ex)
    n.propCycle()
  }
  
  def learnCycle()={ 
    n.loadExample(getExamples())
    n.fullCycle()
  }

  @tailrec final def learn(epochs: Int):Unit={ 
    if(epochs<=0) return
    learnCycle()
    learn(epochs-1)
  }

  lazy val fullBatch = {
    dataset.foldLeft[BpnMultiExample](EmptyMultiExample)(_ + _)
  }

  def miniBatch(indexes: Seq[Int]) = {
    (for (i <- (indexes)) yield dataset(i)).foldLeft[BpnMultiExample](EmptyMultiExample)(_ + _)
  }

  
  def getExamples(): BpnMultiExample = {
    batchSize match {
      case 1 => { //online
        val res = dataset(off)
        off += 1
        res
      }
      case a if (a < dataset.size) => { //minibatch 
        if (off + batchSize <= dataset.size) {
          val res = miniBatch(off until off + batchSize)
          off += batchSize
          res
        } else {
	  val res= miniBatch((off until dataset.size) union (0 until batchSize-(dataset.size-off)))
	  off = batchSize-(dataset.size-off)
	  res
        }
      }
      case b if (b==dataset.size) => { 
	fullBatch
      }
      case _ => { 
	if(bpn.verbosity>=10) println(this+": wrong batchsize, defaulting to full batch")
	fullBatch
      }
    }
  }

  
}

class BpnExample(val inputs: Seq[DenseVector[Double]], val targets: Seq[DenseVector[Double]])
  extends Example[Seq[DenseVector[Double]], Seq[DenseVector[Double]]] {

  def features=inputs
  def label=targets
  var id=""

  def this(input: DenseVector[Double], target: DenseVector[Double]) =
    this(Seq(input), Seq(target))

  def +(that: BpnExample) = {
    val in = for (i <- 0 until inputs.length) yield DenseVector.horzcat(this.inputs(i), that.inputs(i))
    val tgt = for (i <- 0 until targets.length) yield DenseVector.horzcat(this.targets(i), that.targets(i))
    new BpnMultiExample(in, tgt)
  }

  def +(that: BpnMultiExample) = {
    val asMulti=BpnExample.cast2BpnMultiExample(this)
    asMulti + that
  }
}

object BpnExample {
  implicit def cast2BpnMultiExample(from: BpnExample): BpnMultiExample = {
    val in = for (i <- 0 until from.inputs.length) yield 
      DenseMatrix.zeros[Double](from.inputs(i).size,1) := from.inputs(i)
    val tgt = for (i <- 0 until from.targets.length) yield 
      DenseMatrix.zeros[Double](from.targets(i).size,1) := from.targets(i)
    new BpnMultiExample(in, tgt)
  }
}

class BpnMultiExample(val inputs: Seq[DenseMatrix[Double]], val targets: Seq[DenseMatrix[Double]]) {
  def +(that: BpnMultiExample):BpnMultiExample = {
    val in = for (i <- 0 until inputs.length) yield DenseMatrix.horzcat(this.inputs(i), that.inputs(i))
    val tgt = for (i <- 0 until targets.length) yield DenseMatrix.horzcat(this.targets(i), that.targets(i))
    new BpnMultiExample(in, tgt)
  }

  def +(that: BpnExample):BpnMultiExample = {
    val asMulti=BpnExample.cast2BpnMultiExample(that)
    this + asMulti
  }
}

object EmptyMultiExample extends BpnMultiExample(null,null){ 
  override def +(that: BpnMultiExample):BpnMultiExample= that
} 
