package bpn
import breeze.linalg._
class Teacher(val n: Net, var examples: (Seq[Seq[DenseVector[Double]]], Seq[Seq[DenseVector[Double]]])) {
  var batchsize = examples._1.size
  var random = false

  def showExample(ex: (Seq[DenseMatrix[Double]], Seq[DenseMatrix[Double]])) = {
    if (bpn.verbosity >= 100) println("show example")
    n.setInputs(ex._1)
    n.setTargets(ex._2)
    n.propCycle()
  }

  def fullBatchMatrices(): (Seq[DenseMatrix[Double]], Seq[DenseMatrix[Double]]) = {
    val inputs = for (i <- (0 to examples._1(0).size - 1)) yield //iterate inputs
    //iterate over all examples and construct matrix
    DenseVector.horzcat[Double](examples._1(i) :_*)
    val targets = for (i <- (0 to examples._2(0).size - 1)) yield //iterate outputs
    //iterate over all examples and construct matrix
      DenseVector.horzcat[Double](examples._2(i) :_*)

    (inputs, targets)
  }

  def miniBatchMatrices(indexes: Seq[Int]): (Seq[DenseMatrix[Double]], Seq[DenseMatrix[Double]]) = {
    val inputs = for (i <- (0 to examples._1(0).size - 1)) yield //iterate inputs
    DenseVector.horzcat[Double]((for (j <- indexes) yield examples._1(i)(j)) :_*)//iterate over all examples and construct matrix
    val targets = for (i <- (0 to examples._2(0).size - 1)) yield //iterate outputs
    DenseVector.horzcat[Double]((for (j <- indexes) yield examples._2(i)(j)) :_*) //iterate over all examples and construct matrix

    (inputs, targets)
  }

  protected var c = 0
  def showBatch() = {
    if (batchsize == 1) {
      if (random) {
        val index = scala.util.Random.nextInt(examples._1.size)
        showExample((examples._1(index).map(v=>DenseVector.horzcat[Double](v)), examples._2(index).map(v=>DenseVector.horzcat[Double](v))))
      } else {
        showExample((examples._1(c).map(v=>DenseVector.horzcat[Double](v)), examples._2(c).map(v=>DenseVector.horzcat[Double](v))))
        c = (c + 1) % examples._1.size
      }
    } else if (batchsize == examples._1.size) {
      showExample(fullBatchMatrices)
    } else {
      val batch = if (random) {
        scala.util.Random.shuffle((0 to examples._1.size - 1).toSeq).slice(0, batchsize)
      } else {
        c += batchsize
        if (c <= examples._1.size) {
          c - batchsize to c - 1
        } else {
          c = c - examples._1.size
          (c - batchsize + examples._1.size to examples._1.size - 1) union (0 to c - 1)
        }
      }
      showExample(miniBatchMatrices(batch))
    }
    n.learn()
  }
}
