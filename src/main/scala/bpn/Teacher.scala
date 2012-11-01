package bpn
import mathext._
class Teacher(val n: Net, var examples: (Seq[Seq[RVector]], Seq[Seq[RVector]])) {
  var batchsize = examples._1.size
  var random = false

  def showExample(ex: (Seq[Matrix], Seq[Matrix])) = {
    if (bpn.verbosity >= 100) println("show example")
    n.setInputs(ex._1)
    n.setTargets(ex._2)
    n.propCycle()
  }

  def fullBatchMatrices(): (Seq[Matrix], Seq[Matrix]) = {
    val inputs = for (i <- (0 to examples._1(0).size - 1)) yield //iterate inputs
    //iterate over all examples and construct matrix
    Matrix(for (j <- (0 to examples._1.size - 1)) yield examples._1(j)(i))
    val targets = for (i <- (0 to examples._2(0).size - 1)) yield //iterate outputs
    //iterate over all examples and construct matrix
    Matrix(for (j <- (0 to examples._2.size - 1)) yield examples._2(j)(i))

    (inputs, targets)
  }

  def miniBatchMatrices(indexes: Seq[Int]): (Seq[Matrix], Seq[Matrix]) = {
    val inputs = for (i <- (0 to examples._1(0).size - 1)) yield //iterate inputs
    Matrix(for (j <- indexes) yield examples._1(i)(j)) //iterate over all examples and construct matrix
    val targets = for (i <- (0 to examples._2(0).size - 1)) yield //iterate outputs
    Matrix(for (j <- indexes) yield examples._2(i)(j)) //iterate over all examples and construct matrix

    (inputs, targets)
  }

  protected var c = 0
  def showBatch() = {
    if (batchsize == 1) {
      if (random) {
        val index = scala.util.Random.nextInt(examples._1.size)
        showExample((examples._1(index), examples._2(index)))
      } else {
        showExample((examples._1(c), examples._2(c)))
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
