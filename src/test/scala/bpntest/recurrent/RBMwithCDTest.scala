package bpntest.recurrent
import bpn.recurrent._
import org.scalatest.FunSuite
import breeze.linalg._
import bpn._
import data._

class RBMwithCDTest extends FunSuite { 
  test("RBM trained with CD1 for 2 cases"){ 
    val rbm= new RBM(2,2)
    val dataset=Seq(new VectorObservation(DenseVector(0d,1d)),new VectorObservation(DenseVector(1d,0d)))
    val cd= new ContrastiveDivergence(1,rbm,dataset)
    val G1before=rbm.goodness(DenseMatrix.tabulate[Double](2,4)((r,c)=>r.toDouble),
			      DenseMatrix.create(2,4,Array(0d,0d,0d,1d,1d,0d,1d,1d))).sum
    val G2before=rbm.goodness(DenseMatrix.tabulate[Double](2,4)((r,c)=>1d),
			      DenseMatrix.create(2,4,Array(0d,0d,0d,1d,1d,0d,1d,1d))).sum
    cd.learn(1000)
    val G1after=rbm.goodness(DenseMatrix.tabulate[Double](2,4)((r,c)=>r.toDouble),
			      DenseMatrix.create(2,4,Array(0d,0d,0d,1d,1d,0d,1d,1d))).sum
    val G2after=rbm.goodness(DenseMatrix.tabulate[Double](2,4)((r,c)=>1d),
			      DenseMatrix.create(2,4,Array(0d,0d,0d,1d,1d,0d,1d,1d))).sum
    assert(G1before-G2before<G1after-G2after, "G diff didn't increase")
  }
}
