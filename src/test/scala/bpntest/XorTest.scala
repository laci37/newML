package bpntest
import breeze.linalg._
import bpn._
import org.scalatest._

class XorTest extends FunSuite { 
  
  val dataset= (Seq(
    Seq(DenseVector(Array(0d,0d))),
    Seq(DenseVector(Array(0d,1d))),
    Seq(DenseVector(Array(1d,0d))),
    Seq(DenseVector(Array(1d,1d)))
  ),
  Seq(
    Seq(DenseVector(Array(0d))),
     Seq(DenseVector(Array(1d))),
     Seq(DenseVector(Array(1d))),
     Seq(DenseVector(Array(0d)))
  ))
  
  test("running xortest on default parameters") { 
    val bdr = new ForwardBuilder()
    import bdr._
    create input 2 named "in"
    create sigmoid 2 named "hid"
    create sigmoid 1 named "out"
    connection from "in" to "hid"
    connection from "hid" to "out"
    make output "out"
    val net = build()
    val t= new Teacher(net,dataset)
    t.showExample(t.fullBatchMatrices)
    val before=net.outputs(0).avgSumErr
    for(i<-(1 to 100)) t.showBatch()
    val after=net.outputs(0).avgSumErr
    assert(after<before, "error did not decrease in 100 epochs")
  }

}
