package bpntest
import breeze.linalg._
import bpn._
import org.scalatest._

class XorTest extends FunSuite { 
  
  val dataset= Seq(
    new BpnExample(DenseVector(0d,0d), DenseVector(0d)),
    new BpnExample(DenseVector(0d,1d), DenseVector(1d)),
    new BpnExample(DenseVector(1d,0d), DenseVector(1d)),
    new BpnExample(DenseVector(1d,1d), DenseVector(0d))
  )

  bpn.verbosity=0 //switch off al diag messages

  def initNet()={ 
    val bdr = new ForwardBuilder()
    import bdr._
    create input 2 named "in"
    create sigmoid 2 named "hid"
    create sigmoid 1 named "out"
    connection from "in" to "hid"
    connection from "hid" to "out"
    make output "out"
    build()
  }
  
  test("running xortest on default parameters") {
    val net = initNet()
    val t= new Teacher(net,dataset)
    t.showExample(t.fullBatch)
    val before=net.outputs(0).avgSumErr
    t.learn(100)
    val after=net.outputs(0).avgSumErr
    assert(after<before, "error did not decrease in 100 epochs\n pre: "+before+" post: "+after)
  }

}
