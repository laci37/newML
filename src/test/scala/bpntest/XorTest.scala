package bpntest
import breeze.linalg._
import bpn._
import org.scalatest._
import data._

class XorTest extends FunSuite { 
  
  val dataset= Seq(
    new VectorExample(DenseVector(0d,0d), DenseVector(0d)),
    new VectorExample(DenseVector(0d,1d), DenseVector(1d)),
    new VectorExample(DenseVector(1d,0d), DenseVector(1d)),
    new VectorExample(DenseVector(1d,1d), DenseVector(0d))
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
    val before=t.testErr
    t.learn(1000)
    val after=t.testErr
    assert(after<before, "error did not decrease in 1000 epochs\n pre: "+before+" post: "+after)
  }

}
