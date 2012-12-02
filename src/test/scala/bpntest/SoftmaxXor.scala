package bpntest
import bpn._
import breeze.linalg._

class SoftmaxXor extends XorTest{ 
  override val dataset=Seq(
    new BpnExample(DenseVector(0d,0d), DenseVector(0d, 1d)),
    new BpnExample(DenseVector(0d,1d), DenseVector(1d, 0d)),
    new BpnExample(DenseVector(1d,0d), DenseVector(1d, 0d)),
    new BpnExample(DenseVector(1d,1d), DenseVector(0d, 1d))
  )
  override def initNet()={ 
    val bdr = new ForwardBuilder()
    import bdr._
    create input 2 named "in"
    create sigmoid 2 named "hid"
    create softmax 2 named "out"
    connection from "in" to "hid"
    connection from "hid" to "out"
    build()
  }
}
