package bpntest
import bpn._
import breeze.linalg._

class ConstrConnTest extends ConnectionTest { 
  override def initConnection(in:LayerOutput, out:LayerInput)={ 
    new ConstraintedConnection(in,out,new SteepestDescent(0.1))
  }

  test("the weights in a constraint group should be always equal"){ 
    val in1= initInput
    in1.size=3
    in1.y=DenseMatrix.create(3,2,Array(1d,2d,3d,4d,5d,6d))
    val in2= initInput
    in2.size=3
    in2.y=in1.y :* 3d
    val out1=initOutput
    out1.size=2
    out1.dEdz=DenseMatrix.create(2,2,Array(1d,1d,2d,2d))
    val out2=initOutput
    out2.size=2
    out2.dEdz=DenseMatrix.create(2,2,Array(10d,10d,-3d,-2d))
    val c1=new ConstraintedConnection(in1,out1,new SteepestDescent(0.1))
    val c2=new ConstraintedConnection(in2,out2,new SteepestDescent(0.1))
    c1.addConstraint(c2)
    assert(c1.weights===c2.weights)
    c1.backward
    c2.backward
    c1.learn
    c2.learn
    assert(c1.weights===c2.weights)
  }
}
