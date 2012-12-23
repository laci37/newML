package bpntest
import bpn._
import breeze.linalg._
import org.scalatest.FunSuite

class ConnectionTest extends FunSuite{ 
  import ConnectionTest._
  //init function
  def initInput()={ 
    new MockInput
  }

  def initOutput()= new MockOutput

  def initConnection(in:LayerOutput,out:LayerInput)= new Connection(in,out,new SteepestDescent(0.1))

  test("should forward prop") { 
    val in= initInput()
    val out=initOutput()
    val c=initConnection(in,out)
    c.forward()
    assert(out.fwd)
  }

  test ("should backprop") { 
    val in= initInput()
    val out=initOutput()
    val c=initConnection(in,out)
    c.forward
    c.backward
    assert(in.backwd)
  }

  test("should learn prop"){ 
    val in= initInput()
    val out=initOutput()
    val c=initConnection(in,out)
    c.forward
    c.backward
    c.learn
    assert(out.lrn)
  }

  test("forward and weight matrix size checks"){ 
    val in= initInput()
    val out=initOutput()
    in.size=7
    in.y= DenseMatrix.zeros(7,11)
    out.size=3
    val c=initConnection(in,out)
    c.forward
    assert(c.z.rows===3)
    assert(c.z.cols===11)
    assert(c.weights.rows===7)
    assert(c.weights.cols===3)
  }

  test("backward and dEdw matrix size checks"){ 
    val in= initInput()
    val out=initOutput()
    in.size=6
    in.y=DenseMatrix.zeros(6,23)
    out.size=11
    out.dEdz=DenseMatrix.zeros(11,23)
    val c=initConnection(in,out)
    c.forward
    c.backward
    assert(c.dEdy.cols===23)
    assert(c.dEdy.rows===6)
    assert(c.dEdw.rows===6)
    assert(c.dEdw.cols===11)
  }
}

object ConnectionTest { 
  class MockInput extends LayerOutput{ 
    var size:Int=1
    var y:DenseMatrix[Double]=DenseMatrix.zeros(1,1)
    var outputs=Seq[ConnectionInput]()
    var fwd=false
    def forward()={fwd=true }
    var lrn=false
    def learn()={lrn=true }
    var backwd=false
    def backward()={backwd=true }
  }

  class MockOutput extends LayerInput{ 
    var size: Int=1
    var dEdz: DenseMatrix[Double]=DenseMatrix.zeros(1,1)
    var inputs= Seq[Connection]()
    var fwd=false
    def forward()={fwd=true }
    var lrn=false
    def learn()={lrn=true }
    var backwd=false
    def backward()={backwd=true }
    def getWeightSeq=null
    def loadWeights(data:List[Matrix[Double]]):List[Matrix[Double]]=null
  }
}
