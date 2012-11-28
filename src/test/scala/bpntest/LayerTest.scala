package bpntest
import bpn._
import breeze.linalg._
import org.scalatest.FunSuite

class LayerTest extends FunSuite { 
  class MockOutput(var dEdy: DenseMatrix[Double]) extends ConnectionInput{ 
    var fwd=false
    def forward()= fwd=true
    var lrn=false
    def learn()= lrn=true
  }
  
  class MockInput(out:Layer) extends Connection(null,out,null){ 
    //override var z=null
    var bck= false
    override def backward()= bck=true
  }

  test("not check y matrix size"){ 
    
  }
}
