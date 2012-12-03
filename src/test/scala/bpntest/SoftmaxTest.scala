package bpntest
import bpn._
import breeze.linalg._
import org.scalatest.FunSuite
import math._
import mathext.Implicits._

class SoftmaxTest extends FunSuite { 
  class MockInput(out:LayerInput) extends Connection(new ConnectionTest.MockInput,out,null){ 
    override def z= setZ
    var setZ:DenseMatrix[Double]=null
    var bck= false
    override def backward()= bck=true
  }

  test("calculation test"){ 
    val sm= new Softmax(3)
    val in= new MockInput(sm)
    in.setZ= DenseMatrix.create(3,2,Array(1d,0d,0d,1d,1d,-1d))
    sm.forward()
    assert(sm.y(0,0) =~ E/(2+E),"y(0,0) wrong "+sm.y(0,0))
    assert(sm.y(1,0) =~ 1/(2+E),"y(1,0) wrong "+sm.y(1,0))
    assert(sm.y(2,0) =~ 1/(2+E),"y(2,0) wrong "+sm.y(2,0))
    assert(sm.y(0,1) =~ E/(2*E+1/E),"y(0,1) wrong "+sm.y(0,1))
    assert(sm.y(1,1) =~ E/(2*E+1/E),"y(1,1) wrong "+sm.y(1,1))
    assert(sm.y(2,1) =~ 1/(2*E*E+1),"y(2,1) wrong "+sm.y(2,1))
    sm.targets=DenseMatrix.create(3,2,Array(0d,1d,0d,0d,1d,0d))
    assert(sm.avgSumErr =~ (-log(1/(2+E))-log(E/(2*E+(1/E))))/2, "wrong error "+sm.avgSumErr)
    sm.backward()
    assert(sm.dEdz(0,0) =~ E/(2+E),"dEdz(0,0) wrong "+sm.dEdz(0,0))
    assert(sm.dEdz(1,0) =~ 1/(2+E)-1,"dEdz(1,0) wrong "+sm.dEdz(1,0))
    assert(sm.dEdz(2,0) =~ 1/(2+E),"dEdz(2,0) wrong "+sm.dEdz(2,0))
    assert(sm.dEdz(0,1) =~ E/(2*E+1/E),"dEdz(0,1) wrong "+sm.dEdz(0,1))
    assert(sm.dEdz(1,1) =~ E/(2*E+1/E)-1,"dEdz(1,1) wrong "+sm.dEdz(1,1))
    assert(sm.dEdz(2,1) =~ 1/(2*E*E+1),"dEdz(2,1) wrong "+sm.dEdz(2,1))
  }
}
