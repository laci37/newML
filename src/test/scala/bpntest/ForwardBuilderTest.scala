package bpntest
import bpn._
import math._
import breeze.linalg._
import org.scalatest.FunSuite
class ForwardBuilderTest extends FunSuite {

  test("testing ForwardBuilder with simple network of 2->2->1") {
    val bdr = new ForwardBuilder()
    import bdr._
    val in = create input 2 named "in"
    val hid = create sigmoid 2 named "hid"
    val out = create sigmoid 1 named "out"
    val in2hid:Connection = connection from "in" to "hid"
    val hid2out:Connection = connection from "hid" to "out"
    make output "out"
    val net = build()
    
    //not all possible asserts, just a bunch
    assert(net.inputs.size === 1)
    assert(net.inputs(0) === in)
    assert(in.outputs.size === 1)
    assert(in.outputs(0) === in2hid)
    assert(in2hid.in === in)
    assert(in2hid.out === hid)
    assert(hid.inputs.size === 2)
    assert(hid.inputs.contains(in2hid), "in2hid.inputs")
    assert(net.outputs.size === 1) 
  }

}
