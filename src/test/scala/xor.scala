package bpntest
import bpn._
import math._
import mathext._
class Xor {
  def sig(x: Double) = 1d / (1d + exp(-x))
  val hid = new Layer(2, sig _, x => sig(x) * (1 - sig(x)))
  hid.debug = "hid"
  val out = new Layer(1, sig _, x => sig(x) * (1 - sig(x)))
  out.debug = "out"
  val in = new InputLayer(2)
  val bias = new BiasLayer
  val c1 = new Connection(in, hid, new SteepestDescent(0.1))
  c1.debug = "c1"
  val c2 = new Connection(hid, out, new SteepestDescent(0.1))
  c2.debug = "c2"
  val c3 = new Connection(bias, hid, new SteepestDescent(0.1))
  c3.debug = "c3"
  val c4 = new Connection(bias, out, new SteepestDescent(0.1))
  c4.debug = "c4"
  val oc = new OutputConnection(out, (t, y) => (1d / 2d) * pow(t - y, 2), (t, y) => y - t)
  val n = new Net(Seq(in), Seq(oc), bias)
  val inputs = Seq(Seq(RVector(Array(0d, 0d))),
    Seq(RVector(Array(0d, 1d))),
    Seq(RVector(Array(1d, 0d))),
    Seq(RVector(Array(1d, 1d))))
  val targets = Seq(Seq(RVector(Array(0d))),
    Seq(RVector(Array(1d))),
    Seq(RVector(Array(1d))),
    Seq(RVector(Array(0d))))
  val t = new Teacher(n, (inputs, targets))
  for (i <- (1 to 100)) t.showBatch()
}
