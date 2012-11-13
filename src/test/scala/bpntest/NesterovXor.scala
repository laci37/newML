package bpntest
import bpn._
import math._
import mathext._
class NesterovXor {

  val n = {
    val bdr = new ForwardBuilder
    import bdr._
    gd = () => new NesterovDescent
    create input 2 named "in"
    create sigmoid 2 named "hid"
    create sigmoid 1 named "out"
    connection from "in" to "hid"
    connection from "hid" to "out"
    make output "out"
    build
  }

  val inputs = Seq(Seq(RVector(Array(0d, 0d))),
    Seq(RVector(Array(0d, 1d))),
    Seq(RVector(Array(1d, 0d))),
    Seq(RVector(Array(1d, 1d))))
  val targets = Seq(Seq(RVector(Array(0d))),
    Seq(RVector(Array(1d))),
    Seq(RVector(Array(1d))),
    Seq(RVector(Array(0d))))
  val t = new Teacher(n, (inputs, targets))

}
