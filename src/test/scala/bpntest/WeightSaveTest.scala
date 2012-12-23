package bpntest
import org.scalatest.FunSuite
import bpn._

class WeightSaveTest extends FunSuite { 
  test("testing saving and loading weights on a feedforward net"){ 
    val bdr= new ForwardBuilder
    import bdr._
    create input 2 named "2"
    create input 3 named "3"
    create sigmoid 4 named "4"
    create sigmoid 5 named "5"
    create sigmoid 6 named "6"
    create sigmoid 7 named "7"
    connection from "2" to "4"
    connection from "3" to "4"
    //connection from "2" to "5"
    connection from "4" to "5"
    connection from "4" to "6"
    connection from "5" to "7"
    connection from "6" to "7"
    val n = build()
    val w1=n.weightSeq
    n.loadWeights(w1)
    assert(w1 sameElements n.weightSeq, "the two weightseqs do not equal")
  }
}
