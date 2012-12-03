package bpntest
import bpn._
import org.scalatest.FunSuite

class StochasticXor extends XorTest { 
  override def initNet={ 
    val bdr = new ForwardBuilder()
    import bdr._
    gd = ()=> new SteepestDescent(2)
    //because of randomness it needs large weights and to get those larger learning rate
    create input 2 named "in"
    create binStoch 2 named "hid"
    create sigmoid 1 named "out"
    connection from "in" to "hid"
    connection from "hid" to "out"
    make output "out"
    build()
  }
}
