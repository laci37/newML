package bpntest
import bpn._

abstract class GDXorTest(gd: (()=>GradientDescent)) extends XorTest { 
  override def initNet()={ 
    val bdr = new ForwardBuilder()
    import bdr._
    bdr.gd = this.gd
    create input 2 named "in"
    create sigmoid 2 named "hid"
    create sigmoid 1 named "out"
    connection from "in" to "hid"
    connection from "hid" to "out"
    make output "out"
    build()
  }
}

class NesterovXor extends GDXorTest(()=>new NesterovDescent())
class RmsPropXor extends GDXorTest(()=> new RmsProp)
