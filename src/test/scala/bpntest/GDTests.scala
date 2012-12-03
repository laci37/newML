package bpntest
import bpn._
import org.scalatest.FunSuite
import breeze.linalg._

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

abstract class GDTest(gd: (()=>GradientDescent)) extends FunSuite{ 
  test("testing GD on a single derivative matrix"){ 
    val subject=gd()
    val mat= DenseMatrix.create(3,2,Array(1d,2d,0d,-1d,0d,-3d))
    subject.getDelta(mat) //just to be sure. Momentum methods might produce 0 on the first iteration.
    val res=subject.getDelta(mat)
    assert(res.rows==3, "wrong number of rows")
    assert(res.cols==2, "wrong number of cols")
    assert(res(0,0)<0, "(0,0)="+res(0,0)+" instead of <0")
    assert(res(1,0)<0, "(1,0)="+res(1,0)+" instead of <0")
    assert(res(2,0)==0, "(2,0)="+res(2,0)+" instead of ==0")
    assert(res(0,1)>0, "(0,1)="+res(0,1)+" instead of >0")
    assert(res(1,1)==0, "(1,1)="+res(1,1)+" instead of ==0")
    assert(res(2,1)>0, "(2,1)="+res(2,1)+" instead of >0")
  }
}

class NesterovTest extends GDTest(()=> new NesterovDescent)
class RmsPropTest extends GDTest(()=> new RmsProp)
class SteepestDescentTest extends GDTest(()=> new SteepestDescent(1d))
