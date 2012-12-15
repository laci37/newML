package bpntest
import bpn._
import breeze.linalg._
import org.scalatest.FunSuite
import data._

class AutoEncoderTest extends FunSuite{ 
  val dataset=Seq(
    new VectorObservation(DenseVector(0d,0d,1d)),
    new VectorObservation(DenseVector(0d,1d,0d)),
    new VectorObservation(DenseVector(1d,0d,0d))
  )

  test("testing simple toy autoenc"){
    val bdr= new ForwardBuilder
    import bdr._
    create input 3 named "in"
    create sigmoid 2 named "hid"
    create sigmoid 3 named "out"
    connection from "in" to "hid"
    connection from "hid" to "out"
    make output "out"
    val n=bdr.build
    val t= new AutoEncoderTeacher(n,dataset)
    val before=t.testErr
    t.learn(1000)
    val after=t.testErr
    assert(after<before, "error did not decrease in 1000 epochs\n pre: "+before+" post: "+after)
  }
}
