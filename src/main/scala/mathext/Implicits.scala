
package mathext

object Implicits {
  val rand= scala.util.Random
  class RVectorSeqWrap(val vec:RVector) extends IndexedSeq[Double]{
    def apply(i:Int)=vec(0,i)
    def length=vec.cols
  }

  implicit def RVector2Seq(vec:RVector): IndexedSeq[Double]={
    new RVectorSeqWrap(vec)
  }

  class DoubleExt(val inner:Double){ 
    def ?[T](a:T):RandomChoice[T]=new RandomChoice[T](inner,a)
  }

  class RandomChoice[T](val chance:Double, val a:T){ 
    def |(b:T):T= if(rand.nextDouble<chance) a else b
  }

  implicit def double2DoubleExt(x:Double):DoubleExt= new DoubleExt(x)
}
