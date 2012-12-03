
package mathext

object Implicits {
  val rand= scala.util.Random
 
  class DoubleExt(val inner:Double){ 
    def ?[T](a:T):RandomChoice[T]=new RandomChoice[T](inner,a)
    /**
     * roughly equals (max 0.01% difference)
     */ 
    def =~(that:Double):Boolean = if(inner>=0) that>0.9999*inner && that<1.001*inner else that<0.9999*inner && that>1.001*inner
  }

  class RandomChoice[T](val chance:Double, val a:T){ 
    def |(b:T):T= if(rand.nextDouble<chance) a else b
  }

  implicit def double2DoubleExt(x:Double):DoubleExt= new DoubleExt(x)
}
