
package mathext

object Implicits {
  val rand= scala.util.Random
 
  class DoubleExt(val inner:Double){ 
    def ?[T](a:T):RandomChoice[T]=new RandomChoice[T](inner,a)
  }

  class RandomChoice[T](val chance:Double, val a:T){ 
    def |(b:T):T= if(rand.nextDouble<chance) a else b
  }

  implicit def double2DoubleExt(x:Double):DoubleExt= new DoubleExt(x)
}
