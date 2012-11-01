
package object mathext {
  
  class RVectorSeqWrap(val vec:RVector) extends IndexedSeq[Double]{
    def apply(i:Int)=vec(0,i)
    def length=vec.cols
  }
  implicit def RVector2Seq(vec:RVector): IndexedSeq[Double]={
    new RVectorSeqWrap(vec)
  }
}