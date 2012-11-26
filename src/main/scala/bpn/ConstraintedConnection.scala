package bpn
import collection.mutable.Set
import breeze.linalg._
class ConstraintedConnection(in: LayerOutput, out: LayerInput, gd: GradientDescent) extends Connection(in, out, gd) {
 var constraints=Set[ConstraintedConnection]()
 
 def addConstraint(that:ConstraintedConnection)={
   that.weights=this.weights
   constraints.add(that)
   that.constraints.add(this)
 }
 
 
 
 override def learn()={
   val gradientsum=
     (for(c<-constraints) yield c.dEdw).fold(DenseMatrix.zeros[Double](in.size,out.size))((a, b) ⇒ (a + b))+dEdw
   weights+=gd.getDelta(gradientsum*(1d/constraints.size+1))
   out.learn()
 }
 
}
