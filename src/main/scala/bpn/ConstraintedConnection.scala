package bpn
import collection.mutable.Set
import mathext._
class ConstraintedConnection(in: Layer, out: Layer, gd: GradientDescent) extends Connection(in, out, gd) {
 val constraints=Set[ConstraintedConnection]()
 
 def addConstraint(that:ConstraintedConnection)={
   that.weights=this.weights
   constraints.add(that)
   that.constraints.add(this)
 }
 
 
 
 override def learn()={
   val gradientsum=
     (for(c<-constraints) yield c.avgdEdw).fold(Matrix(in.size,out.size))((a, b) ⇒ (a + b))+avgdEdw
   weights+=gd.getDelta(gradientsum*(1d/constraints.size+1))
   out.learn()
 }
 
}
