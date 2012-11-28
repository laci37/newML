package bpn
import collection.mutable.Set
import breeze.linalg._
class ConstraintedConnection(in: LayerOutput, out: LayerInput, gd: GradientDescent) extends Connection(in, out, gd) {
 var constraints=Set[ConstraintedConnection]()
 protected var changedWeights=false 
 
 def addConstraint(that:ConstraintedConnection)={
   that.weights=this.weights
   constraints.add(that)
   that.constraints.add(this)
 }
 
  

 override def learn()={
   if(!changedWeights){ 
     val gradientsum=
       (for(c<-constraints) yield c.dEdw).fold(DenseMatrix.zeros[Double](in.size,out.size))((a, b) ⇒ (a + b))+dEdw
     val delta=gd.getDelta(gradientsum:/(constraints.size+1).toDouble)
     for(c<-constraints) c.changeWeights(delta,this)
     weights+=delta
   }
   out.learn()
 }

 protected def changeWeights(delta:DenseMatrix[Double], caller:ConstraintedConnection)={ 
   changedWeights=true
   weights+=delta
 }
 
}
