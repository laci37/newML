package data
import breeze.linalg._

trait Observation[F] extends Serializable{
  def features: F
}

trait Example[F, L] extends Observation[F] {
  def label: L
}

trait CatableObservation[F, Multi <: CatedObservation[_, ThisType, Multi], ThisType <: CatableObservation[F, Multi, ThisType]] extends Observation[F] {
  def cat(that: ThisType): Multi = cat(that.asMulti)
  def cat(that: Multi): Multi = asMulti cat that

  def asMulti: Multi
}

trait CatableExample[F, L, Multi<:CatedExample[_,_,ThisType,Multi], ThisType <: CatableExample[F, L, Multi, ThisType]]
 extends CatableObservation[F,Multi,ThisType] with Example[F, L] 

trait CatedObservation[F, Single <: CatableObservation[_, ThisType, Single], ThisType <: CatedObservation[F, Single, ThisType]]
  extends Observation[F] {
  def cat(that: Single): ThisType = cat(that.asMulti)
  def cat(that: ThisType): ThisType
}

trait CatedExample[F, L, Single <: CatableExample[_, _, ThisType, Single], ThisType <: CatedExample[F, L, Single, ThisType]]
  extends CatedObservation[F,Single,ThisType] with Example[F, L] 

object CatFold{ 
  def apply[M<:CatedObservation[_,T,M],T<:CatableObservation[_,M,T]](s:Seq[T])(implicit empty:M):M={ 
    s.foldLeft[M](empty)(_ cat _)
  }
}

class VectorObservation(val features: DenseVector[Double]) extends CatableObservation[DenseVector[Double], MatrixObservation, VectorObservation] {

  override def cat(that: VectorObservation): MatrixObservation = {
    new MatrixObservation(DenseVector.horzcat(this.features, that.features))
  }

  lazy val asMulti: MatrixObservation = {
    new MatrixObservation(DenseMatrix.create(features.size, 1, features.data))
  }
}

class MatrixObservation(val features: DenseMatrix[Double]) extends CatedObservation[DenseMatrix[Double], VectorObservation, MatrixObservation] {

  def cat(that: MatrixObservation): MatrixObservation = {
    new MatrixObservation(DenseMatrix.horzcat(this.features, that.features))
  }

}

object MatrixObservation{ 
  implicit object empty extends MatrixObservation(null) {
    override def cat(that: MatrixObservation) = that
    override def cat(that: VectorObservation) = that.asMulti
  }
}

class VectorExample(val features: DenseVector[Double], val label: DenseVector[Double])
  extends CatableExample[DenseVector[Double], DenseVector[Double], MatrixExample, VectorExample] {

  override def cat(that: VectorExample): MatrixExample = {
    new MatrixExample(DenseVector.horzcat(this.features, that.features), DenseVector.horzcat(this.label, that.label))
  }

  lazy val asMulti: MatrixExample = {
    new MatrixExample(DenseMatrix.create(features.size, 1, features.data), DenseMatrix.create(label.size, 1, label.data))
  }
}

class MatrixExample(val features: DenseMatrix[Double], val label: DenseMatrix[Double])
  extends CatedExample[DenseMatrix[Double], DenseMatrix[Double], VectorExample, MatrixExample] {

  def cat(that: MatrixExample): MatrixExample = {
    new MatrixExample(DenseMatrix.horzcat(this.features, that.features), DenseMatrix.horzcat(this.label, that.label))
  }

}

object MatrixExample{ 
  implicit object empty extends MatrixExample(null, null) {
    override def cat(that: MatrixExample) = that
    override def cat(that: VectorExample) = that.asMulti
  }
}
