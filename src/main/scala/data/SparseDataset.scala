package data
import breeze.linalg._

class SparseObsDataset(data:Seq[SparseVector[Double]]) extends Iterator[VectorObservation]{
  def training=data

  private var off=0

  def next={ 
    val vec= DenseVector.zeros[Double](data(off).size)
    vec:=training(off)
    off=(off + 1) % training.length
    new VectorObservation(vec)
  }

  def hasNext=true
}

class SparseExampleDataset(features:Seq[SparseVector[Double]], labels:Seq[SparseVector[Double]]) extends Iterator[VectorExample]{ 
  def training=(features,labels)

  private var off=0

  def next={ 
    val fvec= DenseVector.zeros[Double](training._1(off).size)
    val lvec= DenseVector.zeros[Double](training._2(off).size)
    fvec:=training._1(off)
    lvec:=training._2(off)
    off=(off+1) % training._1.size
    new VectorExample(fvec,lvec)
  }

  def hasNext=true
}
