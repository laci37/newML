package data


class Dataset[T](val data: Seq[T]) extends Iterator[T]{

  def training = data
  
  var off = 0
  def nextBatch(size: Int): Seq[T] = {
    if (size > training.length || size < 1) throw new IllegalArgumentException
    off += size
    if (off < training.length) {
      training.slice(off - size, off)
    } else {
      val pt1 = training.slice(off - size, training.length)
      off -= training.length
      pt1 union training.slice(0, off)
    }
  }

  def hasNext=true

  def next={ 
    val res=training(off)
    off+=1;
    res
  }

}

object Dataset{ 
  def fromFile[T](filename:String):Dataset[T]={ 
    new Dataset(readData[T](filename))
  }

  def readData[T](filename:String):Seq[T]={ 
    import java.io._
    import collection.mutable.ArrayBuffer
    val res=new ArrayBuffer[T]()
    val is=new ObjectInputStream(new FileInputStream(filename))
    try{ 
      while(true){ 
	res+=is.readObject().asInstanceOf[T]
      }
    }
    is.close()
    res
  }
}

class CrossValidDataset[T](data: Seq[T], k: Int) extends Dataset[T](data) {
  val rand = scala.util.Random
  val shuffled = rand.shuffle(data)
  val validation = shuffled.slice(0, data.length / k)
  val test = shuffled.slice(data.length / k, data.length * 2 / k)
  override val training = shuffled.slice(data.length * 2 / k, data.length)

}

trait CatableDataset[M<:CatedObservation[_,T,M], T<:CatableObservation[_,M,T]]
 extends Dataset[T]{ 
   def nextMulti(size:Int)(implicit empty:M): M={ 
     CatFold[M,T](nextBatch(size))(empty)
   }
}
