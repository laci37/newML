package bpn
import breeze.linalg._
import data._
import annotation.tailrec
class Teacher(n: Net, data: Seq[VectorExample]) {
  var batchSize = data.size
  protected var off = 0 //the current offset in the dataset
  val dataset= new Dataset(data) with CatableDataset[MatrixExample,VectorExample]

  //shows an example to the network, but does no learning pass
  def showExample(ex:MatrixExample)={ 
    n.loadExample(ex)
    n.propCycle()
  }
  
  def learnCycle()={ 
    n.loadExample(dataset.nextMulti(batchSize))
    n.fullCycle()
  }

  def testErr():Double={ 
    n.loadExample(dataset.nextMulti(data.size)) //show the full batch
    n.forwardProp()
    n.avgSumErr
  }

  @tailrec final def learn(epochs: Int):Unit={ 
    if(epochs<=0) return
    learnCycle()
    learn(epochs-1)
  }
}

class OnlineTeacher(n:Net, provider:Iterator[VectorExample]) { 
  
  def learnCycle()={ 
    n.loadExample(provider.next.asMulti)
    n.fullCycle()
  }

  @tailrec final def learn(c:Int):Unit={
    if(c<=0) return
    learnCycle()
    learn(c-1)
  }
}
