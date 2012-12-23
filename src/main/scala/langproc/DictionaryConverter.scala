package langproc
import data.{VectorObservation, VectorExample}
import java.io._
import collection.mutable.ArrayBuffer
import breeze.linalg._

class DocObsConverter(separator:Char, wc:WordConverter, stripOf:Seq[Char]){
  var bufferSize=2000000

  def convert(str:String):Seq[Vector[Double]]={ 
    val words=str.split(separator)
    val stripped=words map (s=> s.diff(stripOf))
    (stripped map wc.convert)
  }

  def convertFile(file:File):Seq[Vector[Double]]={ 
    val is=new FileReader(file)
    val res=new ArrayBuffer[Vector[Double]]()
    var remain="" //the word segment that might remain at the end of the buffer
    val buffer=new Array[Char](bufferSize)
   
    while ( is.read(buffer) != -1){ 
      val end=buffer.lastIndexOf(separator)
      val toConv=new String(buffer.slice(0,end))
      res++= convert(remain+toConv)
      remain=new String(buffer.slice(end,buffer.length))      
    }
    res ++= convert(remain)
    is.close()
    res
  }

  def convertAndSave(ifile:File, ofile:File):Unit={ 
    val is=new FileReader(ifile)
    val os= new ObjectOutputStream(new FileOutputStream(ofile))
    var remain="" //the word segment that might remain at the end of the buffer
    val buffer=new Array[Char](bufferSize)
    while (is.read(buffer) != -1){ 
      val end=buffer.lastIndexOf(separator)
      val toConv=new String(buffer.slice(0,end))
      convert(remain+toConv) foreach os.writeObject _ 
      remain=new String(buffer.slice(end,buffer.length))      
    }
    convert(remain) foreach os.writeObject _
    os.flush()
    is.close()
    os.close()
  }
}
