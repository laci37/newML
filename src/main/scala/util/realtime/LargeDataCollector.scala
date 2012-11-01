package util.realtime
import collection.mutable.ArrayBuffer
import java.io._
class LargeDataCollector(datastream: Seq[() ⇒ Double]) extends Simulable {
  val buff = new ArrayBuffer[(Double, Array[Double])]
  val tmpfile = getTempFile()
  val tmpfilestr = new RandomAccessFile(tmpfile, "rw")

  var time = 0d
  var maxBufferSize = 6250000 / (datastream.size + 1) //results in a ~50MB buffer
  def maxBufferSizeInBytes = maxBufferSize * (datastream.size + 1) * 8
  def maxBufferSizeInBytes_=(value: Int) = maxBufferSize = value / ((datastream.size + 1) * 8)

  var desc = ""
  var metadata = new Array[String](datastream.size)
  var c = 0

  protected def getTempFile() = {
    var i = 0
    var file = new File("ldgtemp"+i)
    while (file.exists) {
      i += 1
      file = new File("ldgtemp"+i)
    }
    file
  }

  def timestep(dt: Double): Object = {
    if (dt < 0) throw new Exception("dt<0")
    buff += ((time, (for (d ← datastream) yield d()).toArray))
    time += dt
   /*if (c < 2) {
      println("Printing debug frame, time: "+buff(buff.size - 1)._1)
      buff(buff.size - 1)._2 foreach println _
      println()
      c += 1
    }*/
    if (buff.size > maxBufferSize) writeBuffer()
    null
  }

  def update() = {}

  def writeBuffer() = {
    println(this+": writing buffer to file")
    buff foreach { tpl ⇒
      tmpfilestr.writeDouble(tpl._1)
      tpl._2 foreach tmpfilestr.writeDouble _
    }
    buff.clear
    System.gc()
  }

  def save(filename: String): Unit = {
    println(this+": saving")
    writeBuffer()

    val os = try {
      new DataOutputStream(new FileOutputStream(new File(filename)))
    } catch {
      case e: IOException ⇒ {
        println("IOEXCeption:"+e.getMessage)
        return
      }
    }

    try {
      os.writeInt(1) //version
      os.writeInt(datastream.size)
      os.writeUTF(desc)
      metadata.slice(0,datastream.size) foreach os.writeUTF _
      if(metadata.size<datastream.size){
        for(i<-(1 to datastream.size-metadata.size)) os.writeUTF("unknown")
      }
    } catch {
      case e: IOException ⇒ {
        println("IOException occured" :+ e.getMessage)
        os.close
        return
      }
    }

    var buff = new Array[Byte](maxBufferSizeInBytes) //copy buffer
    tmpfilestr.seek(0)
    def available = tmpfilestr.length - tmpfilestr.getFilePointer
    var maxlen = available
    try {
      while (true) {
        maxlen = available //save the number of bytes to read 
        tmpfilestr.readFully(buff)
        os.write(buff, 0, buff.size)
      }
    } catch {
      case e: EOFException ⇒ { //finished copying
        os.write(buff, 0, maxlen.toInt)
      }
      case e: IOException ⇒ {
        println("IOException caught:"+e.getMessage)
        new File(filename).delete
      }
    } finally {
      os.close()
    }

  }

  override def finalize() = {
    try {
      tmpfilestr.close()
      tmpfile.delete()
    } finally {
      super.finalize()
    }
  }
}