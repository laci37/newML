package util.realtime
import collection.mutable.ArrayBuffer

class DataCollector(datastream: Seq[Unit => Double]) extends Simulable {
  var buff = new ArrayBuffer[(Double, Array[Double])]
  var time = 0d
  def timestep(dt: Double): Object = {
    buff += ((time, (for (d <- datastream) yield d()).toArray))
    time += dt
    null
  }

  var metadata = new Array[String](datastream.length)
  var desc = ""
  var notes = Map[Double, String]()

  def update(): Unit = {}

  def save(filename: String): Unit = {
    import java.io.{ ObjectOutputStream, FileOutputStream }
    val fos = new FileOutputStream(filename)
    val oos = new ObjectOutputStream(fos)
    oos.writeObject(new DataCollector.DataExt(desc,notes,metadata, buff.toArray))
  }

}

object DataCollector {
  class Data(val metadata: Array[String], val data: Array[(Double, Array[Double])]) extends Serializable
  class DataExt(val desc: String,
    val notes: Map[Double, String],
    metadata: Array[String],
    data: Array[(Double, Array[Double])])
    extends Data(metadata, data)
}