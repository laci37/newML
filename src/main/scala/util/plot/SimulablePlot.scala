package util.plot
import util.realtime._
import collection.mutable.{ Seq, ListBuffer }
import java.awt.{ Point, Color, BasicStroke, Graphics2D, Graphics }
import annotation.tailrec
class SimulablePlot extends Plotter with Simulable {
  var xscale = 1d
  var yscale = -20d
  var time = 0d
  var qtime = 0
  def timestep(dt: Double) = {
    time += dt
    if (qtime != (time * xscale).toInt) {
      qtime = (time * xscale).toInt
      addPoints()
    }
  }

  def addPoints(): Unit = {
    if (graphs.size == data.size) {
      for (i <- (0 to data.size - 1)) {
        graphs(i)._1.append(new Point(qtime, (data(i)() * yscale).toInt))
      }
    } else {
      while (graphs.size < data.size) {
        graphs = graphs :+ (ListBuffer[Point](), getColor(graphs.size), stroke)
      }
      addPoints()
    }
  }

  var data = Seq[Unit => Double]()
  var stroke = new BasicStroke

  def getColor(num: Int) = {
    // first 3 digits in base-3
    val b = num % 3
    val g = (num / 3) % 3
    val r = (num / 9) % 3
    new Color(r*0.5f, g*0.5f, b*0.5f)
  }

  def addData(d: Unit => Double) = data = data :+ d

  def update() = {}

  override def paintComponent(g: Graphics) = {
    val g2d = g.asInstanceOf[Graphics2D]
    g2d.clearRect(0, 0, getWidth, getHeight)
    g2d.translate((-qtime + getWidth).toInt, getHeight / 2)
    super.paintComponent(g2d)
  }
}