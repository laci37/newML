package util.plot
import javax.swing.JPanel
import java.awt._
import collection.mutable.{ Seq, ListBuffer }
class TimePlotter(lines: (Color, Stroke)*) extends Plotter {
  var yscale = 20d //scaling of Y values
  var xscale = 20d
  var xaxis = 100 //Y coordinate of the X axis

  graphs = Seq((for (l <- lines) yield (ListBuffer[Point](), l._1, l._2)): _*)

  private var time = 0d
  def historyLength = this.getWidth / xscale

  def step(dt: Double, values: Double*) = {
    time += dt
    for (i <- (0 to values.size - 1)) {
      graphs(i)._1.append(new Point((time * xscale).toInt, (xaxis - values(i) * yscale).toInt))
    }
  }

  override def paintComponent(g: Graphics) = {
    val g2d = g.asInstanceOf[Graphics2D]
    g2d.clearRect(0, 0, getWidth, getHeight)
    g2d.translate((-time * xscale + getWidth).toInt, 0)
    super.paintComponent(g2d)
  }
}