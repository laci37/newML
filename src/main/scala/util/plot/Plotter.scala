package util.plot
import javax.swing.JPanel
import java.awt._
import collection.mutable.{Seq, ListBuffer}
class Plotter() extends JPanel {
  
  var graphs: Seq[(ListBuffer[Point], Color, Stroke)] = Seq()
  var transx=0
  var transy=0
  
  override def paintComponent(g: Graphics) = {
    val g2d = g.asInstanceOf[Graphics2D]    
    g2d.clearRect(0, 0, getWidth, getHeight)
    g2d.translate(transx,transy)
    graphs foreach { data =>
      val points = data._1
      g2d.setColor(data._2)
      g2d.setStroke(data._3)
      for (i <- (0 to points.size - 2)) {
        import math._
        if(g2d.hitClip(min(points(i).x,points(i+1).x),
            min(points(i).y,points(i+1).y),
            abs(points(i).x-points(i+1).x),
            abs(points(i).x-points(i+1).x)))
        g2d.drawLine(points(i).x,
          points(i).y,
          points(i + 1).x,
          points(i + 1).y)
      }
    }
  }
  
  def clear()={
    graphs foreach { g=> g._1.clear()}
    this.repaint()
  }
}