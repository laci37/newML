package test.hopcomp
import javax.swing.{ JPanel }
import java.awt.{ Graphics, Graphics2D, Color, BasicStroke, Dimension }
class GridDisplay(val sizeX:Int, val sizeY:Int) extends JPanel {
  //check constructor arguments
  if(sizeX<=0 || sizeY<=0) throw new IllegalArgumentException("Size must be positive")
  
  //setup super properties
  setPreferredSize(new Dimension(sizeX*50, sizeY*50))

  
  protected var _data = new Array[Boolean](sizeX*sizeY)
  def data = _data
  def data_=(value: Array[Boolean]) = {
    if(value.size!=sizeX*sizeY) throw new IllegalArgumentException("Data array size is wrong")
    _data = value
    repaintAuto()
  }

  protected var _colTrue = Color.RED
  def colTrue = _colTrue
  def colTrue_=(value: Color) = {
    _colTrue = value
    repaintAuto()
  }

  protected var _colFalse = Color.BLACK
  def colFalse = _colFalse
  def colFalse_=(value: Color) = {
    _colFalse = value
    repaintAuto()
  }

  var autoRepaint = false
  protected def repaintAuto() = {
    if (autoRepaint) repaint()
  }

  override def paintComponent(g: Graphics) = {
    for (i ← (0 to 3)) for (j ← (0 to 3)) {
      if (data(i + j * 4)) g.setColor(colTrue)
      else g.setColor(colFalse)
      g.fillRect(i * getWidth / sizeX, j * getHeight / sizeY, getWidth / sizeX, getHeight / sizeY)
    }
  }

}