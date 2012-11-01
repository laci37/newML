package util.plot
import java.awt._
import event._
import javax.swing._
import util.realtime.DataCollector.Data
import java.io.{ ObjectInputStream, FileInputStream }
import collection.mutable.{ Map, ListBuffer }
class DataDisplay(filename: String) extends JFrame {

  //UI elements
  val plot = new Plotter()
  val panel = new Panel
  val listModel = new DefaultListModel()
  val listBox = new JList(listModel)
  val listPane = new ScrollPane
  val text = new JTextField
  val defaultStroke = new BasicStroke

  //data variables
  val data = new ObjectInputStream(new FileInputStream(filename)).readObject.asInstanceOf[Data]
  var dataGraphMap = Array.fill[Option[Int]](data.metadata.size)(None)

  //display variables
  var xscale = 1d
  var yscale = 20d
  def xaxis = plot.getHeight / 2

  initUI

  def initUI() = {
    add(plot, BorderLayout.CENTER)
    add(panel, BorderLayout.SOUTH)
    listPane.add(listBox, BorderLayout.CENTER)
    panel.add(listPane, BorderLayout.CENTER)
    panel.add(text, BorderLayout.EAST)
    text.setPreferredSize(new Dimension(200, 30))
    
    listBox.addMouseListener(new MouseListener() {
      def mouseClicked(e: MouseEvent): Unit = {
        if (e.getClickCount() == 2) toggleGraph()
      }

      def mouseEntered(e: MouseEvent) = {}
      def mouseExited(e: MouseEvent) = {}
      def mousePressed(e: MouseEvent) = {}
      def mouseReleased(e: MouseEvent) = {}
    })
    //mouse listener for drag translate
    plot.addMouseListener(new MouseListener() {
      var pressx = 0
      var pressy = 0
      def mouseClicked(e: MouseEvent): Unit = {}
      def mouseEntered(e: MouseEvent) = {}
      def mouseExited(e: MouseEvent) = {}
      def mousePressed(e: MouseEvent) = {
        pressx = e.getX
        pressy = e.getY
      }
      def mouseReleased(e: MouseEvent) = {
        plot.transx += e.getX - pressx
        plot.transy += e.getY - pressy
        plot.repaint()
      }
    })
    data.metadata.reverse foreach {s=>
      listModel.add(0,s)
      }
  }

  def toggleGraph() = synchronized {
    val index = listBox.getLeadSelectionIndex
    if (dataGraphMap(index) != None) {
      val gindex = dataGraphMap(index).get
      for (i <- (0 to dataGraphMap.size - 1)) {
        if (dataGraphMap(i) != None && dataGraphMap(i).get > gindex)
          dataGraphMap(i) = Some(dataGraphMap(i).get - 1)
      }
      plot.graphs = plot.graphs diff Seq(plot.graphs(gindex))
      dataGraphMap(index) = None
    } else addGraph()
    plot.repaint()
  }

  def addGraph() = {
    val index = listBox.getLeadSelectionIndex
    try {
      val colInt = Integer.parseInt(text.getText, 16)
      val col = new Color(colInt)
      dataGraphMap(index) = Some(plot.graphs.size)
      plot.graphs = plot.graphs :+ (null, col, defaultStroke)
      updateGraph(index)
    } catch {
      case e => JOptionPane.showMessageDialog(this, e.getMessage, "Error", JOptionPane.ERROR_MESSAGE)
    }
  }

  def updateGraph(index: Int) = {
    var lastx = -1
    val buff = new ListBuffer[Point]
    for (dp <- data.data) {
      if (lastx != (dp._1 * xscale).toInt) {
        lastx = (dp._1 * xscale).toInt
        buff.append(new Point(lastx, (dp._2(index) * -yscale).toInt + xaxis))
      }
    }
    val graph = plot.graphs(dataGraphMap(index).get)
    plot.graphs(dataGraphMap(index).get) = (buff, graph._2, graph._3)
  }

}