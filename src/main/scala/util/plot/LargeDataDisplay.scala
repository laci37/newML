package util.plot
import java.io._
import java.awt._
import event._
import javax.swing._
import java.io.RandomAccessFile
import annotation.tailrec
import collection.mutable.{ Map, ListBuffer }
class LargeDataDisplay(filename: String) extends JFrame {

  //UI elements
  val plot = new Plotter()
  val panel = new Panel
  val listModel = new DefaultListModel()
  val listBox = new JList(listModel)
  val listPane = new ScrollPane
  val text = new JTextField
  val defaultStroke = new BasicStroke

  //data
  val file = new RandomAccessFile(filename, "r")
  val filever = file.readInt()
  if (filever != 1) throw new Exception("Unkknown file version")
  val ntrack = file.readInt()
  val frameSize = (ntrack + 1) * 8
  val desc = file.readUTF()
  val metadata = for (i ← (1 to ntrack)) yield file.readUTF()
  val dataBegin = file.getFilePointer
  val timeBegin = file.readDouble()
  file.skipBytes(8 * ntrack)
  val estTimeStep = file.readDouble - timeBegin
  file.seek(file.length - frameSize)
  val timeEnd = file.readDouble

  //display
  val dataGraphMap = Map[Int, Int]()
  var xscale = 1d
  var yscale = 20d
  def windowBegin = -plot.transx / xscale
  def windowEnd = (-plot.transx + plot.getWidth) / xscale

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
        dataGraphMap foreach { kv ⇒
          updateGraph(kv._1)
        }
        plot.repaint()
      }
    })
    metadata.reverse foreach { s ⇒
      listModel.add(0, s)
    }
  }

  def toggleGraph() = synchronized {
    val track = listBox.getLeadSelectionIndex
    val graph = dataGraphMap.getOrElse(track, -1)
    if (graph == -1) {
      try {
        val colInt = Integer.parseInt(text.getText, 16)
        val col = new Color(colInt)
        dataGraphMap(track) = plot.graphs.size
        println("check1")
        plot.graphs = plot.graphs :+ (null, col, defaultStroke)
        println("check2")
        updateGraph(track)
        println("check3")
      } catch {
        case e ⇒
          JOptionPane.showMessageDialog(this, e.toString+" "+e.getMessage, "Error", JOptionPane.ERROR_MESSAGE)
          println("check4")
      }
    } else {
      for (i ← (0 to ntrack - 1)) {
        if (dataGraphMap.get(i) != None && dataGraphMap(i) > graph)
          dataGraphMap(i) -= 1
      }
      plot.graphs = plot.graphs diff Seq(plot.graphs(graph))
      System.gc()
    }
  }

  def updateGraph(track: Int) = {
    val data = loadGraph(track, windowBegin, windowEnd)
    val pointBuff = new ListBuffer[Point]
    var lastX: Option[Int] = None
    data foreach { tpl ⇒
      if (lastX.isEmpty || lastX.get<(tpl._1 * xscale).toInt){
        pointBuff += new Point((tpl._1 * xscale).toInt, (tpl._2 * -yscale + plot.getHeight / 2).toInt)
        lastX= Some((tpl._1 * xscale).toInt)
      }
    }
    val old = plot.graphs(dataGraphMap(track))
    plot.graphs(dataGraphMap(track)) = (pointBuff, old._2, old._3)
    System.gc()
  }

  def loadGraph(n: Int, begin: Double, end: Double): Array[(Double, Double)] = {
    val firstFrame = math.max(findFrame(begin) - frameSize, dataBegin)
    val lastFrame = findFrame(end)
    val res = new Array[(Double, Double)](((lastFrame - firstFrame) / frameSize).toInt)
    file.seek(firstFrame)
    for (i ← (0 to res.size - 1)) {
      val time = file.readDouble()
      file.skipBytes(8 * n)
      val value = file.readDouble()
      res(i) = (time, value)
      file.skipBytes(8 * (ntrack - n - 1))
    }
    res
  }

  def findFrame(time: Double): Long = {
    if (time <= timeBegin) dataBegin
    else if (time >= timeEnd) file.length - frameSize
    else {
      val estFrame = ((time - timeBegin) / estTimeStep).toInt
      file.seek(dataBegin + (estFrame * frameSize))
      val foundTime = file.readDouble()
      if (foundTime == time) file.getFilePointer() - 8
      else if (foundTime < time) seekForward(file.getFilePointer + frameSize - 8, time)
      else seekBackward(file.getFilePointer() - frameSize - 8, time)
    }
  }

  @tailrec
  final def seekForward(begin: Long, time: Double): Long = {
    file.seek(begin)
    if (file.readDouble() >= time) return file.getFilePointer - 8
    seekForward(file.getFilePointer() + frameSize - 8, time)
  }

  @tailrec
  final def seekBackward(begin: Long, time: Double): Long = {
    file.seek(begin)
    if (file.readDouble() < time) return file.getFilePointer() + frameSize - 8
    seekBackward(file.getFilePointer() - frameSize - 8, time)
  }

  override def finalize()={
    try{
      file.close()
    } finally {
      super.finalize()
    }
  }
}