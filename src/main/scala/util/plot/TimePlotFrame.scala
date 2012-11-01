package util.plot
import javax.swing.JFrame
class TimePlotFrame(val cont:TimePlotter) extends JFrame{
   getContentPane.add(cont)
   setSize(500,200)
   show
}