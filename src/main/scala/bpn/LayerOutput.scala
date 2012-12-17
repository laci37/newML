package bpn
import mathext._
import util.DebugInfo
import breeze.linalg._
trait LayerOutput extends DebugInfo{
 def size:Int
 def y:Matrix[Double]
 var outputs:Seq[ConnectionInput]
 def backward()
 def forward() 
}
