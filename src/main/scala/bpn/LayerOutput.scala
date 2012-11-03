package bpn
import mathext._
import util.DebugInfo
trait LayerOutput extends DebugInfo{
 def size:Int
 def y:Matrix
 var outputs:Seq[ConnectionInput]
 def backward()
 def forward() 
}
