package bpn
import mathext._
trait LayerOutput {
 def size:Int
 def y:Matrix
 var outputs:Seq[ConnectionInput]
 def backward()
 def forward() 
}
