package bpn
import mathext._
import util.DebugInfo
trait LayerInput extends DebugInfo{
  def size: Int
  def dEdz: Matrix
  var inputs:Seq[Connection]
  def forward()
  def learn()
  def backward()
}
