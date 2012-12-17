package bpn
import mathext._
import util.DebugInfo
import breeze.linalg._
trait LayerInput extends DebugInfo{
  def size: Int
  def dEdz: Matrix[Double]
  var inputs:Seq[Connection]
  def forward()
  def learn()
  def backward()
}
