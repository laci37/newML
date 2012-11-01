package bpn
import mathext._
trait LayerInput {
  def size: Int
  def dEdz: Matrix
  var inputs:Seq[Connection]
  def forward()
  def learn()
  def backward()
}
