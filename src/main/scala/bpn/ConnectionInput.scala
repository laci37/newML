package bpn
import mathext._
import breeze.linalg._
trait ConnectionInput {
  def dEdy:DenseMatrix[Double]
  def forward()
  def learn()
}
