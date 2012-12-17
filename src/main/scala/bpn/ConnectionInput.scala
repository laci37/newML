package bpn
import mathext._
import breeze.linalg._
trait ConnectionInput {
  def dEdy:Matrix[Double]
  def forward()
  def learn()
}
