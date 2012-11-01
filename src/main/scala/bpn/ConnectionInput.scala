package bpn
import mathext._
trait ConnectionInput {
  def dEdy:Matrix
  def forward()
  def learn()
}