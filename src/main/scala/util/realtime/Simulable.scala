package util.realtime

trait Simulable {
  def timestep(dt:Double):Any
  def update()
}