package util.realtime
import collection.mutable.Set
import util.plot._
class Simulator(var objects: Set[Simulable]) extends Simulable {
  def this(_objects: Simulable*) = this(Set(_objects: _*))
  def timestep(dt: Double) = {
    objects foreach { o => o.timestep(dt) }
  }

  def update() = {
    objects foreach { o => o.update() }
  }
  
  
}