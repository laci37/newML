package util.realtime
import collection.mutable.Set
class SimulatorRoot(objects: Set[Simulable]) extends Simulator(objects) {
  
  def this(_objects: Simulable*) = this(Set(_objects: _*))
  
  override def timestep(dt:Double)={
    super.timestep(dt)
    update()
  }
  
  def simulate(step:Double,c:Int)={
    for(i<-(1 to c)){
      timestep(step)
    }
  }
}