package util
trait DebugInfo{ 
  var debug=""
  override def toString()=debug
  
  /**
   * Sets the debug string and returns this. Added for DSLs
   */ 
  def named(name:String)={ 
    debug=name
    this
  }
}
