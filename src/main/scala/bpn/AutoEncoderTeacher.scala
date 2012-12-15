package bpn
import breeze.linalg._
import data._

class AutoEncoderTeacher(val n:Net, data:Seq[VectorObservation]) 
  extends Teacher(n ,data.map(v=> new VectorExample(v.features,v.features)))
