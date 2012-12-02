package util
import breeze.linalg._
object NanChecker {

  def nanCheck(d:Double){ 
    if(d!=d){ 
      throw new NanException
    }
    else if(d==Double.PositiveInfinity || d==Double.NegativeInfinity){ 
      throw new InfException
    }
  }

  def nanCheck(v:Vector[Double]){ 
    v.values foreach nanCheck _
  }

  def nanCheck(m:Matrix[Double]){ 
    m.values foreach nanCheck _
  }

  class NanException extends Exception
  class InfException extends Exception
}
