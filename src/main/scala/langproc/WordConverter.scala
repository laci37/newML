package langproc
import breeze.linalg._

trait WordConverter{ 
  def convert(word:String):SparseVector[Double]
  def reverse(vec:Vector[Double]):String
}

abstract class CharWordConverter(maxLength:Int,caseSensitive:Boolean) extends WordConverter{ 
  
  val chars: Seq[Char]
  lazy val charToIntMap=Map((for(i<-(0 until chars.size)) yield chars(i)->i).toSeq :_*)

  lazy val letterSize= if(caseSensitive) chars.length+1 else chars.length

  def convertLetter(letter:Char):SparseVector[Double]={ 
    if(caseSensitive){ 
      val vec=SparseVector.zeros[Double](chars.length+1)
      if(charToIntMap.contains(letter))
	vec(charToIntMap(Character.toLowerCase(letter)))=1d
      if(!Character.isLowerCase(letter)) vec(chars.length)=1d
      vec
    } else { 
      val vec=SparseVector.zeros[Double](chars.length)
      if(charToIntMap.contains(letter))
	vec(charToIntMap(Character.toLowerCase(letter)))=1d
      vec
    }
  }

  def reverseLetter(vec:Vector[Double]):Char={ 
    var maxValue=Double.NegativeInfinity
    var maxIndex= -1
    for(i<-(0 until chars.length)) 
	if(vec(i)>maxValue) {maxValue=vec(i); maxIndex=i}
    chars(maxIndex)
  }

  def convert(word:String):SparseVector[Double]={ 
    val charVectors=for(c<-word) yield convertLetter(c)
    val padded=charVectors.padTo(maxLength,SparseVector.zeros[Double](letterSize))
    SparseVector.vertcat(padded :_*)
 }

  def reverse(vec:Vector[Double]):String={ 
    if(vec.size!=maxLength*letterSize) 
      throw new IllegalArgumentException
    val slices=for(i<-(0 until maxLength)) yield vec(i*letterSize until (i+1)*letterSize)
    new String((slices.map(reverseLetter)).toArray)
  }
}

class HunCharWC(maxLength:Int, caseSensitive:Boolean) extends CharWordConverter(maxLength,caseSensitive){ 
   val chars= Seq('a','á', 'b', 'c', 'd', 'e', 'é', 'f', 'g', 'h', 'i', 'í', 'j', 'k', 'l', 'm', 'n', 'o', 'ó', 'ö', 'ő', 'p', 'q', 'r', 's', 't', 'u', 'ú', 'ü', 'ű', 'v', 'w', 'x', 'y', 'z', '-')
} 
