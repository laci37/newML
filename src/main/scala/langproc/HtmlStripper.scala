package langproc

object HtmlStripper{ 
  def strip(html:String)={ 
    val BodyRegex="""<(?:body|BODY)[^>]*>([\s\S]*)</(?:body|BODY)>""".r
    val ScriptRegex="""<(?:script|SCRIPT)[^>]*+>[\s\S]*?</(?:script|SCRIPT)>""".r
    val TagRegex="""<[^>]+>|&\w*;|\n|\r""".r
    val body=BodyRegex.findFirstMatchIn(html).get.group(0)
    val split1=ScriptRegex.split(body)
    val text1=split1.mkString(" ")
    val split2=TagRegex.split(text1)
    split2.mkString(" ")
  }
}
