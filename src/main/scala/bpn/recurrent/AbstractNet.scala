package bpn.recurrent
import bpn._
class AbstractNet(layers: Seq[AbstractLayer], conns: Set[(AbstractLayer, AbstractLayer)]) {
  def buildForward(outputMarks: Seq[Seq[AbstractLayer]], gd: () => GradientDescent) = {
    val bdr = new ForwardBuilder
    bdr.gd = gd
    import bdr._
    layers foreach { l => create input l.size named (l.debug + "@t0") }
    for (i <- 0 to outputMarks.length - 1) {
      layers foreach { l =>
        l match {
          case _: AbstractSigmoid => create sigmoid l.size named (l + "@t" + (i + 1))
          case _: AbstractLinear => create linear l.size named (l + "@t" + (i + 1))
	  case _: AbstractInput => create input l.size named (l.debug + "@t0")
        }
      }
      conns foreach { c => connection withConstraint (c._1 + "&" + c._2) from (c._1 + "@t" + i) to (c._2 + "@t" + (i + 1)) }
      outputMarks(i) foreach { l => make output (l + "@t" + (i + 1)) }
    }
  }
}

abstract class AbstractLayer(val size: Int) extends util.DebugInfo
case class AbstractSigmoid(override val size: Int) extends AbstractLayer(size)
case class AbstractLinear(override val size: Int) extends AbstractLayer(size)
case class AbstractInput(override val size: Int) extends AbstractLayer(size)
