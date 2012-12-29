package bpn
import breeze.linalg._

class LSTMem(size: Int) extends ConnectionStubSupervisor with LayerStubSupervisor {

  val in = new LinearLayer(size)
  val inConn = new ConnectionStub(in, this)
  val inGate = new SigmoidLayer(size)
  val inGateConn = new ConnectionStub(inGate, this)
  val loopGate = new SigmoidLayer(size)
  val loopGateConn = new ConnectionStub(loopGate, this)
  val outGate = new SigmoidLayer(size)
  val outGateConn = new ConnectionStub(outGate, this)
  val out = new LayerStub(size, this)

  var prev: Option[LSTMem] = None
  var next: Option[LSTMem] = None

  protected var dEdLoopInput_store: DenseMatrix[Double] = null
  def dEdLoopInput = dEdLoopInput_store
  var loop: DenseMatrix[Double] = null

  protected var fwdCount = 0
  protected var backCount = 0
  protected var learnCount = 0
  protected var getWeightSeqCount = 0
  protected var loadWeightsCount = 0

  protected def nForwardCallers = if (prev.isDefined) 5 else 4
  protected def nBackwardCallers = if (next.isDefined) 2 else 1

  def forward() = {
    fwdCount += 1
    if (fwdCount >= nForwardCallers) {
      fwdCount = 0
      loop = in.y :* inGate.y
      prev foreach {
        this.loop += _.loop :* this.loopGate.y
      }
      out.y = loop :* outGate.y
      next foreach { _.forward() }
      out.forward()
    }
  }

  def backward() = {
    backCount += 1
    if (backCount >= nBackwardCallers) {
      backCount = 0
      outGateConn.dEdy = out.sumdEdy :* loop
      var loopErr = out.sumdEdy :* outGate.y
      next foreach {
        loopErr += _.dEdLoopInput
      }
      inConn.dEdy = inGate.y :* loopErr
      inGateConn.dEdy = in.y :* loopErr
      dEdLoopInput_store = loopGate.y :* loopErr
      loopGateConn.dEdy = if (prev.isDefined) prev.get.loop :* loopErr
      else DenseMatrix.zeros(size, in.y.cols)
    }
  }

  def learn() = {
    learnCount += 1
    if (learnCount >= nForwardCallers) {
      learnCount = 0
      out.learn()
      next foreach (_.learn())
    }
  }

  def getWeightSeq = {
    getWeightSeqCount += 1
    if (getWeightSeqCount >= nForwardCallers) {
      getWeightSeqCount = 0
      out.getWeightSeq ::: (if (next.isDefined) next.get.getWeightSeq else Nil)
    } else Nil
  }

  def loadWeights(data: List[Matrix[Double]]) = {
    loadWeightsCount += 1
    if (loadWeightsCount >= nForwardCallers) {
      loadWeightsCount = 0
      var outret = out.loadWeights(data)
      next foreach {n=>
        outret = n.loadWeights(outret)
      }
      outret
    } else data
  }
}
