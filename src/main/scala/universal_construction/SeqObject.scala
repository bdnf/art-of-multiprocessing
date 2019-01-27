package universal_construction

import consensus.nConsensus

import scala.reflect.ClassTag


abstract class Invoc(method: String, args: Array[Object])

abstract class Response(value: Object)

class Node(var invoc: Invoc, numOfThreads: Int) {

  var seq = 0
  val decideNext: nConsensus[Node] = new nConsensus(numOfThreads)
  var next: Node = null

  def max(arr: Array[Node]): Node = {
    var max = arr(0)
    for(i <- 0 until arr.length){
      if (max.seq < arr(i).seq) max = arr(i)
    }
    return max
  }
}

trait SeqObject {
  def apply(invoc: Invoc): Response

}
