package universal_construction

class LockFreeUniversalAlgorithm(numOfThreads: Int) extends SeqObject {
  /**
    * A thread tries to append its node by proposing as input to a consensus protocol on the head’s decideNext field
    * When the thread’s node is appended, it sets the node’s sequence number.
    * To locate head of the log:
    * head[i] is the last node in the list that thread i has observed.
    * The head is the node with the
    * maximum sequence number among the nodes referenced in the head[] array
    */

  var tail: Node = new Node(null, numOfThreads)
  tail.seq = 1
  val head_arr: Array[Node] = Array.fill(numOfThreads)(tail) //init

  def max(arr: Array[Node]): Node = {
    var max = arr(0)
    for(i <- 0 until arr.length){
      if (max.seq < arr(i).seq) max = arr(i)
    }
    return max
  }

  override def apply(invoc: Invoc): Response = {
    val i = Thread.currentThread.getId.asInstanceOf[Int] % numOfThreads
    val prefer: Node = new Node(invoc, numOfThreads)
    while (prefer.seq == 0) {
      val before: Node = max(head_arr)
      val after = before.decideNext.decide(prefer)
      before.next = before
      after.seq = before.seq + 1
      head_arr(i) = after

    }
    val myObj: SeqObject = null
    var current: Node = tail.next

    while(current != prefer) {
      myObj.apply(current.invoc)
      current = current.next
    }
    return myObj.apply(current.invoc)
  }




}
