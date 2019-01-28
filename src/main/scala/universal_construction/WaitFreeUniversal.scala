package universal_construction

class WaitFreeUniversal(numOfThreads: Int) extends SeqObject {

  /**
    * Using helping:
    * announce[i] is the node thread i is currently trying to append to the list
    * a thread first announces its new node and tries to append to head[], if fails -> other thread will help
    *
    * One thread might append the node, and set the node’s sequence number,
    * at the same time that another thread appended the same node and set its sequence number.
    * The algorithm avoids this error because of the order in which threads read the maximum head[] array
    * value and the sequence number of a node in the announce[] array.
    */

  var tail: Node = new Node(null, numOfThreads)
    tail.seq = 1
    val head_arr: Array[Node] = Array.fill(numOfThreads)(tail) //init
    val announce: Array[Node] = Array.fill(numOfThreads)(tail)

    def max(arr: Array[Node]): Node = {
      var max = arr(0)
      for(i <- 0 until arr.length){
        if (max.seq < arr(i).seq) max = arr(i)
      }
      return max
    }

    override def apply(invoc: Invoc): Response = {
      val i = Thread.currentThread.getId.asInstanceOf[Int] % numOfThreads
      var prefer: Node = new Node(invoc, numOfThreads)
      announce(i) = new Node(invoc, numOfThreads)
      head_arr(i) = max(head_arr)
      while (announce(i).seq == 0) {
        val before: Node = head_arr(i)
        val help = announce((before.seq+1)%numOfThreads)

        if (help.seq == 0) prefer = help
        else prefer = announce(i)

        val after = before.decideNext.decide(prefer)
        before.next = after
        after.seq = before.seq + 1
        head_arr(i) = after
      }
      val myObj: SeqObject = null
      var current: Node = tail.next

      while(current != announce(i)) {
        myObj.apply(current.invoc)
        current = current.next
      }
      head_arr(i) = announce(i)
      return myObj.apply(current.invoc)
    }


}
