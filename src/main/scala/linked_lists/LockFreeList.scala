package linked_lists

import java.util.concurrent.atomic.AtomicMarkableReference

case class NodeMark[T](item: T,
                         key: Int,
                         var next: AtomicMarkableReference[NodeMark[T]] = new AtomicMarkableReference[NodeMark[T]](null, false)
                        )



class LockFreeList {
  //TODO change for generic type T

  var tail: NodeMark[Int] = new NodeMark(Integer.MAX_VALUE, Integer.MAX_VALUE,new AtomicMarkableReference[NodeMark[Int]](null, false))
  var head: NodeMark[Int] = new NodeMark(Integer.MIN_VALUE, Integer.MIN_VALUE,new AtomicMarkableReference[NodeMark[Int]](null, false) )


  while (!this.head.next.compareAndSet(null, this.tail, false, false)){}



  def find(head: NodeMark[Int], key: Int): (NodeMark[Int], NodeMark[Int]) = {
    var pred: NodeMark[Int] = null
    var curr: NodeMark[Int] = null
    var succ: NodeMark[Int] = null
    val marked = Array(false)
    // is curr marked?
    var snip = false

    def cas(): Option[(NodeMark[Int], NodeMark[Int])] = {
      while (true) {
        pred = head
        curr = pred.next.getReference
        while (true) {
          succ = curr.next.get(marked)
          while (marked(0)) { // replace curr if marked
            snip = pred.next.compareAndSet(curr, succ, false, false)
            if (!snip) cas()
            curr = pred.next.getReference
            succ = curr.next.get(marked)

            if (curr.key >= key) return Some((pred, curr))
            pred = curr
            curr = succ
          }
        }
      }
      None
    }

    cas() match {
      case Some(window) => return window
      case None => null
    }

  }

  def add(item: Int): Boolean = {
    val key = item.hashCode

    while (true) { // find predecessor and current entries
      val window = find(head, key)
      val pred = window._1
      val curr = window._2

      if (curr.key == key) return false
      else { // splice in new node
        val node = new NodeMark(item, key, new AtomicMarkableReference(curr, false))

        if (pred.next.compareAndSet(curr, node, false, false)) return true
      }
    }
    return false
  }

  def remove(item: Int): Boolean = {
    val key = item.hashCode
    var snip = false
    while (true) { // find predecessor and curren entries
      val window = find(head, key)
      val pred = window._1
      val curr = window._2
      // is the key present?
      if (curr.key != key) return false
      else {      // snip out matching node. mark as logically removed
        val succ = curr.next.getReference
        snip = curr.next.attemptMark(succ, true)
        if (snip) {
          pred.next.compareAndSet(curr, succ, false, false)
          return true
        }
      }
    }
    false
  }

  def contains(item: Int): Boolean = {
    val key = item.hashCode
    // find predecessor and current entries
    val window = find(head, key)
    val pred = window._1
    val curr = window._2
    curr.key == key
  }




}


