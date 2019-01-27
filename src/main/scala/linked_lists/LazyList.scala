//package linked_lists

import mutex.BakeryLock

case class NodeMarked[T](item: T,
                   key: Int,
                   var next: NodeMarked[T],
                   var lock: BakeryLock = new BakeryLock(),
                   var marked: Boolean = false
                  )

class LazyList {
  //TODO change for generic type T

  /**
    * contains() is wait-free
    * add(), remove() are blocking. If one thread delayed others may also be delayed
    */

  val tail = new NodeMarked(Integer.MAX_VALUE, Integer.MAX_VALUE, null)
  val head = new NodeMarked(Integer.MIN_VALUE, Integer.MIN_VALUE, tail)

  private def validate[T](pred: NodeMarked[T], curr: NodeMarked[T]): Boolean = {
    return !pred.marked && !curr.marked && (pred.next == curr)
  }

  def contains(item: Int): Boolean = {
    //wait-free
    val key = item.hashCode()
    var curr = head
    while(curr.key < key) curr = curr.next

    return curr.key == key && !curr.marked // linearization point when unmarked node is found
  }

  def add(item: Int): Boolean = {
    //blocking
    val key = item.hashCode()
    while (true) {
      var pred = head
      var curr = pred.next
      while (curr.key < key) {
        pred = curr
        curr = curr.next
      }
      pred.lock.lock()
      try {
        curr.lock.lock()
        try {
          if (validate(pred, curr)) {
            if (key == curr.key) {
              return false
            } else {
              //Add node
              val newNode = new NodeMarked(item, key, next = curr)
              pred.next = newNode
              return true
            }
          }
        } finally curr.lock.unlock()
      } finally pred.lock.unlock()

    }
    return false
  }

  def remove(item: Int): Boolean = {
    //blocking
    val key = item.hashCode()
    while(true) {
      var pred = head
      var curr = head.next
      while(curr.key == key){
        pred = curr; curr = curr.next
      }
      pred.lock.lock()
      try {
        curr.lock.lock()
        try {
          if (validate(pred, curr)) {
            if (key != curr.key) {
              return true
            } else {
              //linarization point
              curr.marked = true
              pred.next = curr.next
              return true
            }
          }
        } finally curr.lock.unlock()

      } finally {
        pred.lock.unlock()
      }
    }
    return false
  }
}

