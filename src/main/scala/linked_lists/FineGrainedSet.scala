//package linked_lists

import java.util.concurrent.locks.ReentrantLock
import mutex.BakeryLock

case class Node[T](item: T, key: Int, var next: Node[T], var lock: BakeryLock = new BakeryLock())

class FineGrainedSet {
  //TODO change for generic type T
  //ensures that locks are acquired in the same order always. Otherwise deadlock may occure
  val tail = new Node(Integer.MAX_VALUE, Integer.MAX_VALUE, null)
  val head = new Node(Integer.MIN_VALUE, Integer.MIN_VALUE, tail)


  def add(item: Int): Boolean = {

    val key = item.hashCode()
    head.lock.lock()
    var pred: Node[Int] = head
    try {
      var curr = pred.next
      curr.lock.lock()
      try {
        while (curr.key < key) {
          pred.lock.unlock()
          pred = curr
          curr = curr.next
          curr.lock.lock()
        }
        if (key == curr.key) {
          return false
        } else {
          val newNode = new Node(item, key, next = curr)
          pred.next = newNode
          return true
        }
      } finally {
        curr.lock.unlock()
      }

    } finally {
      pred.lock.unlock()
    }

  }

  def remove(item: Int): Boolean = {

    var pred, curr: Node[Int] = head
    val key = item.hashCode()
    head.lock.lock()
    try {
      pred = head
      curr = pred.next
      curr.lock.lock()
      try {
        while (curr.key < key) {
          pred.lock.unlock()
          pred = curr
          curr = curr.next
          curr.lock.lock()
        }
        if (key == curr.key) {
          pred.next = curr.next
          return true
        }
        return false
      } finally curr.lock.unlock()

    } finally {
      pred.lock.unlock()
    }

  }


}
