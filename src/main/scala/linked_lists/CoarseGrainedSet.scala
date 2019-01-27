package linked_lists

import java.util.concurrent.locks.ReentrantLock

import mutex.BakeryLock

trait Set[T]{
  def add[T](x:T)
  def remove[T](x:T)
  def contains[T](x:T)
}

case class Node[T](item: T, key: Int, var next: Node[T])

class CoarseList[T] {

  val tail = new Node[T](Int(Integer.MIN_VALUE), Integer.MIN_VALUE, null)
  val head = new Node[T](Int(Integer.MIN_VALUE), Integer.MIN_VALUE, tail)

  private val lock = new ReentrantLock()

//  def this() = this {
//
//  }

  def add[T](item: T): Boolean = {
    var pred, curr: Node[T] = null
    val key = item.hashCode()
    lock.lock()
    try {

      pred = head
      curr = pred.next
      while( curr.key < key ){
        pred = curr
        curr = curr.next
      }
      if (key == curr.key){
        return false
      } else {
        val node = new Node(item, key, next = curr)
        //node.next = curr
        pred.next = node
        return true
      }

    } finally {
      lock.unlock()
    }

  }

  def remove[T](item: T): Boolean = {

    var pred, curr: Node[T] = null
    val key = item.hashCode()
    lock.lock()
    try {
      pred = head
      curr = pred.next
      while( curr.key < key ){
        pred = curr
        curr = curr.next
      }
      if (key == curr.key){
        pred.next = curr.next
        return true
      } else return false

    } finally {
      lock.unlock()
    }

  }
}



object LinkedListBasedSet extends App {
 
}
