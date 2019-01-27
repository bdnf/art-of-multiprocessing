package linked_lists

import java.util.concurrent.locks.ReentrantLock

trait Set[T]{
  def add[T](x:T)
  def remove[T](x:T)
  def contains[T](x:T)
}

case class Node[T](item: T, key: Int, var next: Node[T])

class CoarseList {
  //TODO change for generic type T

  //TODO create sentinel nodes as Empty

  val tail: Node[Int] = new Node(Integer.MAX_VALUE, Integer.MAX_VALUE, null)
  val head: Node[Int] = new Node(Integer.MIN_VALUE, Integer.MIN_VALUE, tail)

  var lock = new ReentrantLock()
//  def this() = this {
//
//  }

  def add(item: Int): Boolean = {
    var pred, curr: Node[Int] = head
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
        val node = Node(item, key, curr)
        //node.next = curr
        pred.next = node
        return true
      }

    } finally {
      lock.unlock()
    }

  }

  def remove(item: Int): Boolean = {

    var pred, curr: Node[Int] = null
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



object CoarseGrainedSet extends App {

}
