package stack_queue

import java.util.concurrent.atomic.AtomicReference

class Node[T](var value: T,
              var next: AtomicReference[Node[T]]
             )

abstract class LockFreeQueue[T](capacity: Int) {
    /**
      *implementation prevents method calls from starving
      * by having the quicker threads help the slower threads.
      *
      * An enqueuer creates a new node with the new value to be enqueued
      * reads tail, and finds the node that appears to be last
      * To verify that node is indeed last, it checks whether that node has a successor.
      * If so, the thread attempts to append the new node by calling compareAndSet().
      */

    var tail: Node[T]
    var head: Node[T]

    def enq[T](x: T): Unit = {

      var node = new Node(x, null)
      while (true) {
        var last = tail
        var next = tail.next.get()
        if (last == tail){
          if(next == null){
            if (last.next.compareAndSet(next, node)) {   //append new node
              tail.next.compareAndSet(last, node)        //change tail from prior last to current last
              return
            }
          } else {
            tail.next.compareAndSet(last, next)
          }
        }
      }

    }

    def deq[T](): Option[T] = {

      while(true){
        val first = head
        val last = tail.next.get()
        val next = first.next.get()
        if(first == head){
          if (first == last){
            if (next == null) {
              None
            }
            tail.next.compareAndSet(last, next)
          } else {
            val value = next.value
            if (head.next.compareAndSet(first, next))
              return Some(value)
          }
        }
      }
      None
    }

}
