package stack_queue

import java.util.concurrent.atomic.AtomicReference

import scala.util.Random

class BackoffStack[T] {

  /**
    * call by pop() does not have an ABA problem (see Chapter 10)
    * because the Java garbage collector ensures that a node cannot be reused by one thread.
    *
    * Disadvantage: top is a bottleneck
    */
  val top = new AtomicReference[Node[T]](null)
  val backoffMin = 100 //ns
  val backoffMax = 1500 //ns
  val backoff: Backoff = Backoff(backoffMin,backoffMax)

  def tryPush[T](node: Node[T]): Boolean = {
    val oldTop = top.get()
    node.next = oldTop.next
    return top.compareAndSet(oldTop, node)
  }

  def push[T](value: T): Unit = {
    val node = new Node(value, null)
    while(true){
      if (tryPush(node)){
        return
      } else {
        backoff.backoff()
      }
    }
  }
}

case class Backoff(minTime: Int, maxTime: Int) {
  def backoff() = {
    val t = Random.nextInt(math.abs(maxTime - minTime))
    Thread.sleep(t) //nanosec
  }
}
