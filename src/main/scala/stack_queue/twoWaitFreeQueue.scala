package stack_queue

class twoWaitFreeQueue[T](capacity: Int) {
  /**
    * A single-enqueuer/single-dequeuer FIFO queue
    */

  @volatile var head, tail = 0
  val items = new Array[T](capacity)

  def enq[T](x: T): Unit = {
    if (tail-head == items.length) {println("Queue is full");return}
    items(tail % items.length) = x
    tail +=1                                  //linearization point
  }

  def deq[T](): Option[T] = {

      if (tail == head) { return None }       //linearization point
      val x = items(head % items.length)
      head +=1                                //linearization point
      return Some(x)

    }

}
