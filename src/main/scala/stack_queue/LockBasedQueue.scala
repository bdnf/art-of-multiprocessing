package stack_queue

import java.util.concurrent.locks.ReentrantLock

class LockBasedQueue[T](capacity: Int) {

  var head, tail = 0
  val items = new Array[T](capacity)
  val lock: ReentrantLock = new ReentrantLock()

  def enq[T](x: T): Unit ={
    lock.lock()
    try {
      if (head - tail == items.length) {println("Queue is full"); return}
      items(tail % items.length) = x
      tail += 1

    } finally{
      lock.unlock()
    }
  }

  def deq[T](): Option[T] = {
    lock.lock()
    try{
      if (tail == head) { return None }
      val x = items(head % items.length)
      head +=1
      return Some(x)

    } finally{
      lock.unlock()
    }
  }


}
