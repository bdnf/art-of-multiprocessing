package consensus

import scala.collection.mutable
import scala.reflect.ClassTag

class twoQueueConsensus[T](implicit m: ClassTag[T]) extends ConsensusProtocol[T]{
  //Wait-free
  val WIN: Int = 1
  val LOSE: Int = 0

  override val N: Int = 2
  val queue = new mutable.Queue[Int]()
  queue.enqueue(WIN)
  queue.enqueue(LOSE)

  override def decide(value: T): T = {
    propose(value)
    val status = queue.dequeue()
    val id = Thread.currentThread.getId.asInstanceOf[Int] % N
    if (status == WIN ) return proposed(id)
    else return proposed(1 - id)
  }
}
