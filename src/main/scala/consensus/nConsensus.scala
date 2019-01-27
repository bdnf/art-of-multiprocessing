package consensus

import java.util.concurrent.atomic.AtomicInteger

import scala.reflect.ClassTag

class nConsensus[T:ClassTag](n: Int) extends ConsensusProtocol[T] {
  override val N: Int = n
  val INIT = -1

  private val r = new AtomicInteger(INIT)

  //if id is first -> decide own value,
  //else take value written in register by fastest thread

  override def decide(value: T): T = {
    propose(value)
    val id = Thread.currentThread.getId.asInstanceOf[Int] % N
    if (r.compareAndSet(INIT, id)) return proposed(id)
    else return proposed(r.get())
  }

}
