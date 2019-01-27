package consensus

import scala.reflect.ClassTag

class Assign23(init: Int) {
  var r = Array.fill(3)(init) //init - value to fill

  def assign(v0: Int, v1: Int, i0: Int, i1: Int) = synchronized {
    r(i0) = v0
    r(i1) = v1
  }

  def read(i: Int): Int = synchronized(return read(i))
}

class MultipleAssignment[T:ClassTag] extends ConsensusProtocol[T] {
  override val N: Int = 3

  val assign23: Assign23 = new Assign23(-1)

  def decide(value: T): T = {
    propose(value)
    val i = Thread.currentThread.getId.asInstanceOf[Int] % N
    val j = 1 - i
    //double assignment
    assign23.assign(i, i, i, i+1)
    val other = assign23.read((i+2) % N)
    if (other == -1 || other == assign23.read(1)) return proposed(i) //i Wins
    else return proposed(j) //i loses
  }
}
