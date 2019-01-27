package consensus
//https://stackoverflow.com/questions/16921168/scala-generic-method-no-classtag-available-for-t
import scala.reflect
import scala.reflect._

trait Consensus[T] {
  def decide(value: T): T
}

abstract class ConsensusProtocol[T:reflect.ClassTag] extends Consensus[T] {
  val N: Int
  var proposed: Array[T] = new Array[T](N)

  def propose(value: T) = {
    val i = Thread.currentThread.getId.asInstanceOf[Int] % N
    proposed(i) = value
  }

  def decide(value: T): T  //which thread was first

}
