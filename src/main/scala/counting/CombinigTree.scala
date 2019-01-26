package counting

import java.util

import counting.Node.{CStatus, FIRST}


class CombiningTree(width: Int) {

  var nodes =  new Array[Node](width-1)
  nodes(0) = new Node()
  for (i <- 0 to (nodes.length - 1)) {
    nodes(i) = nodes((i - 1) / 2)
  }
  var leaf = new Array[Node]((width + 1) / 2)
  for (i <- 0 to (leaf.length - 1)) {
    leaf(i) = nodes(nodes.length - i - 1)
  }

  def getAndIncrement(): Unit = {
    var stack = new util.Stack[Node]()
    var myLeaf: Node = leaf((Thread.currentThread.getId.asInstanceOf[Int] % width)/2)

    var node: Node = myLeaf
    //precombinig phase
    println("Started precombing phase...")
    while (node.precombine()) node = node.parent
    var stop:Node = node
    //combining phase
    println("Started combing phase...")
    node = myLeaf
    var combined = 1
    while (node != stop) {
      combined = node.combine(combined)
      stack.push(node)
      node = node.parent
    }
    //operation phase
    println("Started operation phase...")
    val prior = stop.op(combined)
    println("Started distribution phase...")
    while (!stack.empty) {
      node = stack.pop
      node.distribute(prior)
    }

  }
}


object Node {
  sealed trait CStatus

  case object IDLE extends CStatus
  case object FIRST extends CStatus  //1st is partner for combining
  case object SECOND extends CStatus
  case object DONE extends CStatus//1st completed and deposited value for 2nd
  case object ROOT extends CStatus
}

class Node(var cStatus: CStatus, var locked: Boolean, var parent: Node) {

  import Node.{IDLE, ROOT, SECOND, DONE}

  var firstValue: Int = 0
  var secondValue: Int = 0
  var oldValue, result: Int = 0

  def this() = this(
    cStatus = Node.ROOT,
    locked = false,
    parent = null
  )


  def this(myParent: Node) = this(
    cStatus = Node.IDLE,
    locked = false,
    parent = myParent
  )

  def precombine(): Boolean = synchronized {
    while (locked) wait()

    cStatus match {
      case IDLE => {
        cStatus = FIRST;
        return true
      }

      case FIRST => {
        locked = true
        cStatus = SECOND
        return false
      }

      case ROOT => {println("ROOT node reached: ", result); return false}
      case _ => throw new NoSuchElementException
    }
  }

  def combine(combined: Int): Int = synchronized {
    while (locked) wait() //Wait until node is unlocked

    locked = true //Lock out late attempts to combine
    firstValue = combined //Remember our contribution
    cStatus match {
      case FIRST => return firstValue //1st thread is alone
      case SECOND => return firstValue + secondValue; //Combine with 2nd
      case _ => throw new NoSuchElementException
    }
  }

  def op(combined: Int): Int = synchronized {
    cStatus match {
      case ROOT => {
        oldValue = result
        result += combined
        println("Result is: ", result)
        return oldValue // add sum to root, return prior
      }
      case SECOND => {
        secondValue = combined
        locked = false;
        notifyAll() //unlock node, notify 1nd
        while (cStatus != DONE) wait()
        locked = false
        notifyAll() // unlock and return
        cStatus = IDLE
        print("returning result", result)
        return result
      }
      case _ => throw new NoSuchElementException

    }
  }

  def distribute(prior: Int): Unit = synchronized {
    cStatus match {
      case FIRST => { //No combining, unlock and reset
        cStatus = IDLE
        locked = false;
        notifyAll()
        return
      }
      case SECOND => { //Notify 2nd thread that result is available
        result = prior + firstValue
        cStatus = DONE
        notifyAll()
        return
      }
      case _ =>
    }
  }
}


object CountingTree extends App {

  val tree = new CombiningTree(8)

  val threads = (1 to 5).map(_ => new Thread(() => tree.getAndIncrement()))
  threads.foreach(t => println(t.getId.asInstanceOf[Int]))
  threads.foreach(_.start())
  threads.foreach(_.join())
}

