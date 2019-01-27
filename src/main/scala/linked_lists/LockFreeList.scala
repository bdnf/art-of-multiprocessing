package linked_lists

import java.util.concurrent.atomic.AtomicMarkableReference

case class NodeMark[T](item: T,
                         key: Int,
                         var next: AtomicMarkableReference[NodeMark[T]] = new AtomicMarkableReference[NodeMark[T]](null, false)
                        )



class LockFreeList {
  //TODO change for generic type T

  var tail: NodeMark[Int] = new NodeMark(Integer.MAX_VALUE, Integer.MAX_VALUE,new AtomicMarkableReference[NodeMark[Int]](null, false))
  var head: NodeMark[Int] = new NodeMark(Integer.MIN_VALUE, Integer.MIN_VALUE,new AtomicMarkableReference[NodeMark[Int]](null, false) )


  while (!this.head.next.compareAndSet(null, this.tail, false, false)){}


  def find(head: NodeMark[Int], key: Int): (NodeMark[Int], NodeMark[Int]) = {
    var pred: NodeMark[Int] = null
    var curr: NodeMark[Int] = null
    var succ: NodeMark[Int] = null
    val marked = Array(false)
    // is curr marked?
    var snip = false

    def findPair: (NodeMark[Int], NodeMark[Int]) = {
      while (true) {
        pred = head
        curr = pred.next.getReference
        while (true) {
          succ = curr.next.get(marked)
          while (marked(0)) {
            // replace curr if marked
            snip = pred.next.compareAndSet(curr, succ, false, false)
            if (!snip) findPair //sentinel node wiill never be marked
            curr = pred.next.getReference
            succ = curr.next.get(marked)
          }
          if (curr.key >= key) return (pred, curr)
          pred = curr
          curr = succ
        }
      }
      (pred, curr)
    }

    findPair


  }

  def add(item: Int): Boolean = {
    println("Adding node")
    val key = item.hashCode
    if (head.next.getReference != null) {
      while (true) { // find predecessor and current entries

        val window = find(head, key)
        val pred = window._1
        val curr = window._2

        if (curr.key == key) return false
        else { // splice in new node

          val node = new NodeMark(item, key, new AtomicMarkableReference(curr, false))

          if (pred.next.compareAndSet(curr, node, false, false)) {
            println("Node added: ", item)
            return true
          }
        }
      }
    } else {
      val node = new NodeMark(item, key, new AtomicMarkableReference(head, false))

      if (head.next.compareAndSet(null, node, false, false)) {
        return true
      }
    }
    return false
  }

  def remove(item: Int): Boolean = {
    println("Removing node", item)
    val key = item.hashCode
    var snip = false
    while (true) { // find predecessor and curren entries
      val window = find(head, key)
      val pred = window._1
      val curr = window._2
      // is the key present?
      if (curr.key != key) return false
      else {      // snip out matching node. mark as logically removed
        val succ = curr.next.getReference
        snip = curr.next.attemptMark(succ, true)
        if (snip) {
          pred.next.compareAndSet(curr, succ, false, false)
          return true
        }
      }
    }
    false
  }

  def contains(item: Int): Boolean = {
    val key = item.hashCode
    // find predecessor and current entries
    val window = find(head, key)
    val pred = window._1
    val curr = window._2
    curr.key == key
  }

//  def foreach(f: Int => Boolean): Unit = {
//    val curr = head
//    while(curr.next == null)
//    f(curr.next)
//  }

  def printElements: Unit = {
   val curr = head
   var next = curr.next.getReference
   while(next.key < tail.key){
     print("Element: ", next.item)
     next = next.next.getReference
   }
  }

}

object LockFreeList extends App {

  var list = new LockFreeList
  val threads = (1 to 10).map(i => {
    new Thread(() => {
      if (i%5 == 0) list.remove(scala.util.Random.nextInt(10))
      else list.add(scala.util.Random.nextInt(10))
    })
  })

  //threads.foreach(t => println(t.getId.asInstanceOf[Int]))
  threads.foreach(_.start())
  threads.foreach(_.join())
  print("List contains: ")
  list.printElements
}


