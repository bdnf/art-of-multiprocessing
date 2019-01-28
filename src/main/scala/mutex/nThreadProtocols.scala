package mutex

trait Lock {
  def lock(): Unit
  def unlock(): Unit
}

class Filter extends Lock {

  /**
    * Deadlock-free
    * creates n-1 waiting rooms that thread traverses to be able to acquire the lock
    *
    * Properties:
    * At least one thread trying to enter level i succeeds.
    * If more than one thread is trying to enter level , then at least one is blocked
    *
    * first-come-first-served if, whenever, thread A finishes
    * its doorway before thread B starts its doorway
    */

  var level: Array[Int] = Array[Int](0)
  var victim: Array[Int] = Array[Int](0)
  var n:Int = 0

  def this(n: Int) = {
    this()
    level = Array[Int](n)
    victim = Array[Int](n)
    this.n = n
    for (i <- 0 to n) level(i) = 0
  }

  override def lock(): Unit = {
    val me = Thread.currentThread.getId.asInstanceOf[Int] % n
    for (i <- 1 until n) { //Doorway section
      //CS at level n-1
      //attempt level i
      level(me) = i
      victim(i) = me

      //spin while conflict exists at the same or higher level exist
      //while (( exist(k) != me | level(k) >= i) && (victim(i) == me)) wait()

      while(existThreadOnHigherLevel(i) && (victim(i) == me)) wait()  //Waiting section
    }
  }

  private def existThreadOnHigherLevel(index: Int): Boolean = {

    for (k <- index until n) if (k !=index && level(k) >= index ) true
    false
  }

  override def unlock(): Unit = {
    val i = Thread.currentThread.getId.asInstanceOf[Int] % n
    level(i) = 0
  }
}

class BakeryLock extends Lock {
  /**
    * Strictly first-come-first-served
    * flag(i) indicates if Thread waht to enter CS
    * each thread generates a label which is max+1
    * to avoid acquiring same label (label(i),i) < (label(j),j) ordering is used
    * !label numbers are always increasing, may wrap around in future
    *
    * The principal drawback is the need to read and write n distinct locations
    */
  var n: Int = 0
  var flag: Array[Boolean] = Array(false)
  var label: Array[Int]  = Array(0)


  def this(n: Int) = {
    this()
    flag = Array.fill[Boolean](n)(false)
    label = Array.fill[Int](n)(0)
    this.n = n
  }

  override def lock(): Unit = {
    val i = Thread.currentThread.getId.asInstanceOf[Int] % n
    flag(i) = true
    label(i) = label.reduceLeft(_ max _) + 1  //max of array +1
    while (existThreadWithLowerLabel(i)) wait()
  }

  private def existThreadWithLowerLabel(index: Int): Boolean = {
    //lexicographical ordel (i, label(i))
    var i = 0
    while (i < label.length) {
      if (i < index && label(i) < label(index)) return false
    }
    return true
  }

  override def unlock(): Unit = {
    val i = Thread.currentThread.getId.asInstanceOf[Int] % n
    flag(i) = false
  }
}

object nThreadProtocols {

}
