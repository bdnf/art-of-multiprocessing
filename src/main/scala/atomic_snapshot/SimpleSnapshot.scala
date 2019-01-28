package atomic_snapshot

class SeqSnapshot[T](capacity: Int, init: T, numOfThreads: Int) extends Snapshot[T] {
  var a_value: Array[T] = Array.fill(capacity)(init)
  //wait-free
  override def update[T](v: T): Unit = {
    val id = Thread.currentThread.getId.asInstanceOf[Int] % numOfThreads

    a_value(id) = v
  }
  //obstruction-free
  override def scan(): Array[T] = {
    return a_value.clone()
  }
}


class StampedValue[T](var stamp: Long, var value: T, var snap: Array[T]) {
  def this(value: T) = this(0, value, null)
  def this(stamp: Long, value: T) = this(stamp, value, null)
}

class SimpleSnapshot[T](capacity: Int, init: T, numOfThreads: Int) extends Snapshot[T] {
  /**
    * Each update() call calls scan(), and appends the result of the scan to the value’s label.
    *
    * @stamp field incremented each time the thread updates its value
    * @value field containing the register’s actual value
    * @snap field containing that thread’s most recent scan
    */

  var a_table: Array[StampedValue[T]] = Array.fill(capacity)(new StampedValue[T](init))

  //wait-free
  override def update[T](v: T): Unit = {
    val id = Thread.currentThread.getId.asInstanceOf[Int] % numOfThreads
    val oldValue = a_table(id)
    val newValue = new StampedValue[T](oldValue.stamp + 1, v)
    a_table(id) = newValue
  }

  private def collect[T](): Array[StampedValue[T]] = return a_table.clone()

  //lock-free
  override def scan(): Array[T] = {
    var oldCopy: Array[StampedValue[T]] = collect()

    def scanAgain(): Array[T] = {
      val newCopy: Array[StampedValue[T]] = collect()
      if (oldCopy != newCopy) {
        oldCopy = newCopy;
        scanAgain()
      }
      var result = new Array[T](a_table.length)
      for (j <- 0 until a_table.length)
        result(j) = newCopy(j).value
      return result
    }

    scanAgain()

  }
}
