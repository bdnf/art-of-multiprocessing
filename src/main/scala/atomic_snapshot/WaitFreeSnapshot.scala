package atomic_snapshot

class WaitFreeSnapshot[T](capacity: Int, init: T, numOfThreads: Int) extends Snapshot[T] {
  /**
    * each update() call helps a scan() it may interfere with, by taking a snapshot before modifying its register
    * a thread moves if it completes an update()
    */

  var a_table: Array[StampedValue[T]] = Array.fill(capacity)(new StampedValue[T](init))

  //wait-free. update() helps scan()
  override def update[T](v: T): Unit = {
    val id = Thread.currentThread.getId.asInstanceOf[Int] % numOfThreads
    val snap = scan()
    val oldValue = a_table(id)
    val newValue = new StampedValue[T](oldValue.stamp + 1, v, snap)
    a_table(id) = newValue
  }

  private def collect[T](): Array[StampedValue[T]] = return a_table.clone()

  //wait-free now!
  override def scan(): Array[T] = {

    var oldCopy: Array[StampedValue[T]] = collect()
    var moved = new Array[Boolean](a_table.length)

    def scanAgain(): Array[T] = {
      val newCopy: Array[StampedValue[T]] = collect()
      for (j <- 0 until a_table.length){
        //if changed
        if (oldCopy(j).stamp != newCopy(j).stamp) {
          if (moved(j)) return newCopy(j).snap
          else {
            moved(j) = true
            oldCopy = newCopy;
            scanAgain()
          }
        }
      }
      var result = new Array[T](a_table.length)
      for (j <- 0 until a_table.length)
        result(j) = newCopy(j).value
      return result
    }

    scanAgain()

  }
}
