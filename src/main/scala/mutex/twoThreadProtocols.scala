package mutex

trait Lock {
  def lock(): Unit
  def unlock(): Unit
}

abstract class Counter {
  var value: Long
  val lock: Lock
  def getAndIncrement(): Long = {
    var temp: Long = 0
    lock.lock()
    try {
      temp = value
      value = value + 1


    } finally{
      lock.unlock()
    }
    return temp
  }
}

class LockOne extends Lock {
  //Deadlock may occur

  var flag = new Array[Boolean](2)

  def lock()= {
    val i = Thread.currentThread.getId % 2
    val j = 1 - i
    flag(i) = true
    while (flag(j)) wait()
  }

  def unlock() = {
    val i = Thread.currentThread.getId % 2
    flag(i) = false
  }
}

class LockTWO extends Lock {
  //The LockTwo class is inadequate because it deadlocks if one thread runs completely ahead of the other.
  // Nevertheless, LockTwo has an interesting property: if the threads run concurrently, the lock() method succeeds.
  var victim: Long = 0

  def lock()= {
    val i = Thread.currentThread.getId
    victim = i
    while (victim == i) wait()
  }

  def unlock() = {
  }
}

class Peterson extends Lock {
  //Starvation-free, Deadlock-free
  //combines LockOne and LockTwo

  var flag = new Array[Boolean](2)
  var victim: Int = 0
  def lock()= {
    val i = Thread.currentThread.getId.asInstanceOf[Int] % 2
    val j = 1 - i
    flag(i) = true
    victim = i
    while (flag(j) && victim == i) wait()
  }

  def unlock() = {
    val i = Thread.currentThread.getId % 2
    flag(i) = false
  }
}

