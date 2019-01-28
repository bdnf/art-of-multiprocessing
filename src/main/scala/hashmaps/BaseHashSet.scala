package hashmaps

import java.util

abstract class BaseHashSet[T](capacity: Int) {
  var table = new Array[util.ArrayList[T]](capacity)
  val setSize: Int
  for (i <- 0 until capacity) table(i) = new util.ArrayList[T]()
}
