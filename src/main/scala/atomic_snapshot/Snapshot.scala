package atomic_snapshot

trait Snapshot[T] {
  //constructs an instantaneous view of an array of atomic registers.
  def update[T](v: T): Unit
  def scan(): Array[T]

}