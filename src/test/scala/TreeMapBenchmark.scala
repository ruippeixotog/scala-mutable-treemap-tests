import scala.collection.{immutable, mutable}
import scala.util.Random

object TreeMapBenchmark extends App {

  def benchmark[T](msg: String)(block: => T) = {
    println(s"--- $msg ---")
    val t = System.currentTimeMillis()
    block
    println(s"Took ${System.currentTimeMillis() - t} ms")
  }

  val insertOps = 1000000
  val searchOps = 1000000
  val deleteOps = 1000000

  // some of the keys are repeated on purpose, to test inserts of existing keys and deletions of non-existing keys
  val arr = Array.fill(insertOps)(Random.nextInt() -> Random.nextInt())

  var imap = immutable.TreeMap[Int, Int]()
  val mmap = mutable.TreeMap[Int, Int]()
  val jmap = new java.util.TreeMap[Int, Int]()

  println("Total unique elements to insert/get/delete: " + arr.toSet.size)

  println()
  println("######")
  println("Insertion")
  println("######")

  benchmark("scala.collection.immutable.TreeMap") {
    arr.foreach { kv => imap += kv }
  }

  benchmark("scala.collection.mutable.TreeMap") {
    arr.foreach { kv => mmap += kv }
  }

  benchmark("java.util.TreeMap") {
    arr.foreach { kv => jmap.put(kv._1, kv._2) }
  }

  println()
  println("######")
  println("Search")
  println("######")

  val indicesToSearch = List.fill(insertOps) {
    if(Random.nextBoolean()) arr(Random.nextInt(insertOps))._1 // existing value
    else Random.nextInt() // random value
  }

  benchmark("scala.collection.immutable.TreeMap") {
    indicesToSearch.foreach { k => imap.contains(k) }
  }

  benchmark("scala.collection.mutable.TreeMap") {
    indicesToSearch.foreach { k => mmap.contains(k) }
  }

  benchmark("java.util.TreeMap") {
    indicesToSearch.foreach { k => jmap.containsKey(k) }
  }

  println()
  println("######")
  println("Deletion")
  println("######")

  val indicesToDelete = Random.shuffle(arr.toList).take(deleteOps)

  benchmark("scala.collection.immutable.TreeMap") {
    indicesToDelete.foreach { kv => imap -= kv._1 }
  }

  benchmark("scala.collection.mutable.TreeMap") {
    indicesToDelete.foreach { kv => mmap -= kv._1 }
  }

  benchmark("java.util.TreeMap") {
    indicesToDelete.foreach { kv => jmap.remove(kv._1) }
  }
}
