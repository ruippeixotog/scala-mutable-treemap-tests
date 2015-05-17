import java.io._

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import scala.collection.mutable
import scala.util.Try

object TreeMapProperties extends Properties("mutable.TreeMap") {
  type K = String
  type V = Int

  property("get, contains") = forAll { (allEntries: Map[K, V]) =>
    val entries = allEntries.take(allEntries.size / 2)

    val map = mutable.TreeMap[K, V]()
    map ++= entries

    allEntries.forall { case (k, v) =>
      map.contains(k) == entries.contains(k) &&
        map.get(k) == entries.get(k)
    }
  }

  property("size, isEmpty") = forAll { (entries: Map[K, V]) =>
    val map = mutable.TreeMap[K, V]()
    map ++= entries
    map.size == entries.size && map.isEmpty == entries.isEmpty
  }

  property("+=") = forAll { (map: mutable.TreeMap[K, V], k: K, v: V) =>
    val oldSize = map.size
    val containedKeyBefore = map.contains(k)
    val newExpectedSize = if(containedKeyBefore) oldSize else oldSize + 1

    map += (k -> v)
    map.contains(k) && map.get(k) == Some(v) && map.size == newExpectedSize
  }

  property("-=") = forAll { (map: mutable.TreeMap[K, V], k: K) =>
    val oldSize = map.size
    val containedKeyBefore = map.contains(k)
    val newExpectedSize = if(containedKeyBefore) oldSize - 1 else oldSize

    map -= k
    !map.contains(k) && map.get(k) == None && map.size == newExpectedSize
  }

  property("iterator") = forAll { (entries: Map[K, V]) =>
    val map = mutable.TreeMap[K, V]()
    map ++= entries

    map.iterator.toSeq == entries.toSeq.sorted
  }

  property("iteratorFrom") = forAll { (entries: Map[K, V], k: K) =>
    val map = mutable.TreeMap[K, V]()
    map ++= entries

    map.iteratorFrom(k).toSeq == entries.filterKeys(_ >= k).toSeq.sorted
  }

  property("keysIteratorFrom") = forAll { (entries: Map[K, V], k: K) =>
    val map = mutable.TreeMap[K, V]()
    map ++= entries

    map.keysIteratorFrom(k).toSeq == entries.keysIterator.filter(_ >= k).toSeq.sorted
  }

  property("valuesIteratorFrom") = forAll { (entries: Map[K, V], k: K) =>
    val map = mutable.TreeMap[K, V]()
    map ++= entries

    map.valuesIteratorFrom(k).toSeq == entries.filterKeys(_ >= k).toSeq.sorted.map(_._2)
  }

  property("headOption") = forAll { (map: mutable.TreeMap[K, V]) =>
    map.headOption == Try(map.iterator.next()).toOption
  }

  property("lastOption") = forAll { (map: mutable.TreeMap[K, V]) =>
    map.lastOption == Try(map.iterator.max).toOption
  }

  property("clear") = forAll { (map: mutable.TreeMap[K, V]) =>
    map.clear()
    map.isEmpty
  }

  property("serializable") = forAll { (map: mutable.TreeMap[K, V]) =>
    val bytesOut = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bytesOut)
    out.writeObject(map)
    val bytes = bytesOut.toByteArray

    val in = new ObjectInputStream(new ByteArrayInputStream(bytes))
    val sameMap = in.readObject().asInstanceOf[mutable.TreeMap[K, V]]
    map.iterator.toSeq == sameMap.iterator.toSeq
  }
}
