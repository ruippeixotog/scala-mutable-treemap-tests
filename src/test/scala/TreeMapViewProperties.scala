import java.io._

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.util.Try

object TreeMapViewProperties extends Properties("mutable.TreeMapView") {
  type K = String
  type V = Int

  implicit val ord = implicitly[Ordering[K]]

  def in(key: K, from: Option[K], until: Option[K]) =
    from.fold(true)(_ <= key) && until.fold(true)(_ > key)

  def entriesInView[This <: TraversableOnce[(K, V)], That](entries: This, from: Option[K], until: Option[K])(implicit bf: CanBuildFrom[This, (K, V), That]) = {
    (bf.apply(entries) ++= entries.filter { case (k, _) => in(k, from, until) }).result()
  }

  property("get, contains") = forAll { (allEntries: Map[K, V], from: Option[K], until: Option[K]) =>
    val entries = allEntries.take(allEntries.size / 2)

    val map = mutable.TreeMap[K, V]()
    map ++= entries

    val mapView = map.rangeImpl(from, until)
    allEntries.forall { case (k, v) =>
      mapView.contains(k) == (in(k, from, until) && entries.contains(k)) &&
        mapView.get(k) == (if(in(k, from, until)) entries.get(k) else None)
    }
  }

  property("size, isEmpty") = forAll { (entries: Map[K, V], from: Option[K], until: Option[K]) =>
    val map = mutable.TreeMap[K, V]()
    map ++= entries

    val mapView = map.rangeImpl(from, until)
    mapView.size == entriesInView(entries, from, until).size &&
      mapView.isEmpty == !entries.exists { kv => in(kv._1, from, until) }
  }

  property("+=") = forAll { (map: mutable.TreeMap[K, V], k: K, v: V, from: Option[K], until: Option[K]) =>
    val oldSize = map.size
    val containedKeyBefore = map.contains(k)
    val newExpectedSize = if(containedKeyBefore) oldSize else oldSize + 1
    val isInRange = in(k, from, until)

    val mapView = map.rangeImpl(from, until)
    mapView += (k -> v)

    map.contains(k) && map.get(k) == Some(v) && map.size == newExpectedSize &&
      mapView.contains(k) == isInRange &&
      mapView.get(k) == (if(isInRange) Some(v) else None)
  }

  property("-=") = forAll { (map: mutable.TreeMap[K, V], k: K, from: Option[K], until: Option[K]) =>
    val oldSize = map.size
    val containedKeyBefore = map.contains(k)
    val newExpectedSize = if(containedKeyBefore) oldSize - 1 else oldSize

    val mapView = map.rangeImpl(from, until)
    mapView -= k

    !map.contains(k) && map.get(k) == None && map.size == newExpectedSize &&
      !mapView.contains(k) &&
      mapView.get(k) == None
  }

  property("iterator") = forAll { (entries: Map[K, V], from: Option[K], until: Option[K]) =>
    val map = mutable.TreeMap[K, V]()
    map ++= entries

    val mapView = map.rangeImpl(from, until)
    mapView.iterator.toSeq == entriesInView(entries, from, until).toSeq.sorted
  }

  property("iteratorFrom") = forAll { (entries: Map[K, V], k: K, from: Option[K], until: Option[K]) =>
    val map = mutable.TreeMap[K, V]()
    map ++= entries

    val mapView = map.rangeImpl(from, until)
    val newLower = Some(from.fold(k)(ord.max(_, k)))
    mapView.iteratorFrom(k).toSeq == entriesInView(entries, newLower, until).toSeq.sorted
  }

  property("keysIteratorFrom") = forAll { (entries: Map[K, V], k: K, from: Option[K], until: Option[K]) =>
    val map = mutable.TreeMap[K, V]()
    map ++= entries

    val mapView = map.rangeImpl(from, until)
    val newLower = Some(from.fold(k)(ord.max(_, k)))
    mapView.keysIteratorFrom(k).toSeq == entriesInView(entries, newLower, until).toSeq.sorted.map(_._1)
  }

  property("valuesIteratorFrom") = forAll { (entries: Map[K, V], k: K, from: Option[K], until: Option[K]) =>
    val map = mutable.TreeMap[K, V]()
    map ++= entries

    val mapView = map.rangeImpl(from, until)
    val newLower = Some(from.fold(k)(ord.max(_, k)))
    mapView.valuesIteratorFrom(k).toSeq == entriesInView(entries, newLower, until).toSeq.sorted.map(_._2)
  }

  property("headOption") = forAll { (map: mutable.TreeMap[K, V], from: Option[K], until: Option[K]) =>
    val mapView = map.rangeImpl(from, until)
    mapView.headOption == Try(entriesInView(map.iterator, from, until).next()).toOption
  }

  property("lastOption") = forAll { (map: mutable.TreeMap[K, V], from: Option[K], until: Option[K]) =>
    val mapView = map.rangeImpl(from, until)
    mapView.lastOption == Try(entriesInView(map.iterator, from, until).max).toOption
  }

  property("clear") = forAll { (map: mutable.TreeMap[K, V], from: Option[K], until: Option[K]) =>
    val mapView = map.rangeImpl(from, until)
    mapView.clear()
    map.isEmpty && mapView.isEmpty
  }

  property("serializable") = forAll { (map: mutable.TreeMap[K, V], from: Option[K], until: Option[K]) =>
    val mapView = map.rangeImpl(from, until)

    val bytesOut = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bytesOut)
    out.writeObject(mapView)
    val bytes = bytesOut.toByteArray

    val in = new ObjectInputStream(new ByteArrayInputStream(bytes))
    val sameMapView = in.readObject().asInstanceOf[mutable.TreeMap[K, V]]
    mapView.iterator.toSeq == sameMapView.iterator.toSeq
  }
}
