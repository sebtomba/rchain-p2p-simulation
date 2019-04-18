package coop.rchain.simulation.discovery.analysis

import scala.collection.Searching._
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Random

object IteratedGreedy {

  object implicits {
    implicit def vertexHolderOrdering[V]: Ordering[VertexHolder[V]] =
      (x: VertexHolder[V], y: VertexHolder[V]) => x.id.compare(y.id)
  }

  class VertexHolder[V](val id: Int, val vertex: V) {
    override def equals(obj: Any): Boolean = obj.asInstanceOf[VertexHolder[V]].id == id
    override def hashCode(): Int           = id
  }
  object VertexHolder {
    def apply[V](id: Int, vertex: V): VertexHolder[V] = new VertexHolder[V](id, vertex)
  }

  final case class EdgeHolder[V](vertex: VertexHolder[V], peers: Array[VertexHolder[V]])

  def holderGraphFrom[V](graph: Map[V, Set[V]]): Array[EdgeHolder[V]] = {
    def isNeighbour(v1: V, v2: V): Boolean =
      graph.get(v1).exists(_.contains(v2)) && graph.get(v2).exists(_.contains(v1))

    val mapping = graph.keys.zipWithIndex.toMap

    graph.map {
      case (v, ps) =>
        EdgeHolder[V](
          VertexHolder[V](mapping(v), v),
          ps.filter(isNeighbour(v, _)).map(p => VertexHolder(mapping(p), p)).toArray.sortBy(_.id)
        )
    }.toArray
  }

  def allVertices[V](graph: Array[EdgeHolder[V]]): Array[VertexHolder[V]] = graph.map(_.vertex)

  def contains[V](peers: Array[V], v: V)(implicit ev: Ordering[V]): Boolean =
    peers.search(v) match {
      case InsertionPoint(_) => false
      case _                 => true
    }

  def jump[V: ClassTag](perm: Array[V]): Array[V] = {
    val j        = Random.nextInt(perm.length - 1) + 1
    val permNext = Array.ofDim[V](perm.length)
    permNext(0) = perm(j)
    Array.copy(perm, 0, permNext, 1, j)
    Array.copy(perm, j + 1, permNext, j + 1, perm.length - j - 1)
    permNext
  }

  def shuffle[V: ClassTag](perm: Array[V]): Array[V] =
    Random.shuffle(Vector(perm: _*)).toArray

  def numberOfEdges[V](graph: Array[EdgeHolder[V]]): Int = {
    for {
      e <- graph
      p <- e.peers
    } yield
      if (e.vertex.id < p.id) (e.vertex.id, p.id)
      else (p.id, e.vertex.id)
  }.distinct.length

  // see https://arxiv.org/abs/1709.02475
  def independenceNumberUpperBound(vertices: Int, edges: Int): Int =
    Math.floor(0.5d + Math.sqrt(0.25d + Math.pow(vertices, 2) - vertices - (2 * edges))).toInt

  def independenceNumber[V](graph: Array[EdgeHolder[V]], iterations: Int = 1000): Int = {
    implicit val ord: Ordering[VertexHolder[V]] = implicits.vertexHolderOrdering
    val upperBound                              = independenceNumberUpperBound(graph.length, numberOfEdges(graph))

    def greedy(
        vs: Array[VertexHolder[V]],
        i: Int = 0,
        acc: List[VertexHolder[V]] = Nil
    ): List[VertexHolder[V]] =
      if (i < vs.length) {
        val peers = graph(vs(i).id).peers
        greedy(vs, i + 1, if (acc.forall(v => !contains(peers, v))) vs(i) :: acc else acc)
      } else acc

    def iterate(
        perm: Array[VertexHolder[V]],
        perm0: Array[VertexHolder[V]],
        k0: Int = 1,
        i: Int = iterations
    ): Int =
      if (k0 >= upperBound || i <= 0) k0
      else {
        val k           = greedy(perm).length
        val (k1, perm1) = if (k >= k0) (k, perm) else (k0, perm0)
        iterate(jump(perm1), perm1, k1, i - 1)
      }

    val all = allVertices(graph)
    iterate(all, all)
  }

  def minimumCliqueCovering[V](
      graph: Array[EdgeHolder[V]],
      k: Int = 1,
      iterations: Int = 10000
  ): Seq[Seq[VertexHolder[V]]] = {
    implicit val ord: Ordering[VertexHolder[V]] = implicits.vertexHolderOrdering

    @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
    def greedy(
        vs: Array[VertexHolder[V]],
        i: Int = 0,
        acc: mutable.MutableList[mutable.MutableList[VertexHolder[V]]] = mutable.MutableList.empty
    ): Seq[Seq[VertexHolder[V]]] =
      if (i < vs.length) {
        val peers = graph(vs(i).id).peers
        val q = acc.find(_.forall(contains(peers, _))).getOrElse {
          val l = mutable.MutableList.empty[VertexHolder[V]]
          acc += l
          l
        }
        q += vs(i)
        greedy(vs, i + 1, acc)
      } else acc

    def iterate(
        perm: Array[VertexHolder[V]],
        acc: Seq[Seq[VertexHolder[V]]],
        best: Seq[Seq[VertexHolder[V]]],
        i: Int = iterations
    ): Seq[Seq[VertexHolder[V]]] =
      if (acc.length <= k || i <= 0) pickBest(acc, best)
      else iterate(shuffle(acc.toArray).flatten, greedy(perm), pickBest(acc, best), i - 1)

    def pickBest(
        a: Seq[Seq[VertexHolder[V]]],
        b: Seq[Seq[VertexHolder[V]]]
    ): Seq[Seq[VertexHolder[V]]] =
      if (a.size < b.size) a else b

    val all   = allVertices(graph)
    val start = greedy(all)
    iterate(all, start, start)
  }

  def balanceCliques[V](
      graph: Array[EdgeHolder[V]],
      cliques: Seq[Seq[VertexHolder[V]]]
  ): Seq[Seq[VertexHolder[V]]] = {
    implicit val ord: Ordering[VertexHolder[V]] = implicits.vertexHolderOrdering
    val target                                  = allVertices(graph).length / cliques.length

    def distribute(
        c: Seq[VertexHolder[V]],
        cs: Seq[Seq[VertexHolder[V]]]
    ): (Seq[VertexHolder[V]], Seq[Seq[VertexHolder[V]]]) =
      cs.foldLeft((c, Seq.empty[Seq[VertexHolder[V]]])) { (acc, cSrc) =>
        val (cDest, csSrcDone) = acc
        val count              = Math.min(cSrc.length - target, target - cDest.length)
        if (count > 0) {
          val move =
            cSrc
              .filter { v =>
                val peers = graph(v.id).peers
                cDest.forall(contains(peers, _))
              }
              .take(count)
          (cDest ++ move, cSrc.filter(v => !move.contains(v)) +: csSrcDone)
        } else (cDest, cSrc +: csSrcDone)
      }

    val (small, large) = cliques.partition(_.length < target)
    val result =
      small
        .sortBy(_.length)
        .foldLeft((Seq.empty[Seq[VertexHolder[V]]], large.sortWith(_.length > _.length))) {
          (acc, cDest) =>
            val (csDestDone, csSrc)    = acc
            val (cDestDone, csSrcNext) = distribute(cDest, csSrc)
            (cDestDone +: csDestDone, csSrcNext.sortWith(_.length > _.length))
        }

    result._2 ++ result._1
  }
}
