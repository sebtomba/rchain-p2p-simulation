package coop.rchain.simulation.discovery.analysis

import scala.collection.JavaConverters._

import org.jgrapht.alg.clique.DegeneracyBronKerboschCliqueFinder
import org.jgrapht.alg.connectivity.KosarajuStrongConnectivityInspector
import org.jgrapht.graph._
import org.jgrapht.traverse.DepthFirstIterator

object GraphTransformer {
  type Graph[Vertex] = Map[Vertex, Set[Vertex]]

  def stronglyConnectedComponents[Vertex](graph: Graph[Vertex]): Seq[Set[Vertex]] = {
    val g = new SimpleDirectedGraph[Vertex, DefaultEdge](classOf[DefaultEdge])
    getAllPeers(graph).foreach(g.addVertex)
    graph.foreach { case (node, peers) => peers.foreach(g.addEdge(node, _)) }
    val alg = new KosarajuStrongConnectivityInspector(g)
    alg.getStronglyConnectedComponents.asScala.map(c => new DepthFirstIterator(c).asScala.toSet)
  }

  def maximalCliques[Vertex](graph: Graph[Vertex]): Seq[Set[Vertex]] = {
    val g = new SimpleGraph[Vertex, DefaultEdge](classOf[DefaultEdge])
    getAllPeers(graph).foreach(g.addVertex)
    graph.foreach { case (node, peers) => peers.foreach(g.addEdge(node, _)) }
    val alg = new DegeneracyBronKerboschCliqueFinder(g)
    alg.asScala.map(_.asScala.toSet).toSeq.sortWith(_.size > _.size)
  }

  def selectCliques[Vertex](cliques: Seq[Set[Vertex]]): Seq[Set[Vertex]] = {
    def loop(c: Seq[Set[Vertex]], result: Seq[Set[Vertex]] = Nil): Seq[Set[Vertex]] =
      c match {
        case Nil => result
        case h +: t =>
          loop(t.map(_ -- h).filter(_.size > 2).sortWith(_.size > _.size), h +: result)
      }

    loop(cliques).reverse
  }

  def getAllPeers[Vertex](graph: Graph[Vertex]): Set[Vertex] =
    (graph.keys.toStream ++ graph.values.toStream.flatMap(_.toStream)).toSet

  def removeAdjacentEdges[Vertex: Ordering](graph: Graph[Vertex]): Graph[Vertex] =
    graph.toStream
      .flatMap {
        case (node, peers) =>
          peers.toStream.map { p =>
            if (Ordering[Vertex].lt(node, p)) node -> p
            else p                                 -> node
          }
      }
      .distinct
      .foldLeft(Map.empty[Vertex, Set[Vertex]]) { (acc, p) =>
        val (p1, p2) = p
        val s        = acc.getOrElse(p1, Set.empty) + p2
        acc + (p1 -> s)
      }

  def removeNodes[Vertex](
      graph: Graph[Vertex],
      nodes: Set[Vertex]
  ): Graph[Vertex] =
    graph.toStream
      .filter(e => !nodes.contains(e._1))
      .map { case (node, peers) => node -> (peers -- nodes) }
      .filter(_._2.nonEmpty)
      .toMap

}
