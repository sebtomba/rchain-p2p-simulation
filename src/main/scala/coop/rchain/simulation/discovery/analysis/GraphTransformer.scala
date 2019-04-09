package coop.rchain.simulation.discovery.analysis

import scala.collection.JavaConverters._

import coop.rchain.comm.PeerNode

import org.jgrapht.alg.clique.DegeneracyBronKerboschCliqueFinder
import org.jgrapht.alg.connectivity.KosarajuStrongConnectivityInspector
import org.jgrapht.graph._
import org.jgrapht.traverse.DepthFirstIterator

object GraphTransformer {
  def stronglyConnectedComponents(graph: Map[PeerNode, Seq[PeerNode]]): Seq[Set[PeerNode]] = {
    val g = new SimpleDirectedGraph[PeerNode, DefaultEdge](classOf[DefaultEdge])
    graph.keys.foreach(g.addVertex)
    graph.foreach { case (node, peers) => peers.foreach(g.addEdge(node, _)) }
    val alg = new KosarajuStrongConnectivityInspector(g)
    alg.getStronglyConnectedComponents.asScala.map(c => new DepthFirstIterator(c).asScala.toSet)
  }

  def maximalCliques(graph: Map[PeerNode, Seq[PeerNode]]): Seq[Set[PeerNode]] = {
    val g = new SimpleGraph[PeerNode, DefaultEdge](classOf[DefaultEdge])
    getAllPeers(graph).foreach(g.addVertex)
    graph.foreach { case (node, peers) => peers.foreach(g.addEdge(node, _)) }
    val alg = new DegeneracyBronKerboschCliqueFinder(g)
    alg.asScala.map(_.asScala.toSet).toSeq.sortWith(_.size > _.size)
  }

  def selectCliques(cliques: Seq[Set[PeerNode]]): Seq[Set[PeerNode]] = {
    def loop(c: Seq[Set[PeerNode]], result: Seq[Set[PeerNode]] = Nil): Seq[Set[PeerNode]] =
      c match {
        case Nil => result
        case h +: t =>
          loop(t.map(_ -- h).filter(_.size > 2).sortWith(_.size > _.size), h +: result)
      }

    loop(cliques).reverse
  }

  def getAllPeers(graph: Map[PeerNode, Seq[PeerNode]]): Set[PeerNode] =
    (graph.keys.toStream ++ graph.values.toStream.flatMap(_.toStream)).toSet

  def removeAdjacentEdges(graph: Map[PeerNode, Seq[PeerNode]]): Map[PeerNode, Seq[PeerNode]] =
    graph.toStream
      .flatMap {
        case (node, peers) =>
          peers.toStream.map { p =>
            if (node.id.toString < p.id.toString) node -> p
            else p                                     -> node
          }
      }
      .distinct
      .foldLeft(Map.empty[PeerNode, Seq[PeerNode]]) { (acc, p) =>
        val (p1, p2) = p
        val s        = p2 +: acc.getOrElse(p1, Seq.empty)
        acc + (p1 -> s)
      }
}
