package coop.rchain.simulation.discovery

import collection.JavaConverters._

import coop.rchain.comm.PeerNode

import org.jgrapht.alg.clique.DegeneracyBronKerboschCliqueFinder
import org.jgrapht.alg.connectivity.KosarajuStrongConnectivityInspector
import org.jgrapht.graph._
import org.jgrapht.traverse.DepthFirstIterator

object GraphTransformer {
  def stronglyConnectedComponents(graph: Map[PeerNode, Seq[PeerNode]]): Seq[Seq[PeerNode]] = {
    val g = new SimpleDirectedGraph[PeerNode, DefaultEdge](classOf[DefaultEdge])
    graph.keys.foreach(g.addVertex)
    graph.foreach { case (node, peers) => peers.foreach(g.addEdge(node, _)) }
    val alg = new KosarajuStrongConnectivityInspector(g)
    alg.getStronglyConnectedComponents.asScala.map(c => new DepthFirstIterator(c).asScala.toSeq)
  }

  def maximalCliques(graph: Map[PeerNode, Seq[PeerNode]]): Seq[Seq[PeerNode]] = {
    val g = new SimpleGraph[PeerNode, DefaultEdge](classOf[DefaultEdge])
    graph.keys.foreach(g.addVertex)
    removeAdjacentEdges(graph).foreach { case (node, peers) => peers.foreach(g.addEdge(node, _)) }
    val alg = new DegeneracyBronKerboschCliqueFinder(g)
    alg.asScala.map(_.asScala.toSeq).toSeq
  }

  def removeAdjacentEdges(graph: Map[PeerNode, Seq[PeerNode]]): Map[PeerNode, Seq[PeerNode]] =
    graph.toStream
      .flatMap {
        case (node, peers) =>
          peers.toStream.map { p =>
            if (node.id.id < p.id.id) node -> p
            else p                         -> node
          }
      }
      .distinct
      .foldLeft(Map.empty[PeerNode, Seq[PeerNode]]) { (acc, p) =>
        val (p1, p2) = p
        val s        = p2 +: acc.getOrElse(p1, Seq.empty)
        acc + (p1 -> s)
      }
}
