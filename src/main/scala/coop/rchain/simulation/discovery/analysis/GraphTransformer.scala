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
    alg.maximumIterator.asScala.map(_.asScala.toSet).toSeq.sortWith(_.size > _.size)
  }

  def getAllPeers[Vertex](graph: Graph[Vertex]): Set[Vertex] =
    (graph.keys.toStream ++ graph.values.toStream.flatMap(_.toStream)).toSet

  def minimumCliqueCovering[Vertex](graph: Graph[Vertex]): Seq[Set[Vertex]] = {
    val hg     = IteratedGreedy.holderGraphFrom(graph)
    val alphaG = IteratedGreedy.independenceNumber(hg)
    IteratedGreedy
      .balanceCliques(hg, IteratedGreedy.minimumCliqueCovering(hg, alphaG))
      .map(_.map(_.vertex).toSet)
  }
}
