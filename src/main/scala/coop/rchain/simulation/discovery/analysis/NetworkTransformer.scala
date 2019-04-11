package coop.rchain.simulation.discovery.analysis

import coop.rchain.comm.{NodeIdentifier, PeerNode}

object NetworkTransformer {
  type NetworkGraph = Map[NetworkElement, Set[NetworkElement]]

  implicit val ordering: Ordering[NetworkElement] =
    (x: NetworkElement, y: NetworkElement) => x.id.toString.compare(y.id.toString)

  def networkFromPeerNodes(
      graph: Map[PeerNode, Set[PeerNode]]
  ): NetworkGraph =
    graph.map {
      case (node, peers) =>
        NetworkElement(node.id, Leaf) -> peers.map(p => NetworkElement(p.id, Leaf))
    }

  def replaceByClique(
      graph: NetworkGraph,
      clique: Set[NetworkElement],
      cliqueId: () => NodeIdentifier
  ): NetworkGraph = {
    val c   = NetworkElement(cliqueId(), Clique(clique))
    val cps = clique.flatMap(graph.getOrElse(_, Nil)) -- clique
    graph.toStream
      .filter(e => !clique.contains(e._1))
      .map {
        case (e, peers) =>
          val (ps1, ps2) = peers.partition(clique.contains)
          val ps         = if (ps1.isEmpty) ps2 else ps2 + c
          e -> ps
      }
      .toMap + (c -> cps)
  }

  def replaceByCliques(
      graph: NetworkGraph,
      cliques: Seq[Set[NetworkElement]],
      cliqueId: () => NodeIdentifier
  ): NetworkGraph = cliques.foldLeft(graph)((g, c) => replaceByClique(g, c, cliqueId))

  def selectClique(graph: NetworkGraph): Option[Set[NetworkElement]] =
    GraphTransformer
      .maximalCliques(GraphTransformer.reduceToUndirectedConnectedGraph(graph))
      .sortWith(_.size > _.size)
      .headOption

  def reduceToNetworkOfCliques(
      graph: NetworkGraph,
      cliqueId: () => NodeIdentifier
  ): NetworkGraph = {
    def loop(g: NetworkGraph, clique: Option[Set[NetworkElement]]): NetworkGraph =
      clique match {
        case None => g
        case Some(c) =>
          val next = replaceByClique(g, c, cliqueId)
          loop(next, selectClique(next))
      }

    loop(graph, selectClique(graph))
  }
}
