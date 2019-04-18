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

  def selectCliques(graph: NetworkGraph): Seq[Set[NetworkElement]] =
    GraphTransformer
      .minimumCliqueCovering(graph)
      .filter(_.size > 1)
      .sortWith(_.size > _.size)

  def reduceToNetworkOfCliques(
      graph: NetworkGraph,
      cliqueId: () => NodeIdentifier
  ): NetworkGraph = {
    def loop(g: NetworkGraph, cliques: Seq[Set[NetworkElement]]): NetworkGraph =
      cliques match {
        case Nil => g
        case cs =>
          val next = replaceByCliques(g, cs, cliqueId)
          loop(next, selectCliques(next))
      }

    loop(graph, selectCliques(graph))
  }

  def reduceToCliques(
      graph: NetworkGraph,
      cliqueId: () => NodeIdentifier
  ): NetworkGraph =
    selectCliques(graph) match {
      case Nil => graph
      case cs  => replaceByCliques(graph, cs, cliqueId)
    }
}
