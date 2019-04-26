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
      level: Int,
      graph: NetworkGraph,
      clique: Set[NetworkElement],
      cliqueId: () => NodeIdentifier
  ): NetworkGraph = {
    val c   = NetworkElement(cliqueId(), Clique(level, clique))
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
      level: Int,
      graph: NetworkGraph,
      cliques: Seq[Set[NetworkElement]],
      cliqueId: () => NodeIdentifier
  ): NetworkGraph = cliques.foldLeft(graph)((g, c) => replaceByClique(level, g, c, cliqueId))

  def selectCliques(graph: NetworkGraph): Seq[Set[NetworkElement]] =
    GraphTransformer
      .minimumCliqueCovering(graph)
      .filter(_.size > 1)
      .sortWith(_.size > _.size)

  def selectCliquesByCliqueSize(graph: NetworkGraph): Seq[Set[NetworkElement]] = {
    val hg = IteratedGreedy.holderGraphFrom(graph)
    val nodes = IteratedGreedy.allVertices(hg).sortWith { (v1, v2) =>
      (v1.vertex.networkHierarchy, v2.vertex.networkHierarchy) match {
        case (Clique(_, m1), Clique(_, m2)) => m1.size < m2.size
        case (Clique(_, _), Leaf)           => false
        case (Leaf, Clique(_, _))           => true
        case _                              => hg(v1.id).peers.length < hg(v2.id).peers.length
      }
    }
    IteratedGreedy
      .balanceCliques(hg, IteratedGreedy.greedyCliqueCovering(hg, nodes))
      .map(_.map(_.vertex).toSet)
      .filter(_.size > 1)
      .sortWith(_.size > _.size)
  }

  def reduceToNetworkOfCliques(
      graph: NetworkGraph,
      cliqueId: () => NodeIdentifier
  ): NetworkGraph = {
    def loop(level: Int, g: NetworkGraph, cliques: Seq[Set[NetworkElement]]): NetworkGraph =
      cliques match {
        case Nil => g
        case cs =>
          val next = replaceByCliques(level, g, cs, cliqueId)
          loop(level + 1, next, selectCliquesByCliqueSize(next))
      }

    loop(1, graph, selectCliques(graph))
  }

  def reduceToCliques(
      graph: NetworkGraph,
      cliqueId: () => NodeIdentifier
  ): NetworkGraph = {
    def loop(level: Int, g: NetworkGraph, cliques: Seq[Set[NetworkElement]]): NetworkGraph =
      cliques match {
        case Nil => g
        case cs  => replaceByCliques(level, g, cs, cliqueId)
      }

    loop(1, graph, selectCliques(graph))
  }

}
