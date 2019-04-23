package coop.rchain.simulation.discovery.analysis

import coop.rchain.comm.NodeIdentifier

sealed trait NetworkHierarchy
case object Leaf                                                  extends NetworkHierarchy
final case class Clique(level: Int, members: Set[NetworkElement]) extends NetworkHierarchy

final case class NetworkElement(
    id: NodeIdentifier,
    networkHierarchy: NetworkHierarchy
)
