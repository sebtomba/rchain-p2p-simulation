package coop.rchain.simulation.discovery

import cats.Monad
import cats.implicits._

import coop.rchain.comm.PeerNode
import coop.rchain.graphz._

object GraphzGenerator {
  private def initGraph[G[_]: Monad: GraphSerializer](name: String): G[Graphz[G]] =
    Graphz[G](
      name,
      Graph,
      bgcolor = Some("#ECECEC"),
      outputorder = Some(EdgesFirst),
      margin = Some("0"),
      size = Some("20!"),
      node = Map("color"    -> "\"#AB0433\"", "shape" -> "point", "height" -> "0.01"),
      edge = Map("penwidth" -> "0.03", "color"        -> "\"#818181\"")
    )

  def generate[G[_]: Monad: GraphSerializer](
      graph: List[(PeerNode, Seq[PeerNode])]
  ): G[Graphz[G]] =
    for {
      g <- initGraph[G]("kademlia")
      _ <- graph.traverse {
            case (node, peers) => g.edges(node.id.toShortString, peers.map(_.id.toShortString))
          }
      _ <- g.close
    } yield g
}
