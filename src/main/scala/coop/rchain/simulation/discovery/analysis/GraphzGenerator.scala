package coop.rchain.simulation.discovery.analysis

import cats.Monad
import cats.implicits._

import coop.rchain.graphz._

object GraphzGenerator {

  private val palette =
    Array(
      "#4FAB04",
      "#046BAB",
      "#AB0487",
      "#A3AB04",
      "#04AB98",
      "#7C04AB",
      "#AB6004",
      "#04AB44",
      "#2804AB",
      "#AB0C04",
      "#17AB04",
      "#0433AB",
      "#AB044F",
      "#6BAB04",
      "#0487AB",
      "#AB04A3",
      "#AB9804",
      "#04AB7C",
      "#6004AB",
      "#AB4404",
      "#04AB28",
      "#0C04AB",
      "#AB0417",
      "#33AB04",
      "#044FAB",
      "#AB046B",
      "#87AB04",
      "#04A3AB",
      "#9804AB",
      "#AB7C04",
      "#04AB60",
      "#4404AB",
      "#AB2804",
      "#04AB0C",
      "#0417AB",
      "#AB0433"
    )

  private def cliqueColor(clique: Clique): String =
    palette((clique.level - 1) % palette.length)

  private def cliqueHeight(clique: Clique): Float =
    0.1f + 0.02f * clique.level

  private def initGraph[G[_]: Monad: GraphSerializer](name: String): G[Graphz[G]] =
    Graphz[G](
      name,
      Graph,
      bgcolor = Some("#ECECEC"),
      outputorder = Some(EdgesFirst),
      margin = Some("0"),
      size = Some("20!"),
      node = Map(
        "color"     -> "\"#ECECEC\"",
        "fillcolor" -> "\"#AB0433\"",
        "shape"     -> "point",
        "height"    -> "0.1",
        "penwidth"  -> "0.3"
      ),
      edge = Map(
        "penwidth"  -> "0.3",
        "color"     -> "\"#818181\"",
        "fontname"  -> "\"Arial\"",
        "fontsize"  -> "8",
        "fontcolor" -> "\"#AB0433\""
      )
    )

  def generate[G[_]: Monad: GraphSerializer](
      graph: NetworkTransformer.NetworkGraph
  ): G[Graphz[G]] =
    for {
      g <- initGraph[G]("kademlia")
      _ <- graph.toList.traverse {
            case (node, peers) => generateNetworkHierarchy(g, 1, node, peers)
          }
      _ <- g.close
    } yield g

  private def generateNetworkHierarchy[G[_]: Monad: GraphSerializer](
      g: Graphz[G],
      level: Int,
      node: NetworkElement,
      peers: Set[NetworkElement]
  ): G[Unit] = node match {
    case NetworkElement(id, Leaf) =>
      g.edges(id.toShortString, peers.map(_.id.toShortString))
    case NetworkElement(id, clique: Clique) =>
      g.subgraph {
        for {
          sg <- Graphz.subgraph[G](s"clique_${id.toShortString}", Graph, level)
          _ <- sg.node(
                id.toShortString,
                height = Some(cliqueHeight(clique)),
                fillcolor = Some(cliqueColor(clique))
              )
          _ <- sg.edges(id.toShortString, peers.map(_.id.toShortString))
          _ <- clique.members.toList
                .traverse(generateNetworkHierarchy(sg, level + 1, _, Set(node)))
          _ <- sg.close
        } yield sg
      }
  }

}
