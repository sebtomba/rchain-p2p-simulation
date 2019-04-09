package coop.rchain.simulation.discovery

import scala.concurrent.duration._
import scala.util.Random

import akka.actor._

import cats._
import cats.data._
import cats.mtl.implicits._

import coop.rchain.comm.PeerNode
import coop.rchain.graphz.{GraphSerializer, StringSerializer}
import coop.rchain.simulation.discovery.analysis.{GraphTransformer, GraphzGenerator}
import coop.rchain.catscontrib.ski.kp

class KademliaNetwork extends Actor with Timers with ActorLogging {
  import KademliaNetwork._
  import KademliaNode._

  def receive: Receive = {
    case Spawn(nodes, stopAfter) =>
      val bootstrap = createNode()
      bootstrap ! Identify
      context.become(waiteForBootstrap(nodes, stopAfter))
  }

  def waiteForBootstrap(nodes: Int, stopAfter: FiniteDuration): Receive = {
    case Identification(bootstrap) =>
      (1 until Math.max(1, nodes)).foreach(kp(createNode(Some(bootstrap))))
      timers.startSingleTimer(StopNetwork, StopNetwork, stopAfter)
      context.become(running(bootstrap))
  }

  def running(bootstrap: PeerNode): Receive = {
    case StopNetwork =>
      log.info("Stopping the network...")
      val nodes = context.children.toSeq
      context.actorOf(ResultWorker.props) ! ResultWorker.CollectResults(nodes)
      context.become(waitForResults)
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def waitForResults: Receive = {
    case ResultWorker.Results(results) =>
      type Effect[A] = StateT[Id, StringBuffer, A]
      implicit val ser: GraphSerializer[Effect] = new StringSerializer[Effect]

      implicit val ord: Ordering[PeerNode] =
        (x: PeerNode, y: PeerNode) => x.id.toString.compare(y.id.toString)

      val comps = GraphTransformer.stronglyConnectedComponents(results)
      log.info(s"Found ${comps.size} strongly connected component(s)")

      val cliques         = GraphTransformer.maximalCliques(GraphTransformer.removeAdjacentEdges(results))
      val distinctCliques = GraphTransformer.selectCliques(cliques)
      log.info(s"Found ${distinctCliques.size} distinct maximal clique(s)")
      distinctCliques.zipWithIndex.foreach {
        case (ps, i) => log.info(s"clique $i: ${ps.map(_.id.toShortString).mkString(", ")}")
      }

      val builder = new StringBuffer()
      builder.append("Result:\n")
      GraphzGenerator.generate[Effect](results.toList).runS(builder)
      log.info(builder.toString)

      context.stop(self)
  }

  private def createNode(bootstrap: Option[PeerNode] = None): ActorRef = {
    val node = context.actorOf(KademliaNode.props(Random.nextInt(5).seconds, 3.second, 20))
    bootstrap.foreach(b => node ! Bootstrap(b))
    node
  }
}

object KademliaNetwork {
  final case class Spawn(nodes: Int, stopAfter: FiniteDuration)
  final object StopNetwork

  def props: Props = Props(new KademliaNetwork())
}
