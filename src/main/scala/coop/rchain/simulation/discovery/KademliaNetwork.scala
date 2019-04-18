package coop.rchain.simulation.discovery

import scala.concurrent.duration._
import scala.util.Random

import akka.actor._

import cats._
import cats.data._
import cats.mtl.implicits._

import coop.rchain.catscontrib.ski.kp
import coop.rchain.comm.{NodeIdentifier, PeerNode}
import coop.rchain.graphz.{GraphSerializer, StringSerializer}
import coop.rchain.simulation.discovery.analysis._

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
      context.children.foreach(context.stop)

//      implicit val ord: Ordering[PeerNode] =
//        (x: PeerNode, y: PeerNode) => x.id.toString.compare(y.id.toString)
//
//      val comps = GraphTransformer.stronglyConnectedComponents(results)
//      log.info(s"Found ${comps.size} strongly connected component(s)")

//      val hg = IteratedGreedy.holderGraphFrom(results)
//      val vs = IteratedGreedy.allVertices(hg).length
//      log.info(s"number of vertives = $vs")
//      val es = IteratedGreedy.numberOfEdges(hg)
//      log.info(s"number of edges = $es")
//
//      val ub = IteratedGreedy.independenceNumberUpperBound(vs, es)
//      log.info(s"alpha(G) upper bound p = $ub")
//      val startAlphaG = System.nanoTime()
//      val alphaG      = IteratedGreedy.independenceNumber(hg)
//      val endAlphaG   = Duration.fromNanos(System.nanoTime() - startAlphaG)
//      log.info(s"Independence Number calculation time: ${endAlphaG.toMillis}ms")
//      log.info(s"alpha(G) = $alphaG")
//
//      val startC = System.nanoTime()
//      val cs     = IteratedGreedy.minimumCliqueCovering(hg, alphaG)
//      val endC   = Duration.fromNanos(System.nanoTime() - startC)
//      log.info(s"Minimum clique covering calculation time: ${endC.toMillis}ms")
//      log.info(s"Found ${cs.size} minimum covering clique(s)")
//      cs.zipWithIndex.foreach {
//        case (ps, i) => log.info(s"clique $i: ${ps.map(_.vertex.id.toShortString).mkString(", ")}")
//      }
//
//      val startBal = System.nanoTime()
//      val csBal    = IteratedGreedy.balanceCliques(hg, cs)
//      val endBal   = Duration.fromNanos(System.nanoTime() - startBal)
//      log.info(s"Clique balancing calculation time: ${endBal.toMillis}ms")
//      log.info(s"Balanced ${csBal.size} minimum covering clique(s)")
//      csBal.zipWithIndex.foreach {
//        case (ps, i) => log.info(s"clique $i: ${ps.map(_.vertex.id.toShortString).mkString(", ")}")
//      }

      type Effect[A] = StateT[Id, StringBuffer, A]
      implicit val ser: GraphSerializer[Effect] = new StringSerializer[Effect]
      val graph                                 = NetworkTransformer.networkFromPeerNodes(results)
      val builder                               = new StringBuffer()
      builder.append("Result:\n")
      GraphzGenerator
        .generate[Effect](
          NetworkTransformer.reduceToNetworkOfCliques(graph, () => nodeIdentifier())
        )
        .runS(builder)
      log.info(builder.toString)

      context.stop(self)
  }

  private def nodeIdentifier(): NodeIdentifier = NodeIdentifier.random(20)

  private def createNode(bootstrap: Option[PeerNode] = None): ActorRef = {
    val node = context.actorOf(
      KademliaNode.props(nodeIdentifier(), Random.nextInt(5).seconds, 3.second)
    )
    bootstrap.foreach(b => node ! Bootstrap(b))
    node
  }
}

object KademliaNetwork {
  final case class Spawn(nodes: Int, stopAfter: FiniteDuration)
  final object StopNetwork

  def props: Props = Props(new KademliaNetwork())
}
