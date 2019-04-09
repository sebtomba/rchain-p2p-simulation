package coop.rchain.simulation.discovery

import scala.concurrent.duration._
import scala.util.Random

import akka.actor._
import akka.pattern.pipe

import cats.implicits._

import coop.rchain.catscontrib.ski.kp
import coop.rchain.comm.{NodeIdentifier, PeerNode}
import coop.rchain.comm.discovery._

import monix.eval.Task
import monix.execution.Scheduler

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class KademliaNode(
    initialDelay: FiniteDuration,
    discoveryInterval: FiniteDuration,
    addressSize: Int
) extends Actor
    with Timers
    with ActorLogging {
  import KademliaNode._

  private val local: PeerNode = {
    val key: Array[Byte] = Array.ofDim(addressSize)
    Random.nextBytes(key)
    PeerNode(NodeIdentifier(key), self)
  }
  private implicit val scheduler: Scheduler               = Scheduler(context.dispatcher)
  private implicit val kademliaRPC: KademliaRPC[Task]     = ActorKademliaRPC(local, context.system)
  private implicit val kademliaStore: KademliaStore[Task] = KademliaStore.table[Task](local.id)
  private implicit val nodeDiscovery: NodeDiscovery[Task] = NodeDiscovery.kademlia(local.id)

  override def preStart(): Unit =
    log.info(s"Starting node ${local.id.toShortString}")

  def receive: Receive = {
    case Identify => sender ! Identification(local)

    case Bootstrap(node) =>
      KademliaStore[Task].updateLastSeen(node).runToFuture.andThen {
        case _ => scheduleDiscovery(initialDelay)
      }

    case GetPeers =>
      (for {
        peers      <- KademliaStore[Task].peers
        fillFactor <- KademliaStore[Task].fillFactor
      } yield Peers(local, peers, fillFactor)).runToFuture.pipeTo(sender)

    case Ping(peer) =>
      KademliaHandleRPC.handlePing[Task](peer).as(Pong).runToFuture.pipeTo(sender)

    case Lookup(key, peer) =>
      KademliaHandleRPC.handleLookup[Task](peer, key).map(LookupResponse).runToFuture.pipeTo(sender)

    case Discover =>
      NodeDiscovery[Task].discover
        .doOnFinish(kp(Task.delay(scheduleDiscovery(discoveryInterval))))
        .runToFuture
  }

  private def scheduleDiscovery(delay: FiniteDuration): Unit =
    if (!timers.isTimerActive(Discover))
      timers.startSingleTimer(Discover, Discover, discoveryInterval)
}

object KademliaNode {
  case object Identify
  final case class Identification(node: PeerNode)
  final case class Bootstrap(peerNode: PeerNode)
  case object GetPeers
  final case class Peers(node: PeerNode, peers: Seq[PeerNode], fillFactor: Double)
  final case class Ping(sender: PeerNode)
  case object Pong
  final case class Lookup(key: Array[Byte], sender: PeerNode)
  final case class LookupResponse(nodes: Seq[PeerNode])
  case object Discover

  def props(
      initialDelay: FiniteDuration,
      discoveryInterval: FiniteDuration,
      addressSize: Int
  ): Props = Props(new KademliaNode(initialDelay, discoveryInterval, addressSize))
}
