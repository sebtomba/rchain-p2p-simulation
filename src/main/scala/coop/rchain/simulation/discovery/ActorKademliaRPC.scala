package coop.rchain.simulation.discovery

import scala.concurrent.duration._

import akka.actor._

import coop.rchain.comm.discovery.KademliaRPC
import coop.rchain.comm.PeerNode

import monix.eval.Task
import monix.execution.Callback

class ActorKademliaRPC(local: PeerNode, system: ActorSystem) extends KademliaRPC[Task] {
  import ActorKademliaRPC._
  import KademliaNode._

  def ping(peer: PeerNode): Task[Boolean] =
    Task.create { (_, cb) =>
      val worker = system.actorOf(PingWorker.props(cb))
      worker ! PingWorker.Send(peer, Ping(local))
    }

  def lookup(key: Seq[Byte], peer: PeerNode): Task[Seq[PeerNode]] =
    Task.create { (_, cb) =>
      val worker = system.actorOf(LookupWorker.props(cb))
      worker ! LookupWorker.Send(peer, Lookup(key.toArray, local))
    }
}

object ActorKademliaRPC {

  def apply(local: PeerNode, system: ActorSystem): KademliaRPC[Task] =
    new ActorKademliaRPC(local, system)

  private object Timeout

  private class PingWorker(callback: Callback[Throwable, Boolean]) extends Actor with Timers {
    import KademliaNode._
    import PingWorker._

    def receive: Receive = {
      case Send(to, ping) =>
        to.endpoint ! ping
        timers.startSingleTimer(Timeout, Timeout, 3.seconds)
        context.become(awaitResponse)
    }

    def awaitResponse: Receive = {
      case Pong =>
        timers.cancel(Timeout)
        callback.onSuccess(true)
        context.stop(self)

      case Timeout =>
        callback.onSuccess(false)
        context.stop(self)
    }
  }

  private object PingWorker {
    final case class Send(to: PeerNode, ping: KademliaNode.Ping)

    def props(callback: Callback[Throwable, Boolean]): Props =
      Props(new PingWorker(callback))
  }

  private class LookupWorker(callback: Callback[Throwable, Seq[PeerNode]])
      extends Actor
      with Timers {
    import KademliaNode._
    import LookupWorker._

    def receive: Receive = {
      case Send(to, lookup) =>
        to.endpoint ! lookup
        timers.startSingleTimer(Timeout, Timeout, 3.seconds)
        context.become(awaitResponse)
    }

    def awaitResponse: Receive = {
      case LookupResponse(peers) =>
        timers.cancel(Timeout)
        callback.onSuccess(peers)
        context.stop(self)

      case Timeout =>
        callback.onSuccess(Nil)
        context.stop(self)
    }
  }

  private object LookupWorker {
    final case class Send(to: PeerNode, lookup: KademliaNode.Lookup)

    def props(callback: Callback[Throwable, Seq[PeerNode]]): Props =
      Props(new LookupWorker(callback))
  }
}
