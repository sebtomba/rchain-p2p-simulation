package coop.rchain.simulation.discovery

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.Try

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class ActorTerminator() extends Actor {
  import ActorTerminator._

  def receive: Receive = {
    case Watch(actor) =>
      context.watch(actor)
      context.become(wait(sender()))
  }

  def wait(callback: ActorRef): Receive = {
    case Terminated(_) =>
      callback ! Done
      context.stop(self)
  }
}

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object ActorTerminator {
  final case class Watch(actor: ActorRef)
  case object Done

  def awaitTermination(
      actor: ActorRef
  )(implicit actorSystem: ActorSystem, timeout: Timeout): Unit = {
    val t = actorSystem.actorOf(Props(new ActorTerminator()))
    Try(Await.ready(t ? Watch(actor), Duration.Inf))
  }
}
