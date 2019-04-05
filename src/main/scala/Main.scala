import scala.concurrent.duration._
import scala.io.StdIn

import akka.actor.ActorSystem

import coop.rchain.simulation.discovery.KademliaNetwork
import coop.rchain.simulation.discovery.KademliaNetwork._

object Main {
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("kademlia")

    try {
      val network = system.actorOf(KademliaNetwork.props)
      network ! Spawn(100, 20.seconds)
      StdIn.readLine()
    } finally {
      system.terminate()
    }
  }
}
