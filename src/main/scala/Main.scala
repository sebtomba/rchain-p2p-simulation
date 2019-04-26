import scala.concurrent.duration._

import akka.actor.ActorSystem
import akka.util.Timeout

import coop.rchain.simulation.discovery.{ActorTerminator, KademliaNetwork}
import coop.rchain.simulation.discovery.KademliaNetwork._

object Main {
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def main(args: Array[String]): Unit = {
    implicit val system: ActorSystem = ActorSystem("kademlia")
    implicit val timeout: Timeout    = Timeout(1.hour)

    try {
      val network = system.actorOf(KademliaNetwork.props)
      network ! Spawn(5000, 120.seconds)
      ActorTerminator.awaitTermination(network)
    } finally {
      system.terminate()
    }
  }
}
