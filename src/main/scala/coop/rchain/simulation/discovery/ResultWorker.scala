package coop.rchain.simulation.discovery

import akka.actor._

import coop.rchain.comm.PeerNode

class ResultWorker() extends Actor {
  import ResultWorker._

  def receive: Receive = {
    case CollectResults(nodes) =>
      nodes.foreach(_ ! KademliaNode.GetPeers)
      context.become(waitForResults(nodes.size, Map.empty))
  }

  def waitForResults(count: Int, results: Map[PeerNode, Set[PeerNode]]): Receive = {
    case KademliaNode.Peers(node, peers, _) =>
      if (!results.contains(node)) {
        val resultsNext = results + (node -> peers.toSet)
        if (resultsNext.size == count) {
          context.parent ! Results(resultsNext)
          context.stop(self)
        } else context.become(waitForResults(count, resultsNext))
      }
  }
}

object ResultWorker {
  final case class CollectResults(nodes: Seq[ActorRef])
  final case class Results(results: Map[PeerNode, Set[PeerNode]])

  def props: Props = Props(new ResultWorker())
}
