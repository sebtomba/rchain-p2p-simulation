package coop.rchain.comm

import scala.util.Random

import akka.actor.ActorRef

import coop.rchain.crypto.codec.Base16

final case class NodeIdentifier(key: Seq[Byte]) {
  private val keyString         = Base16.encode(key.toArray)
  override def toString: String = keyString
  def toShortString: String     = toString.take(7)
}

object NodeIdentifier {
  def random(addressSize: Int): NodeIdentifier = {
    val key: Array[Byte] = Array.ofDim(addressSize)
    Random.nextBytes(key)
    NodeIdentifier(key)
  }
}

final case class PeerNode(id: NodeIdentifier, endpoint: ActorRef) {

  def key: Seq[Byte] = id.key
  val sKey: String   = id.toString

  override def toString: String = toAddress

  val toAddress: String = sKey

}
