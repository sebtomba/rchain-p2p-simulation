package coop.rchain.comm

import akka.actor.ActorRef

import coop.rchain.crypto.codec.Base16

final case class NodeIdentifier(key: Seq[Byte]) {
  private val keyString         = Base16.encode(key.toArray)
  override def toString: String = keyString
  def toShortString: String     = toString.take(7)
}

object NodeIdentifier {
  def apply(name: String): NodeIdentifier =
    NodeIdentifier(name.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte))
}

final case class PeerNode(id: NodeIdentifier, endpoint: ActorRef) {

  def key: Seq[Byte] = id.key
  val sKey: String   = id.toString

  override def toString: String = toAddress

  val toAddress: String = sKey

}
