package coop.rchain.comm.discovery

import cats.implicits._
import cats.Monad

import coop.rchain.comm.PeerNode

object KademliaHandleRPC {
  def handlePing[F[_]: Monad: KademliaStore](peer: PeerNode): F[Unit] =
    KademliaStore[F].updateLastSeen(peer)

  def handleLookup[F[_]: Monad: KademliaStore](
      peer: PeerNode,
      id: Array[Byte]
  ): F[Seq[PeerNode]] =
    KademliaStore[F].updateLastSeen(peer) >> KademliaStore[F].lookup(id)
}
