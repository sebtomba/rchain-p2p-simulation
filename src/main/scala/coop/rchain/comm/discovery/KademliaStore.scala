package coop.rchain.comm.discovery

import cats.data.EitherT
import cats.Functor
import cats.effect.Sync

import coop.rchain.comm._

trait KademliaStore[F[_]] {
  def peers: F[Seq[PeerNode]]
  def sparseness: F[Seq[Int]]
  def fillFactor: F[Double]
  def updateLastSeen(peerNode: PeerNode): F[Unit]
  def lookup(key: Seq[Byte]): F[Seq[PeerNode]]
  def find(key: Seq[Byte]): F[Option[PeerNode]]
  def remove(key: Seq[Byte]): F[Unit]
}

object KademliaStore extends KademliaStoreInstances {
  def apply[F[_]](implicit ev: KademliaStore[F]): KademliaStore[F] = ev
}

sealed abstract class KademliaStoreInstances {
  implicit def eitherTKademliaStore[E, F[_]: Functor: KademliaStore]
      : KademliaStore[EitherT[F, E, ?]] =
    new KademliaStore[EitherT[F, E, ?]] {

      def peers: EitherT[F, E, Seq[PeerNode]] =
        EitherT.liftF(KademliaStore[F].peers)

      def sparseness: EitherT[F, E, Seq[Int]] =
        EitherT.liftF(KademliaStore[F].sparseness)

      def fillFactor: EitherT[F, E, Double] =
        EitherT.liftF(KademliaStore[F].fillFactor)

      def updateLastSeen(peerNode: PeerNode): EitherT[F, E, Unit] =
        EitherT.liftF(KademliaStore[F].updateLastSeen(peerNode))

      def lookup(key: Seq[Byte]): EitherT[F, E, Seq[PeerNode]] =
        EitherT.liftF(KademliaStore[F].lookup(key))

      def find(key: Seq[Byte]): EitherT[F, E, Option[PeerNode]] =
        EitherT.liftF(KademliaStore[F].find(key))

      def remove(key: Seq[Byte]): EitherT[F, E, Unit] =
        EitherT.liftF(KademliaStore[F].remove(key))
    }

  def table[F[_]: Sync: KademliaRPC](id: NodeIdentifier): KademliaStore[F] =
    new KademliaStore[F] {
      private val table = PeerTable[PeerNode, F](id.key)

      def peers: F[Seq[PeerNode]]                     = table.peers
      def sparseness: F[Seq[Int]]                     = table.sparseness
      def fillFactor: F[Double]                       = table.fillFactor
      def lookup(key: Seq[Byte]): F[Seq[PeerNode]]    = table.lookup(key)
      def find(key: Seq[Byte]): F[Option[PeerNode]]   = table.find(key)
      def remove(key: Seq[Byte]): F[Unit]             = table.remove(key)
      def updateLastSeen(peerNode: PeerNode): F[Unit] = table.updateLastSeen(peerNode)
    }
}
