package coop.rchain.simulation.discovery.analysis

import java.io.{BufferedWriter, FileWriter}
import java.nio.file.{Path, Paths}
import java.text.SimpleDateFormat
import java.util.Calendar

import cats.implicits._

import coop.rchain.comm.{NodeIdentifier, PeerNode}
import coop.rchain.graphz.{GraphSerializer, WriterSerializer}

import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

object Reporter {
  private val format = new SimpleDateFormat("yyyyMMddHHmmss")

  def reportNetwork(results: Map[PeerNode, Set[PeerNode]], cliqueId: () => NodeIdentifier): Unit = {
    val now    = Calendar.getInstance().getTime
    val ts     = format.format(now)
    val folder = Paths.get(ts)
    if (folder.toFile.mkdir()) {
      exportCliquesNetworkGraph(folder, results, cliqueId).runSyncUnsafe()
    }
  }

  def reportAll(results: Map[PeerNode, Set[PeerNode]], cliqueId: () => NodeIdentifier): Unit = {
    val now    = Calendar.getInstance().getTime
    val ts     = format.format(now)
    val folder = Paths.get(ts)
    if (folder.toFile.mkdir()) {
      (for {
        _ <- exportRawGraph(folder, results)
        _ <- exportCliquesGraph(folder, results, cliqueId)
        _ <- exportCliquesNetworkGraph(folder, results, cliqueId)
      } yield ()).runSyncUnsafe()
    }
  }

  private def exportRawGraph(
      folder: Path,
      results: Map[PeerNode, Set[PeerNode]]
  ): Task[Unit] = {
    val writer                              = new BufferedWriter(new FileWriter(folder.resolve("raw.gv").toFile))
    implicit val ser: GraphSerializer[Task] = new WriterSerializer[Task](writer)
    val graph                               = NetworkTransformer.networkFromPeerNodes(results)
    GraphzGenerator
      .generate[Task](graph)
      .doOnFinish(_ => Task.delay(writer.close()))
      .void
  }

  private def exportCliquesGraph(
      folder: Path,
      results: Map[PeerNode, Set[PeerNode]],
      cliqueId: () => NodeIdentifier
  ): Task[Unit] = {
    val writer                              = new BufferedWriter(new FileWriter(folder.resolve("cliques.gv").toFile))
    implicit val ser: GraphSerializer[Task] = new WriterSerializer[Task](writer)
    val graph                               = NetworkTransformer.networkFromPeerNodes(results)
    GraphzGenerator
      .generate[Task](
        NetworkTransformer.reduceToCliques(graph, cliqueId)
      )
      .doOnFinish(_ => Task.delay(writer.close()))
      .void
  }

  private def exportCliquesNetworkGraph(
      folder: Path,
      results: Map[PeerNode, Set[PeerNode]],
      cliqueId: () => NodeIdentifier
  ): Task[Unit] = {
    val writer                              = new BufferedWriter(new FileWriter(folder.resolve("network.gv").toFile))
    implicit val ser: GraphSerializer[Task] = new WriterSerializer[Task](writer)
    val graph                               = NetworkTransformer.networkFromPeerNodes(results)
    GraphzGenerator
      .generate[Task](
        NetworkTransformer.reduceToNetworkOfCliques(graph, cliqueId)
      )
      .doOnFinish(_ => Task.delay(writer.close()))
      .void
  }
}
