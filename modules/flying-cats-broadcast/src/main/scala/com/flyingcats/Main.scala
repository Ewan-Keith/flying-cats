package com.github.flyingcats.broadcast

import cats.effect.{IO, IOApp, Ref}
import cats.syntax.all._
import io.circe._
import com.github.flyingcats.common._
import com.github.flyingcats.common.NodeState
import com.github.flyingcats.common.Messenger.sendMessage
import scala.concurrent.duration._
import cats.effect.std.Random
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

object Main extends IOApp.Simple {
  import ReadCodecs._
  import BroadcastCodecs._
  import TopologyCodecs._

  case class BroadcastNodeState(
      nodeId: String,
      messages: Vector[Json],
      topology: Map[String, Vector[String]],
      batchBroadcastBuffer: Vector[Json],
      lastBatchBroadcast: Option[LocalDateTime]
  ) extends NodeState {

    def getNodeNeighbours(
    ): Either[Throwable, Vector[String]] = Either.fromOption(
      topology.get(nodeId),
      new RuntimeException(
        s"node ID $nodeId was not found in the provided network topology $topology"
      )
    )

    def addMessage(message: Json): BroadcastNodeState =
      this.copy(messages = messages.appended(message))

    def addMessages(newMessages: Vector[Json]): BroadcastNodeState =
      this.copy(
        messages = messages.concat(newMessages).distinct,
        batchBroadcastBuffer = batchBroadcastBuffer.concat(newMessages).distinct
      )

    def updateTopology(topology: Map[String, Vector[String]]): BroadcastNodeState =
      this.copy(topology = topology)

    def forEachNeighbour(action: String => IO[Unit]): IO[Unit] =
      IO.fromEither(getNodeNeighbours()).flatMap(_.traverse_(action))

    def isForwardBroadcastDue: Boolean = lastBatchBroadcast match {
      case None => !batchBroadcastBuffer.isEmpty
      case Some(dt) =>
        ChronoUnit.MILLIS.between(dt, LocalDateTime.now()) > 4000 && (!batchBroadcastBuffer.isEmpty)
    }

    def clearBatchBroadcastBuffer: BroadcastNodeState =
      this.copy(batchBroadcastBuffer = Vector.empty, lastBatchBroadcast = Some(LocalDateTime.now()))
  }

  val broadcastDecoder: PartialFunction[String, Decoder[MaelstromMessage]] = {
    case "broadcast"         => BroadcastCodecs.decodeBroadcastMessage.widen
    case "broadcast_ok"      => BroadcastCodecs.decodeBroadcastOkMessage.widen
    case "read"              => ReadCodecs.decodeReadMessage.widen
    case "read_ok"           => ReadCodecs.decodeReadOkMessage.widen
    case "topology"          => TopologyCodecs.decodeTopologyMessage.widen
    case "batched_broadcast" => BatchedBroadcastCodecs.decodeBatchedBroadcastMessage.widen
  }

  def handleNewBatchBroadcastMessage(
      b: BatchedBroadcastMessage,
      nstate: Ref[IO, BroadcastNodeState]
  ): IO[Unit] =
    for {
      _ <- nstate.update(_.addMessages(b.messages))
    } yield ()

  val broadcastMessageResponse: PartialFunction[
    (MaelstromMessage, Ref[IO, BroadcastNodeState]),
    IO[Unit]
  ] = {
    case (_: BroadcastOkMessage, _)   => IO.unit
    case (rOk: ReadOkMessage, nstate) => nstate.update(_.addMessages(rOk.messages))
    case (b: BroadcastMessage, nstate) =>
      for {
        messageIsNew <- nstate.get.map(
          !_.messages.contains(b.message)
        )
        _ <- IO.whenA(messageIsNew)(
          handleNewBatchBroadcastMessage(b.asBatchedBroadcastMessage, nstate)
        )
        _ <- b.respond(BroadcastOkMessageBody(b.messageId))
      } yield ()
    case (r: ReadMessage, nstate) =>
      nstate.get.map(_.messages).flatMap { m =>
        r.respond(ReadOkMessageBody(m, r.messageId))
      }
    case (b: BatchedBroadcastMessage, nstate) =>
      for {
        currentMessages <- nstate.get.map(_.messages)
        newMessages = b.messages.filter(!currentMessages.contains(_))
        _ <- IO.whenA(!newMessages.isEmpty)(handleNewBatchBroadcastMessage(b, nstate))
      } yield ()
    case (t: TopologyMessage, nstate) =>
      nstate.update(_.updateTopology(t.topology)) >>
        t.respond(TopologyOkMessageBody(t.messageId))
  }

  def readNeighboursAfterN(ns: Ref[IO, BroadcastNodeState], interval: Duration): IO[Unit] = {
    for {
      _ <- IO.sleep(interval)
      messageId <- Random.scalaUtilRandom[IO].flatMap(_.nextInt)
      nodeState <- ns.get
      _ <- nodeState.forEachNeighbour(neighbourId =>
        sendMessage(
          encodeReadMessage(ReadMessage(nodeState.nodeId, neighbourId, messageId)).noSpaces
        )
      )
    } yield ()
  }.foreverM

  def forwardBatchBroadcastMessages(ns: Ref[IO, BroadcastNodeState]): IO[Unit] = {
    for {
      state <- ns.get
      messageId <- Random.scalaUtilRandom[IO].flatMap(_.nextInt)
      _ <- IO.whenA(state.isForwardBroadcastDue) {
        state.forEachNeighbour(neighbour =>
          sendMessage(
            BatchedBroadcastCodecs
              .encodeBatchedBroadcastMessage(
                BatchedBroadcastMessage(
                  state.nodeId,
                  neighbour,
                  state.batchBroadcastBuffer,
                  messageId
                )
              )
              .noSpaces
          )
        ) >> ns.update(_.clearBatchBroadcastBuffer)
      }
    } yield ()
  }.foreverM

  def initBackroundActions(ns: Ref[IO, BroadcastNodeState]): IO[Unit] =
    IO.both(readNeighboursAfterN(ns, 2.seconds), forwardBatchBroadcastMessages(ns)).void

  def run: IO[Unit] =
    MaelstromApp.buildAppLoop(
      broadcastDecoder,
      broadcastMessageResponse,
      BroadcastNodeState(_, Vector.empty, Map.empty, Vector.empty, None),
      initBackroundActions
    )
}
