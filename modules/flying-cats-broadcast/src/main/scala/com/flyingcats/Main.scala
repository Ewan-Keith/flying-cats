package com.github.flyingcats.broadcast

import cats.effect.{IO, IOApp, Ref}
import cats.syntax.all._
import io.circe._
import com.github.flyingcats.common._
import com.github.flyingcats.common.NodeState
import com.github.flyingcats.common.Messenger.sendMessage
import scala.concurrent.duration._
import cats.effect.std.Random

object Main extends IOApp.Simple {
  import ReadCodecs._
  import BroadcastCodecs._
  import TopologyCodecs._

  case class BroadcastNodeState(
      nodeId: String,
      messages: Vector[Json],
      topology: Map[String, Vector[String]]
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
      this.copy(messages = messages.concat(newMessages).distinct)

    def updateTopology(topology: Map[String, Vector[String]]): BroadcastNodeState =
      this.copy(topology = topology)

    def forEachNeighbour(action: String => IO[Unit]): IO[Unit] =
      IO.fromEither(getNodeNeighbours()).flatMap(_.traverse_(action))
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
      state <- nstate.get
      _ <- state.forEachNeighbour(neighbour =>
        IO.whenA(neighbour != b.src) {
          sendMessage(
            BatchedBroadcastCodecs
              .encodeBatchedBroadcastMessage(
                BatchedBroadcastMessage(state.nodeId, neighbour, b.messages, b.messageId)
              )
              .noSpaces
          )
        }
      )
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

  def run: IO[Unit] =
    MaelstromApp.buildAppLoop(
      broadcastDecoder,
      broadcastMessageResponse,
      BroadcastNodeState(_, Vector.empty, Map.empty),
      readNeighboursAfterN(_, 2.seconds)
    )
}
