package com.github.flyingcats.broadcast

import cats.effect.{IO, IOApp, Ref}
import cats.syntax.all._
import io.circe._
import com.github.flyingcats.common._
import com.github.flyingcats.common.NodeState
import com.github.flyingcats.common.Messenger.sendMessage
import scala.concurrent.duration._
import cats.effect.std.Random

case class BroadcastMessage(
    src: String,
    dest: String,
    message: Json,
    messageId: Int
) extends MaelstromMessage

case class BroadcastOkMessage(
    src: String,
    dest: String
) extends MaelstromMessage

case class BroadcastOkMessageBody(inReplyTo: Int)

object BroadcastCodecs {

  implicit def encodeBroadcastOkMessageBody: Encoder[BroadcastOkMessageBody] =
    new Encoder[BroadcastOkMessageBody] {
      final def apply(a: BroadcastOkMessageBody): Json = Json.obj(
        ("type", Json.fromString("broadcast_ok")),
        ("in_reply_to", Json.fromInt(a.inReplyTo))
      )
    }

  implicit def encodeBroadcastMessage: Encoder[BroadcastMessage] =
    new Encoder[BroadcastMessage] {
      final def apply(a: BroadcastMessage): Json = Json.obj(
        ("src", Json.fromString(a.src)),
        ("dest", Json.fromString(a.dest)),
        (
          "body",
          Json.obj(
            ("type", Json.fromString("broadcast")),
            ("message", a.message),
            ("msg_id", Json.fromInt(a.messageId))
          )
        )
      )
    }

  implicit def decodeBroadcastMessage: Decoder[BroadcastMessage] = new Decoder[BroadcastMessage] {
    def apply(c: HCursor): Decoder.Result[BroadcastMessage] =
      for {
        src <- c.downField("src").as[String]
        dest <- c.downField("dest").as[String]
        message <- c.downField("body").downField("message").as[Json]
        messageId <- c.downField("body").downField("msg_id").as[Int]
      } yield BroadcastMessage(src, dest, message, messageId)
  }

  implicit def decodeBroadcastOkMessage: Decoder[BroadcastOkMessage] =
    new Decoder[BroadcastOkMessage] {
      def apply(c: HCursor): Decoder.Result[BroadcastOkMessage] =
        for {
          src <- c.downField("src").as[String]
          dest <- c.downField("dest").as[String]
        } yield BroadcastOkMessage(src, dest)
    }
}

case class ReadMessage(src: String, dest: String, messageId: Int) extends MaelstromMessage
case class ReadOkMessage(
    src: String,
    dest: String,
    messages: Vector[Json],
    messageId: Option[Int],
    inReplyTo: Int
) extends MaelstromMessage

case class ReadOkMessageBody(
    messages: Vector[Json],
    inReplyTo: Int
)

object ReadCodecs {

  implicit def encodeReadOkMessageBody: Encoder[ReadOkMessageBody] =
    new Encoder[ReadOkMessageBody] {
      final def apply(a: ReadOkMessageBody): Json = Json.obj(
        ("type", Json.fromString("read_ok")),
        ("in_reply_to", Json.fromInt(a.inReplyTo)),
        ("messages", Json.fromValues(a.messages.map(j => j)))
      )
    }

  implicit def encodeReadMessage: Encoder[ReadMessage] =
    new Encoder[ReadMessage] {
      final def apply(r: ReadMessage): Json = Json.obj(
        ("src", Json.fromString(r.src)),
        ("dest", Json.fromString(r.dest)),
        (
          "body",
          Json.obj(
            ("type", Json.fromString("read")),
            ("msg_id", Json.fromInt(r.messageId))
          )
        )
      )
    }

  implicit def decodeReadMessage: Decoder[ReadMessage] = new Decoder[ReadMessage] {
    def apply(c: HCursor): Decoder.Result[ReadMessage] =
      for {
        src <- c.downField("src").as[String]
        dest <- c.downField("dest").as[String]
        messageId <- c.downField("body").downField("msg_id").as[Int]
      } yield ReadMessage(src, dest, messageId)
  }

  implicit def decodeReadOkMessage: Decoder[ReadOkMessage] = new Decoder[ReadOkMessage] {
    def apply(c: HCursor): Decoder.Result[ReadOkMessage] =
      for {
        src <- c.downField("src").as[String]
        dest <- c.downField("dest").as[String]
        messageId <- c.downField("body").downField("msg_id").as[Option[Int]]
        messages <- c.downField("body").downField("messages").as[Vector[Json]]
        inReplyTo <- c.downField("body").downField("in_reply_to").as[Int]
      } yield ReadOkMessage(src, dest, messages, messageId, inReplyTo)
  }
}

case class TopologyMessage(
    id: Int,
    src: String,
    dest: String,
    topology: Map[String, Vector[String]],
    messageId: Int
) extends MaelstromMessage

case class TopologyOkMessageBody(inReplyTo: Int)

object TopologyCodecs {

  implicit def encodeTopologyOkMessageBody: Encoder[TopologyOkMessageBody] =
    new Encoder[TopologyOkMessageBody] {
      final def apply(a: TopologyOkMessageBody): Json = Json.obj(
        ("type", Json.fromString("topology_ok")),
        ("in_reply_to", Json.fromInt(a.inReplyTo))
      )
    }

  implicit def decodeTopologyMessage: Decoder[TopologyMessage] = new Decoder[TopologyMessage] {
    def apply(c: HCursor): Decoder.Result[TopologyMessage] =
      for {
        id <- c.downField("id").as[Int]
        src <- c.downField("src").as[String]
        dest <- c.downField("dest").as[String]
        topology <- c
          .downField("body")
          .downField("topology")
          .as[Map[String, Vector[String]]]
        messageId <- c.downField("body").downField("msg_id").as[Int]
      } yield TopologyMessage(id, src, dest, topology, messageId)
  }
}

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
    case "broadcast"    => BroadcastCodecs.decodeBroadcastMessage.widen
    case "broadcast_ok" => BroadcastCodecs.decodeBroadcastOkMessage.widen
    case "read"         => ReadCodecs.decodeReadMessage.widen
    case "read_ok"      => ReadCodecs.decodeReadOkMessage.widen
    case "topology"     => TopologyCodecs.decodeTopologyMessage.widen
  }

  def handleNewBroadcastMessage(
      b: BroadcastMessage,
      nstate: Ref[IO, BroadcastNodeState]
  ): IO[Unit] =
    for {
      _ <- nstate.update(_.addMessage(b.message))
      state <- nstate.get
      _ <- state.forEachNeighbour(neighbour =>
        IO.whenA(neighbour != b.src) {
          sendMessage(
            BroadcastCodecs
              .encodeBroadcastMessage(
                BroadcastMessage(state.nodeId, neighbour, b.message, b.messageId)
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
        _ <- IO.whenA(messageIsNew)(handleNewBroadcastMessage(b, nstate))
        _ <- b.respond(BroadcastOkMessageBody(b.messageId))
      } yield ()
    case (r: ReadMessage, nstate) =>
      nstate.get.map(_.messages).flatMap { m =>
        r.respond(ReadOkMessageBody(m, r.messageId))
      }
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
