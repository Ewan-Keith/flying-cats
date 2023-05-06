package com.github.flyingcats.broadcast

import cats.effect.{IO, IOApp, Ref}
import cats.syntax.all._
import io.circe._
import com.github.flyingcats.common._
import com.github.flyingcats.common.MaelstromMessageType._
import com.github.flyingcats.common.NodeState
import com.github.flyingcats.common.Messenger.sendMessage

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

case class BroadcastResponse(inReplyTo: Int)

object BroadcastDecoders {

  def encodeResponseMessage: Encoder[BroadcastResponse] =
    new Encoder[BroadcastResponse] {
      final def apply(a: BroadcastResponse): Json = Json.obj(
        ("type", Json.fromString("broadcast_ok")),
        ("in_reply_to", Json.fromInt(a.inReplyTo))
      )
    }

  def encodeBroadcastMessage: Encoder[BroadcastMessage] =
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

  def decodeMessage: Decoder[BroadcastMessage] = new Decoder[BroadcastMessage] {
    def apply(c: HCursor): Decoder.Result[BroadcastMessage] =
      for {
        src <- c.downField("src").as[String]
        dest <- c.downField("dest").as[String]
        message <- c.downField("body").downField("message").as[Json]
        messageId <- c.downField("body").downField("msg_id").as[Int]
      } yield BroadcastMessage(src, dest, message, messageId)
  }

  def decodeOkMessage: Decoder[BroadcastOkMessage] =
    new Decoder[BroadcastOkMessage] {
      def apply(c: HCursor): Decoder.Result[BroadcastOkMessage] =
        for {
          src <- c.downField("src").as[String]
          dest <- c.downField("dest").as[String]
        } yield BroadcastOkMessage(src, dest)
    }
}

case class ReadMessage(src: String, dest: String, messageId: Int)
    extends MaelstromMessage

case class ReadResponse(
    messages: Vector[Json],
    inReplyTo: Int
)

object ReadDecoders {

  def encodeResponseMessage: Encoder[ReadResponse] =
    new Encoder[ReadResponse] {
      final def apply(a: ReadResponse): Json = Json.obj(
        ("type", Json.fromString("read_ok")),
        ("in_reply_to", Json.fromInt(a.inReplyTo)),
        ("messages", Json.fromValues(a.messages.map(j => j)))
      )
    }

  def decodeMessage: Decoder[ReadMessage] = new Decoder[ReadMessage] {
    def apply(c: HCursor): Decoder.Result[ReadMessage] =
      for {
        src <- c.downField("src").as[String]
        dest <- c.downField("dest").as[String]
        messageId <- c.downField("body").downField("msg_id").as[Int]
      } yield ReadMessage(src, dest, messageId)
  }
}
case class TopologyMessage(
    id: Int,
    src: String,
    dest: String,
    topology: Map[String, Vector[String]],
    messageId: Int
) extends MaelstromMessage

case class TopologyResponse(inReplyTo: Int)

object TopologyDecoders {

  def encodeResponseMessage: Encoder[TopologyResponse] =
    new Encoder[TopologyResponse] {
      final def apply(a: TopologyResponse): Json = Json.obj(
        ("type", Json.fromString("topology_ok")),
        ("in_reply_to", Json.fromInt(a.inReplyTo))
      )
    }

  def decodeMessage: Decoder[TopologyMessage] = new Decoder[TopologyMessage] {
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

  case class BroadcastNodeState(
      messages: Vector[Json],
      topology: Map[String, Vector[String]]
  )

  val broadcastDecoder
      : PartialFunction[MaelstromMessageType, Either[Throwable, Decoder[
        MaelstromMessage
      ]]] = {
    case Broadcast =>
      Right(BroadcastDecoders.decodeMessage.widen)
    case BroadcastOk =>
      Right(BroadcastDecoders.decodeOkMessage.widen)
    case Read     => Right(ReadDecoders.decodeMessage.widen)
    case Topology => Right(TopologyDecoders.decodeMessage.widen)
  }

  def handleNewMessage(
      b: BroadcastMessage,
      nstate: Ref[IO, NodeState[BroadcastNodeState]]
  ): IO[Unit] =
    for {
      _ <- nstate.update(current =>
        NodeState(
          current.id,
          BroadcastNodeState(
            current.state.messages.appended(b.message),
            current.state.topology
          )
        )
      )
      state <- nstate.get
      nodeNieghbours <- IO.fromOption(state.state.topology.get(state.id))(
        new RuntimeException(
          s"node ID ${state.id} was not found in the provided network topology ${state.state.topology}"
        )
      )
      _ <- nodeNieghbours.traverse(dest =>
        sendMessage(
          BroadcastDecoders
            .encodeBroadcastMessage(
              BroadcastMessage(state.id, dest, b.message, b.messageId)
            )
            .noSpaces
        )
      )
    } yield ()

  val broadcastMessageResponse: PartialFunction[
    (MaelstromMessage, Ref[IO, NodeState[BroadcastNodeState]]),
    IO[Unit]
  ] = {
    case (_: BroadcastOkMessage, _) => IO.unit
    case (b: BroadcastMessage, nstate) =>
      for {
        messageIsNew <- nstate.get.map(
          !_.state.messages.contains(b.message)
        )
        _ <- IO.whenA(messageIsNew)(handleNewMessage(b, nstate))
        _ <- b.respond(
          BroadcastResponse(b.messageId),
          BroadcastDecoders.encodeResponseMessage
        )
      } yield ()
    case (r: ReadMessage, nstate) =>
      nstate.get.map(_.state.messages).flatMap { m =>
        r.respond(
          ReadResponse(m, r.messageId),
          ReadDecoders.encodeResponseMessage
        )
      }
    case (t: TopologyMessage, nstate) =>
      nstate.update(current =>
        NodeState(
          current.id,
          BroadcastNodeState(current.state.messages, t.topology)
        )
      ) >>
        t.respond(
          TopologyResponse(t.messageId),
          TopologyDecoders.encodeResponseMessage
        )
  }

  def initBroadcastState(): BroadcastNodeState = BroadcastNodeState(
    Vector.empty,
    Map.empty
  )

  def run: IO[Unit] =
    MaelstromApp.buildAppLoop(
      broadcastDecoder,
      broadcastMessageResponse,
      initBroadcastState
    )
}
