package com.github.flyingcats.broadcast

import cats.effect.{IO, IOApp, Ref}
import io.circe._
import io.circe.syntax._
import cats.syntax.functor._
import com.github.flyingcats.common._
import com.github.flyingcats.common.Messenger.respond
import com.github.flyingcats.common.MaelstromMessageType._
import com.github.flyingcats.common.NodeState

case class BroadcastMessage(
    src: String,
    dest: String,
    message: Json,
    messageId: Int
) extends MaelstromMessage

case class BroadcastResponseMessage(
    src: String,
    dest: String,
    inReplyTo: Int
) extends MaelstromMessage

object BroadcastDecoders {

  def encodeResponseMessage: Encoder[BroadcastResponseMessage] =
    new Encoder[BroadcastResponseMessage] {
      final def apply(a: BroadcastResponseMessage): Json = Json.obj(
        ("src", Json.fromString(a.src)),
        ("dest", Json.fromString(a.dest)),
        (
          "body",
          Json.obj(
            ("type", Json.fromString("broadcast_ok")),
            ("in_reply_to", Json.fromInt(a.inReplyTo))
          )
        )
      )
    }

  def decodeMessage: Decoder[BroadcastMessage] = new Decoder[BroadcastMessage] {
    def apply(c: HCursor): Decoder.Result[BroadcastMessage] =
      for {
        src <- c.downField("src").as[String]
        dest <- c.downField("dest").as[String]
        // body <- c.downField("body").as[BroadcastBody]
        message <- c.downField("body").downField("message").as[Json]
        messageId <- c.downField("body").downField("msg_id").as[Int]
      } yield BroadcastMessage(src, dest, message, messageId)
  }
}

case class ReadMessage(src: String, dest: String, messageId: Int)
    extends MaelstromMessage

case class ReadResponseMessage(
    src: String,
    dest: String,
    messages: Vector[Json],
    inReplyTo: Int
) extends MaelstromMessage

object ReadDecoders {

  def encodeResponseMessage: Encoder[ReadResponseMessage] =
    new Encoder[ReadResponseMessage] {
      final def apply(a: ReadResponseMessage): Json = Json.obj(
        ("src", Json.fromString(a.src)),
        ("dest", Json.fromString(a.dest)),
        (
          "body",
          Json.obj(
            ("type", Json.fromString("read_ok")),
            ("in_reply_to", Json.fromInt(a.inReplyTo)),
            ("messages", Json.fromValues(a.messages.map(j => j)))
          )
        )
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

case class TopologyResponseMessage(
    src: String,
    dest: String,
    inReplyTo: Int
) extends MaelstromMessage

object TopologyDecoders {

  def encodeResponseMessage: Encoder[TopologyResponseMessage] =
    new Encoder[TopologyResponseMessage] {
      final def apply(a: TopologyResponseMessage): Json = Json.obj(
        ("src", Json.fromString(a.src)),
        ("dest", Json.fromString(a.dest)),
        (
          "body",
          Json.obj(
            ("type", Json.fromString("topology_ok")),
            ("in_reply_to", Json.fromInt(a.inReplyTo))
          )
        )
      )
    }

  def decodeMessage: Decoder[TopologyMessage] = new Decoder[TopologyMessage] {
    def apply(c: HCursor): Decoder.Result[TopologyMessage] =
      for {
        id <- c.downField("id").as[Int]
        src <- c.downField("src").as[String]
        dest <- c.downField("dest").as[String]
        topology <- c.downField("body").downField("topology").as[Map[String, Vector[String]]]
        messageId <- c.downField("body").downField("msg_id").as[Int]
      } yield TopologyMessage(id, src, dest, topology, messageId)
  }
}

object Main extends IOApp.Simple {

  case class BroadcastNodeState(messages: Vector[Json])

  val broadcastDecoder
      : PartialFunction[MaelstromMessageType, Either[Throwable, Decoder[
        MaelstromMessage
      ]]] = {
    case Broadcast =>
      Right(BroadcastDecoders.decodeMessage.widen)
    case Read     => Right(ReadDecoders.decodeMessage.widen)
    case Topology => Right(TopologyDecoders.decodeMessage.widen)
  }

  val broadcastMessageResponse: PartialFunction[
    (MaelstromMessage, Ref[IO, NodeState[BroadcastNodeState]]),
    IO[Unit]
  ] = {
    case (BroadcastMessage(src, dest, message, messageId), nstate) =>
      nstate.update(current =>
        NodeState(
          current.id,
          BroadcastNodeState(current.state.messages.appended(message))
        )
      ) >>
        respond(
          BroadcastResponseMessage(
            dest,
            src,
            messageId
          ).asJson(BroadcastDecoders.encodeResponseMessage).noSpaces
        )
    case (ReadMessage(src, dest, messageId), nstate) =>
      nstate.get.map(_.state.messages).flatMap { m =>
        respond(
          ReadResponseMessage(
            dest,
            src,
            m,
            messageId
          ).asJson(ReadDecoders.encodeResponseMessage).noSpaces
        )
      }
    case (TopologyMessage(_, src, dest, _, messageId), _) =>
      respond(
        TopologyResponseMessage(
          dest,
          src,
          messageId
        ).asJson(TopologyDecoders.encodeResponseMessage).noSpaces
      )
  }

  def initBroadcastState(): BroadcastNodeState = BroadcastNodeState(
    Vector.empty
  )

  def run: IO[Unit] =
    MaelstromApp.buildAppLoop(
      broadcastDecoder,
      broadcastMessageResponse,
      initBroadcastState
    )
}
