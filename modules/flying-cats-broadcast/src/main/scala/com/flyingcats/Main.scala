package com.github.flyingcats.broadcast

import cats.effect.{IO, IOApp, Ref}
import io.circe._, io.circe.parser._
import io.circe.syntax._
import cats.syntax.functor._
import com.github.flyingcats.common.{
  MaelstromApp,
  MaelstromMessage,
  MaelstromMessageBody,
  MaelstromMessageType
}
import com.github.flyingcats.common.Messenger.respond
import com.github.flyingcats.common.MaelstromMessageType._
import com.github.flyingcats.common.NodeState

case class BroadcastMessage(src: String, dest: String, body: BroadcastBody)
    extends MaelstromMessage

case class BroadcastBody(
    message: Json,
    messageId: Int
) extends MaelstromMessageBody {
  val messageType: MaelstromMessageType = Broadcast
}

case class BroadcastResponseMessage(
    src: String,
    dest: String,
    body: BroadcastResponseBody
) extends MaelstromMessage

case class BroadcastResponseBody(
  inReplyTo: Int
) extends MaelstromMessageBody {
  val messageType: MaelstromMessageType = Broadcast
}

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
            ("in_reply_to", Json.fromInt(a.body.inReplyTo))
          )
        )
      )
    }

  def decodeMessage: Decoder[BroadcastMessage] = new Decoder[BroadcastMessage] {
    def apply(c: HCursor): Decoder.Result[BroadcastMessage] =
      for {
        src <- c.downField("src").as[String]
        dest <- c.downField("dest").as[String]
        body <- c.downField("body").as[BroadcastBody]
      } yield BroadcastMessage(src, dest, body)
  }

  implicit def decodeBody: Decoder[BroadcastBody] = new Decoder[BroadcastBody] {
    def apply(c: HCursor): Decoder.Result[BroadcastBody] =
      for {
        message <- c.downField("message").as[Json]
        messageId <- c.downField("msg_id").as[Int]
      } yield BroadcastBody(message, messageId)
  }
}

case class ReadMessage(src: String, dest: String, body: ReadBody)
    extends MaelstromMessage

case class ReadBody(
  messageId: Int
) extends MaelstromMessageBody {
  val messageType: MaelstromMessageType = Read
}

case class ReadResponseMessage(
    src: String,
    dest: String,
    body: ReadResponseBody
) extends MaelstromMessage

case class ReadResponseBody(
    messages: Vector[Json],
    inReplyTo: Int
) extends MaelstromMessageBody {
  val messageType: MaelstromMessageType = Read
}

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
            ("in_reply_to", Json.fromInt(a.body.inReplyTo)),
            ("messages", Json.fromValues(a.body.messages.map(j => j)))
          )
        )
      )
    }

  def decodeMessage: Decoder[ReadMessage] = new Decoder[ReadMessage] {
    def apply(c: HCursor): Decoder.Result[ReadMessage] =
      for {
        src <- c.downField("src").as[String]
        dest <- c.downField("dest").as[String]
        body <- c.downField("body").as[ReadBody]
      } yield ReadMessage(src, dest, body)
  }

  implicit def decodeBody: Decoder[ReadBody] = new Decoder[ReadBody] {
    def apply(c: HCursor): Decoder.Result[ReadBody] = // Right(ReadBody())
      for {
        messageId <- c.downField("msg_id").as[Int]
      } yield ReadBody(messageId)
  }
}
case class TopologyMessage(
    id: Int,
    src: String,
    dest: String,
    body: TopologyBody
) extends MaelstromMessage

case class TopologyBody(
    topology: Map[String, Vector[String]],
    messageId: Int
) extends MaelstromMessageBody {
  val messageType: MaelstromMessageType = Topology
}

case class TopologyResponseMessage(
    src: String,
    dest: String,
    body: TopologyResponseBody
) extends MaelstromMessage

case class TopologyResponseBody(
    inReplyTo: Int
) extends MaelstromMessageBody {
  val messageType: MaelstromMessageType = Topology
}

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
            ("in_reply_to", Json.fromInt(a.body.inReplyTo))
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
        body <- c.downField("body").as[TopologyBody]
      } yield TopologyMessage(id, src, dest, body)
  }

  implicit def decodeBody: Decoder[TopologyBody] = new Decoder[TopologyBody] {
    def apply(c: HCursor): Decoder.Result[TopologyBody] =
      for {
        topology <- c.downField("topology").as[Map[String, Vector[String]]]
        messageId <- c.downField("msg_id").as[Int]
      } yield TopologyBody(topology, messageId)

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
    case (BroadcastMessage(src, dest, bbody), nstate) =>
      nstate.update(current =>
        NodeState(
          current.id,
          BroadcastNodeState(current.state.messages.appended(bbody.message))
        )
      ) >>
        respond(
          BroadcastResponseMessage(
            dest,
            src,
            BroadcastResponseBody(
              bbody.messageId
            )
          ).asJson(BroadcastDecoders.encodeResponseMessage).noSpaces
        )
    case (ReadMessage(src, dest, rbody), nstate) =>
      nstate.get.map(_.state.messages).flatMap { m =>
        respond(
          ReadResponseMessage(
            dest,
            src,
            ReadResponseBody(m, rbody.messageId)
          ).asJson(ReadDecoders.encodeResponseMessage).noSpaces
        )
      }
    case (TopologyMessage(id, src, dest, tbody), _) =>
      respond(
        TopologyResponseMessage(
          dest,
          src,
          TopologyResponseBody(tbody.messageId)
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
