package com.github.flyingcats.broadcast

import io.circe._
import com.github.flyingcats.common.MaelstromMessage

case class BroadcastMessage(
    src: String,
    dest: String,
    message: Json,
    messageId: Int
) extends MaelstromMessage {

  def asBatchedBroadcastMessage: BatchedBroadcastMessage =
    BatchedBroadcastMessage(src, dest, Vector(message), messageId)
}

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

