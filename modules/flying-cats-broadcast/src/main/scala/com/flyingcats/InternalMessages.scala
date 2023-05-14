package com.github.flyingcats.broadcast

import io.circe._
import com.github.flyingcats.common.MaelstromMessage

case class BatchedBroadcastMessage(
    src: String,
    dest: String,
    messages: Vector[Json],
    messageId: Int
) extends MaelstromMessage

object BatchedBroadcastCodecs {

  implicit def encodeBatchedBroadcastMessage: Encoder[BatchedBroadcastMessage] =
    new Encoder[BatchedBroadcastMessage] {
      final def apply(a: BatchedBroadcastMessage): Json = Json.obj(
        ("src", Json.fromString(a.src)),
        ("dest", Json.fromString(a.dest)),
        (
          "body",
          Json.obj(
            ("type", Json.fromString("batched_broadcast")),
            ("messages", Json.fromValues(a.messages)),
            ("msg_id", Json.fromInt(a.messageId))
          )
        )
      )
    }

  implicit def decodeBatchedBroadcastMessage: Decoder[BatchedBroadcastMessage] = new Decoder[BatchedBroadcastMessage] {
    def apply(c: HCursor): Decoder.Result[BatchedBroadcastMessage] =
      for {
        src <- c.downField("src").as[String]
        dest <- c.downField("dest").as[String]
        messages <- c.downField("body").downField("messages").as[Vector[Json]]
        messageId <- c.downField("body").downField("msg_id").as[Int]
      } yield BatchedBroadcastMessage(src, dest, messages, messageId)
  }
}
