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
            ("type", Json.fromString("broadcast")),
            ("messages", Json.fromValues(a.messages)),
            ("msg_id", Json.fromInt(a.messageId))
          )
        )
      )
    }
}
