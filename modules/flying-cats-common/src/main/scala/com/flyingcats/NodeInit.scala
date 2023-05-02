package com.github.flyingcats.common

import cats.effect.IO
import io.circe.syntax._
import io.circe._, io.circe.parser._

case class InitMessage(src: String, dest: String, body: InitBody)
    extends MaelstromMessage

case class InitBody(messageId: Int, nodeId: String, NodeIds: Vector[String])
    extends MaelstromMessageBody {
  val messageType: MaelstromMessageType = MaelstromMessageType.Init
}

case class InitResponseMessage(
    src: String,
    dest: String,
    body: InitResponseBody
) extends MaelstromMessage

case class InitResponseBody() extends MaelstromMessageBody {
  val messageType: MaelstromMessageType = MaelstromMessageType.InitOk
}

object InitCodecs {

  def encodeResponseMessage: Encoder[InitResponseMessage] =
    new Encoder[InitResponseMessage] {
      final def apply(a: InitResponseMessage): Json = Json.obj(
        ("src", Json.fromString(a.src)),
        ("dest", Json.fromString(a.dest)),
        (
          "body",
          Json.obj(
            ("type", Json.fromString("init_ok")),
            ("in_reply_to", Json.fromInt(1))
          )
        )
      )
    }

  def decodeMessage: Decoder[InitMessage] = new Decoder[InitMessage] {
    def apply(c: HCursor): Decoder.Result[InitMessage] =
      for {
        src <- c.downField("src").as[String]
        dest <- c.downField("dest").as[String]
        body <- c.downField("body").as[InitBody]
      } yield InitMessage(src, dest, body)
  }

  implicit def decodeBody: Decoder[InitBody] = new Decoder[InitBody] {
    def apply(c: HCursor): Decoder.Result[InitBody] =
      for {
        messageId <- c.downField("msg_id").as[Int]
        nodeId <- c.downField("node_id").as[String]
        nodeIds <- c.downField("node_ids").as[Vector[String]]
      } yield InitBody(messageId, nodeId, nodeIds)
  }
}

object NodeInit {
case class NodeState(id: String)

  def initialise: IO[NodeState] = for {
    inputString <- IO.readLine
    inputJson <- IO.fromEither(parse(inputString))
    initMessage <- IO.fromEither(
      inputJson.as[InitMessage](InitCodecs.decodeMessage)
    )
    initResponse = InitResponseMessage(
      initMessage.dest,
      initMessage.src,
      InitResponseBody()
    )
    _ <- IO.println(
      initResponse.asJson(InitCodecs.encodeResponseMessage).noSpaces
    )
  } yield NodeState(initMessage.body.nodeId)


}
