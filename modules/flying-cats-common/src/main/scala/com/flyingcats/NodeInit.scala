package com.github.flyingcats.common

import cats.effect.{IO, Ref}
import io.circe.syntax._
import io.circe._, io.circe.parser._
import com.github.flyingcats.common.MaelstromApp.{respond, logReceived}

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

case class NodeState[A](id: String, state: A)
object NodeInit {

  def initialise[A](initState: () => A): IO[Ref[IO, NodeState[A]]] = for {
    inputString <- IO.readLine
    _ <- logReceived(inputString)
    inputJson <- IO.fromEither(parse(inputString))
    initMessage <- IO.fromEither(
      inputJson.as[InitMessage](InitCodecs.decodeMessage)
    )
    initResponse = InitResponseMessage(
      initMessage.dest,
      initMessage.src,
      InitResponseBody()
    )
    _ <- respond(
      initResponse.asJson(InitCodecs.encodeResponseMessage).noSpaces
    )
    state <- Ref[IO].of(NodeState(initMessage.body.nodeId, initState()))
  } yield state

}
