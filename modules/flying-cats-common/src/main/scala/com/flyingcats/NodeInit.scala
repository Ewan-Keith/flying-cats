package com.github.flyingcats.common

import cats.effect.{IO, Ref}
import io.circe.syntax._
import io.circe._, io.circe.parser._
import com.github.flyingcats.common.Messenger._

case class InitMessage(
    src: String,
    dest: String,
    messageId: Int,
    nodeId: String,
    NodeIds: Vector[String]
) extends MaelstromMessage

case class InitResponseMessage(
    src: String,
    dest: String
) extends MaelstromMessage

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
        messageId <- c.downField("body").downField("msg_id").as[Int]
        nodeId <- c.downField("body").downField("node_id").as[String]
        nodeIds <- c.downField("body").downField("node_ids").as[Vector[String]]
      } yield InitMessage(src, dest, messageId, nodeId, nodeIds)
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
      initMessage.src
    )
    _ <- respond(
      initResponse.asJson(InitCodecs.encodeResponseMessage).noSpaces
    )
    state <- Ref[IO].of(NodeState(initMessage.nodeId, initState()))
  } yield state

}
