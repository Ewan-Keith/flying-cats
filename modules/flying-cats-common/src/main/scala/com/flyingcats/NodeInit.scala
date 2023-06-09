package com.github.flyingcats.common

import cats.effect.{IO, Ref}
import io.circe._, io.circe.parser._
import com.github.flyingcats.common.Messenger._

case class InitMessage(
    src: String,
    dest: String,
    messageId: Int,
    nodeId: String,
    nodeIds: Vector[String]
) extends MaelstromMessage

case class InitResponse()

object InitCodecs {

  implicit def encodeResponse: Encoder[InitResponse] =
    new Encoder[InitResponse] {
      final def apply(a: InitResponse): Json = Json.obj(
        ("type", Json.fromString("init_ok")),
        ("in_reply_to", Json.fromInt(1))
      )
    }

  implicit def decodeMessage: Decoder[InitMessage] = new Decoder[InitMessage] {
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

// case class NodeState[A](id: String, state: A)
trait NodeState {
  val nodeId: String
}
case class IdOnlyState(nodeId: String) extends NodeState

object NodeInit {

  import InitCodecs._

  def initialise[A <: NodeState](initState: String => A): IO[Ref[IO, A]] = for {
    inputString <- IO.readLine
    _ <- logReceived(inputString)
    inputJson <- IO.fromEither(parse(inputString))
    initMessage <- IO.fromEither(
      inputJson.as[InitMessage](InitCodecs.decodeMessage)
    )
    state <- Ref[IO].of(initState(initMessage.nodeId))
    // state <- Ref[IO].of(NodeState(initMessage.nodeId, initState()))
    _ <- initMessage.respond(InitResponse())
  } yield state

}
