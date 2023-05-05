package com.github.flyingcats.echo

import cats.effect.{IO, IOApp}
import io.circe._
import cats.syntax.functor._
import com.github.flyingcats.common._
import com.github.flyingcats.common.MaelstromMessageType._

case class EchoMessage(src: String, dest: String, messageId: Int, echo: String)
    extends MaelstromMessage

case class EchoResponse(
    messageId: Int,
    inReplyTo: Int,
    echo: String
)

object EchoDecoders {

  def encodeResponseMessage: Encoder[EchoResponse] =
    new Encoder[EchoResponse] {
      final def apply(a: EchoResponse): Json =
        Json.obj(
          ("type", Json.fromString("echo_ok")),
          ("msg_id", Json.fromInt(a.messageId)),
          ("in_reply_to", Json.fromInt(a.inReplyTo)),
          ("echo", Json.fromString(a.echo))
        )
    }

  def decodeMessage: Decoder[EchoMessage] = new Decoder[EchoMessage] {
    def apply(c: HCursor): Decoder.Result[EchoMessage] =
      for {
        src <- c.downField("src").as[String]
        dest <- c.downField("dest").as[String]
        messageId <- c.downField("body").downField("msg_id").as[Int]
        echo <- c.downField("body").downField("echo").as[String]
      } yield EchoMessage(src, dest, messageId, echo)
  }
}

object Main extends IOApp.Simple {

  val echoDecoder
      : PartialFunction[MaelstromMessageType, Either[Throwable, Decoder[
        MaelstromMessage
      ]]] = { case Echo => Right(EchoDecoders.decodeMessage.widen) }

  val echoMessageResponse: PartialFunction[(MaelstromMessage, _), IO[Unit]] = {
    case (e: EchoMessage, _) =>
      e.respond(
        EchoResponse(
          e.messageId,
          e.messageId,
          e.echo
        ),
        EchoDecoders.encodeResponseMessage
      )
  }

  def initEmptyState(): Unit = ()

  def run: IO[Unit] =
    MaelstromApp.buildAppLoop(echoDecoder, echoMessageResponse, initEmptyState)
}
