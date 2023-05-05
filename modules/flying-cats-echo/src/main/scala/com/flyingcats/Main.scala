package com.github.flyingcats.echo

import cats.effect.{IO, IOApp}
import io.circe._
import io.circe.syntax._
import cats.syntax.functor._
import com.github.flyingcats.common._
import com.github.flyingcats.common.Messenger.respond
import com.github.flyingcats.common.MaelstromMessageType._

case class EchoMessage(src: String, dest: String, messageId: Int, echo: String)
    extends MaelstromMessage

case class EchoResponseMessage(
    src: String,
    dest: String,
    messageId: Int,
    inReplyTo: Int,
    echo: String
) extends MaelstromMessage

object EchoDecoders {

  def encodeResponseMessage: Encoder[EchoResponseMessage] =
    new Encoder[EchoResponseMessage] {
      final def apply(a: EchoResponseMessage): Json = Json.obj(
        ("src", Json.fromString(a.src)),
        ("dest", Json.fromString(a.dest)),
        (
          "body",
          Json.obj(
            ("type", Json.fromString("echo_ok")),
            ("msg_id", Json.fromInt(a.messageId)),
            ("in_reply_to", Json.fromInt(a.inReplyTo)),
            ("echo", Json.fromString(a.echo))
          )
        )
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
    case (EchoMessage(src, dest, messageId, echo), _) =>
      respond(
        EchoResponseMessage(
          dest,
          src,
          messageId,
          messageId,
          echo
        ).asJson(EchoDecoders.encodeResponseMessage).noSpaces
      )
  }

  def initEmptyState(): Unit = ()

  def run: IO[Unit] =
    MaelstromApp.buildAppLoop(echoDecoder, echoMessageResponse, initEmptyState)
}
