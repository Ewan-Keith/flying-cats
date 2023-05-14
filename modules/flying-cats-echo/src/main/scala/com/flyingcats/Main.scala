package com.github.flyingcats.echo

import cats.effect.{IO, IOApp}
import io.circe._
import cats.syntax.functor._
import com.github.flyingcats.common._

case class EchoMessage(src: String, dest: String, messageId: Int, echo: String)
    extends MaelstromMessage

case class EchoOkMessageBody(
    messageId: Int,
    inReplyTo: Int,
    echo: String
)

object EchoCodecs {

  implicit def encodeEchoOkMessageBody: Encoder[EchoOkMessageBody] =
    new Encoder[EchoOkMessageBody] {
      final def apply(a: EchoOkMessageBody): Json =
        Json.obj(
          ("type", Json.fromString("echo_ok")),
          ("msg_id", Json.fromInt(a.messageId)),
          ("in_reply_to", Json.fromInt(a.inReplyTo)),
          ("echo", Json.fromString(a.echo))
        )
    }

  implicit def decodeEchoMessage: Decoder[EchoMessage] =
    new Decoder[EchoMessage] {
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

  import EchoCodecs._

  val echoDecoder: PartialFunction[String, Decoder[MaelstromMessage]] = { case "echo" =>
    EchoCodecs.decodeEchoMessage.widen
  }

  val echoMessageResponse: PartialFunction[(MaelstromMessage, _), IO[Unit]] = {
    case (e: EchoMessage, _) =>
      e.respond(
        EchoOkMessageBody(
          e.messageId,
          e.messageId,
          e.echo
        )
      )
  }

  def run: IO[Unit] =
    MaelstromApp.buildAppLoop(echoDecoder, echoMessageResponse, () => ())
}
