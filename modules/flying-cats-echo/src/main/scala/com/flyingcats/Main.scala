package com.github.flyingcats.echo

import cats.effect.{IO, IOApp}
import io.circe._, io.circe.parser._
import io.circe.syntax._
import cats.syntax.functor._
import com.github.flyingcats.common.{
  MaelstromApp,
  MaelstromMessage,
  MaelstromMessageBody,
  MaelstromMessageType
}
import com.github.flyingcats.common.Messenger.respond
import com.github.flyingcats.common.MaelstromMessageType._

case class EchoMessage(src: String, dest: String, body: EchoBody)
    extends MaelstromMessage

case class EchoBody(
    messageId: Int,
    echo: String
) extends MaelstromMessageBody {
  val messageType: MaelstromMessageType = Echo
}

case class EchoResponseMessage(
    src: String,
    dest: String,
    body: EchoResponseBody
) extends MaelstromMessage

case class EchoResponseBody(
    messageId: Int,
    inReplyTo: Int,
    echo: String
) extends MaelstromMessageBody {
  val messageType: MaelstromMessageType = Echo
}

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
            ("msg_id", Json.fromInt(a.body.messageId)),
            ("in_reply_to", Json.fromInt(a.body.inReplyTo)),
            ("echo", Json.fromString(a.body.echo))
          )
        )
      )
    }

  def decodeMessage: Decoder[EchoMessage] = new Decoder[EchoMessage] {
    def apply(c: HCursor): Decoder.Result[EchoMessage] =
      for {
        src <- c.downField("src").as[String]
        dest <- c.downField("dest").as[String]
        body <- c.downField("body").as[EchoBody]
      } yield EchoMessage(src, dest, body)
  }

  implicit def decodeBody: Decoder[EchoBody] = new Decoder[EchoBody] {
    def apply(c: HCursor): Decoder.Result[EchoBody] =
      for {
        messageId <- c.downField("msg_id").as[Int]
        echo <- c.downField("echo").as[String]
      } yield EchoBody(messageId, echo)
  }
}

object Main extends IOApp.Simple {

  val echoDecoder
      : PartialFunction[MaelstromMessageType, Either[Throwable, Decoder[
        MaelstromMessage
      ]]] = { case Echo => Right(EchoDecoders.decodeMessage.widen) }

  val echoMessageResponse: PartialFunction[(MaelstromMessage, _), IO[Unit]] = {
    case (EchoMessage(src, dest, ebody), _) =>
      respond(
        EchoResponseMessage(
          dest,
          src,
          EchoResponseBody(
            ebody.messageId,
            ebody.messageId,
            ebody.echo
          )
        ).asJson(EchoDecoders.encodeResponseMessage).noSpaces
      )
  }

  def initEmptyState(): Unit = ()

  def run: IO[Unit] =
    MaelstromApp.buildAppLoop(echoDecoder, echoMessageResponse, initEmptyState)
}
