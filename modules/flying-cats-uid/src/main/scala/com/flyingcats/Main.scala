package com.github.flyingcats.uid

import cats.effect.{IO, IOApp}
import io.circe._
import cats.syntax.functor._
import com.github.flyingcats.common._
import com.github.flyingcats.common.MaelstromMessageType._

case class GenerateMessage(src: String, dest: String, messageId: Int)
    extends MaelstromMessage

case class GenerateResponse(
    messageId: Int,
    inReplyTo: Int,
    id: String
)

object GenerateDecoders {

  def encodeResponseBody: Encoder[GenerateResponse] =
    new Encoder[GenerateResponse] {
      final def apply(a: GenerateResponse): Json =
        Json.obj(
          ("type", Json.fromString("generate_ok")),
          ("msg_id", Json.fromInt(a.messageId)),
          ("in_reply_to", Json.fromInt(a.inReplyTo)),
          ("id", Json.fromString(a.id))
        )
    }

  def decodeMessage: Decoder[GenerateMessage] = new Decoder[GenerateMessage] {
    def apply(c: HCursor): Decoder.Result[GenerateMessage] =
      for {
        src <- c.downField("src").as[String]
        dest <- c.downField("dest").as[String]
        messageId <- c.downField("body").downField("msg_id").as[Int]
      } yield GenerateMessage(src, dest, messageId)
  }
}

object Main extends IOApp.Simple {

  val generateDecoder
      : PartialFunction[MaelstromMessageType, Either[Throwable, Decoder[
        MaelstromMessage
      ]]] = { case Generate => Right(GenerateDecoders.decodeMessage.widen) }

  val generateMessageResponse
      : PartialFunction[(MaelstromMessage, _), IO[Unit]] = {
    case (g: GenerateMessage, _) =>
      g.respond(
        GenerateResponse(
          g.messageId,
          g.messageId,
          java.util.UUID.randomUUID.toString
        ),
        GenerateDecoders.encodeResponseBody
      )
  }

  def initEmptyState(): Unit = ()

  def run: IO[Unit] =
    MaelstromApp.buildAppLoop(
      generateDecoder,
      generateMessageResponse,
      initEmptyState
    )
}
