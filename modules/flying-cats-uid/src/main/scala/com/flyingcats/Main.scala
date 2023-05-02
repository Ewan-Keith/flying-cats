package com.github.flyingcats.uid

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
import com.github.flyingcats.common.MaelstromMessageType._

case class GenerateMessage(src: String, dest: String, body: GenerateBody)
    extends MaelstromMessage

case class GenerateBody(
    messageId: Int
) extends MaelstromMessageBody {
  val messageType: MaelstromMessageType = Generate
}

case class GenerateResponseMessage(
    src: String,
    dest: String,
    body: GenerateResponseBody
) extends MaelstromMessage

case class GenerateResponseBody(
    messageId: Int,
    inReplyTo: Int,
    id: String
) extends MaelstromMessageBody {
  val messageType: MaelstromMessageType = Generate
}

object GenerateDecoders {

  def encodeResponseMessage: Encoder[GenerateResponseMessage] =
    new Encoder[GenerateResponseMessage] {
      final def apply(a: GenerateResponseMessage): Json = Json.obj(
        ("src", Json.fromString(a.src)),
        ("dest", Json.fromString(a.dest)),
        (
          "body",
          Json.obj(
            ("type", Json.fromString("generate_ok")),
            ("msg_id", Json.fromInt(a.body.messageId)),
            ("in_reply_to", Json.fromInt(a.body.inReplyTo)),
            ("id", Json.fromString(a.body.id))
          )
        )
      )
    }

  def decodeMessage: Decoder[GenerateMessage] = new Decoder[GenerateMessage] {
    def apply(c: HCursor): Decoder.Result[GenerateMessage] =
      for {
        src <- c.downField("src").as[String]
        dest <- c.downField("dest").as[String]
        body <- c.downField("body").as[GenerateBody]
      } yield GenerateMessage(src, dest, body)
  }

  implicit def decodeBody: Decoder[GenerateBody] = new Decoder[GenerateBody] {
    def apply(c: HCursor): Decoder.Result[GenerateBody] =
      for {
        messageId <- c.downField("msg_id").as[Int]
      } yield GenerateBody(messageId)
  }
}

object Main extends IOApp.Simple {

  def getUidDecoder(
      messageType: MaelstromMessageType
  ): Either[Throwable, Decoder[MaelstromMessage]] = messageType match {
    case Generate => Right(GenerateDecoders.decodeMessage.widen)
    case v =>
      Left(
        new RuntimeException(
          s"Received unexpected message type for the generate module: $v"
        )
      )
  }

  def generateMessageResponse(echo: MaelstromMessage): IO[Unit] = echo match {
    case GenerateMessage(src, dest, gbody) =>
      IO.println(
        GenerateResponseMessage(
          dest,
          src,
          GenerateResponseBody(
            gbody.messageId,
            gbody.messageId,
            java.util.UUID.randomUUID.toString
          )
        ).asJson(GenerateDecoders.encodeResponseMessage).noSpaces
      )
    case e =>
      IO.raiseError(
        new RuntimeException(
          s"uid node did not expect to receive message of type: $e"
        )
      )
  }

  def run: IO[Unit] =
    MaelstromApp.buildAppLoop(getUidDecoder, generateMessageResponse)
}
