package com.github.flyingcats.common

import cats.effect.{IO, Ref}
import io.circe._, io.circe.parser._
import fs2.{io, text}
import com.github.flyingcats.common.Messenger._

trait MaelstromMessage {
  val src: String
  val dest: String

  def respond[A](responseBody: A)(implicit bodyEncoder: Encoder[A]): IO[Unit] = sendMessage(
    Json
      .obj(
        ("src", Json.fromString(dest)),
        ("dest", Json.fromString(src)),
        ("body", bodyEncoder(responseBody))
      )
      .noSpaces
  )
}

object MaelstromApp {

  private def mainLoop[A <: NodeState](
      decoderLookup: PartialFunction[
        String,
        Decoder[MaelstromMessage]
      ],
      eventResponse: PartialFunction[
        (MaelstromMessage, Ref[IO, A]),
        IO[Unit]
      ],
      currentState: Ref[IO, A]
  ): IO[Unit] = {

    def getDecoder(messageType: String): IO[Decoder[MaelstromMessage]] =
      decoderLookup.lift(messageType) match {
        case None =>
          IO.raiseError(
            new RuntimeException(
              s"Received unexpected message type for the module: $messageType"
            )
          )
        case Some(decoder) => IO.pure(decoder)
      }

    def react(message: MaelstromMessage): IO[Unit] =
      eventResponse.lift((message, currentState)) match {
        case None =>
          IO.raiseError(
            new RuntimeException(
              s"node received unexpected message type: $message"
            )
          )
        case Some(value) => value
      }

    def decodeMessage(json: Json): IO[MaelstromMessage] = for {
      messageType <- IO.fromEither(
        json.hcursor.downField("body").downField("type").as[String]
      )
      decoder <- getDecoder(messageType)
      message <- IO.fromEither(json.as(decoder))
    } yield message

    def decodeMessageAndReact(messageString: String): IO[Unit] = for {
      json <- IO.fromEither(parse(messageString))
      message <- decodeMessage(json)
      _ <- react(message)
    } yield ()

    io.stdin[IO](4096)
      .through((text.utf8.decode))
      .through(text.lines)
      .evalTap(logReceived)
      .map(_.trim)
      .evalMap(decodeMessageAndReact)
      .compile
      .drain
  }

  def buildAppLoop[A <: NodeState](
      decoderLookup: PartialFunction[
        String,
        Decoder[MaelstromMessage]
      ],
      eventResponse: PartialFunction[
        (MaelstromMessage, Ref[IO, A]),
        IO[Unit]
      ],
      initState: String => A,
      backgroundActions: Ref[IO, A] => IO[Unit]
  ): IO[Unit] =
    NodeInit.initialise(initState).flatMap { iState =>
      IO.both(
        backgroundActions(iState),
        mainLoop[A](
          decoderLookup,
          eventResponse,
          iState
        )
      ).void
    }
}
