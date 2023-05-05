package com.github.flyingcats.common

import cats.effect.{IO, Ref}
import io.circe._, io.circe.parser._
import fs2.{io, text}
import com.github.flyingcats.common.Messenger._

trait MaelstromMessage {
  val src: String
  val dest: String

  def respond[A](body: A, encoder: Encoder[A]): IO[Unit] = sendMessage(
    Json
      .obj(
        ("src", Json.fromString(dest)),
        ("dest", Json.fromString(src)),
        ("body", encoder(body))
      )
      .noSpaces
  )
}

sealed trait MaelstromMessageType
object MaelstromMessageType {
  case object Init extends MaelstromMessageType
  case object InitOk extends MaelstromMessageType
  case object Echo extends MaelstromMessageType
  case object Generate extends MaelstromMessageType
  case object Topology extends MaelstromMessageType
  case object Broadcast extends MaelstromMessageType
  case object Read extends MaelstromMessageType

  def getMessageType(
      typeString: String
  ): Either[Throwable, MaelstromMessageType] =
    typeString match {
      case "init"      => Right(Init)
      case "echo"      => Right(Echo)
      case "generate"  => Right(Generate)
      case "topology"  => Right(Topology)
      case "broadcast" => Right(Broadcast)
      case "read"      => Right(Read)
      case s =>
        Left(new RuntimeException(s"unrecognised Maelstrom message type: $s"))
    }
}

object MaelstromApp {

  private def mainLoop[A](
      decoderLookup: PartialFunction[
        MaelstromMessageType,
        Either[Throwable, Decoder[
          MaelstromMessage
        ]]
      ],
      eventResponse: PartialFunction[
        (MaelstromMessage, Ref[IO, NodeState[A]]),
        IO[Unit]
      ],
      currentState: Ref[IO, NodeState[A]]
  ): IO[Unit] = {

    def getDecoder(mtype: MaelstromMessageType): IO[Decoder[MaelstromMessage]] =
      decoderLookup.lift(mtype) match {
        case None =>
          IO.raiseError(
            new RuntimeException(
              s"Received unexpected message type for the module: $mtype"
            )
          )
        case Some(value) => IO.fromEither(value)
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
      typeString <- IO.fromEither(
        json.hcursor.downField("body").downField("type").as[String]
      )
      messageType <- IO.fromEither(
        MaelstromMessageType.getMessageType(typeString)
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

  def buildAppLoop[A](
      decoderLookup: PartialFunction[
        MaelstromMessageType,
        Either[Throwable, Decoder[
          MaelstromMessage
        ]]
      ],
      eventResponse: PartialFunction[
        (MaelstromMessage, Ref[IO, NodeState[A]]),
        IO[Unit]
      ],
      initState: () => A
  ): IO[Unit] =
    NodeInit.initialise(initState).flatMap { iState =>
      mainLoop(
        decoderLookup,
        eventResponse,
        iState
      )
    }

}
