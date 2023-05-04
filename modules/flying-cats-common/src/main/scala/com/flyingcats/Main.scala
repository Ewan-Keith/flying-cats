package com.github.flyingcats.common

import cats.effect.{IO, IOApp, Ref}
import io.circe._, io.circe.parser._
import io.circe.syntax._
import cats.syntax.functor._
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import fs2.{Stream, io, text}
import cats.effect
import java.nio.file.Path

trait MaelstromMessage {
  val src: String
  val dest: String
  val body: MaelstromMessageBody
}

trait MaelstromMessageBody {
  val messageType: MaelstromMessageType
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

  def mainLoop[A](
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
              s"node did not expect to receive message of type: ${message.body.messageType}"
            )
          )
        case Some(value) => value
      }

    val stream: Stream[IO, Unit] =
      io.stdin[IO](4096)
        .through((text.utf8Decode))
        .through(text.lines)
        .evalTap(logReceived)
        .map(_.trim)
        .evalMap { s => IO.fromEither(parse(s)) }
        .evalMap { j =>
          for {
            ts <- IO.fromEither(
              j.hcursor.downField("body").downField("type").as[String]
            )
            messageType <- IO.fromEither(
              MaelstromMessageType.getMessageType(ts)
            )
            d <- getDecoder(messageType)
            m <- IO.fromEither(j.as(d))
          } yield m
        }
        .evalMap(react)

    stream.compile.drain
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

  private val debugFileLocation: Option[Path] =
    None
  private val debugEnabled: Boolean = debugFileLocation.isDefined

  def debugLog(s: String): IO[Unit] =
    IO.fromOption(debugFileLocation)(
      new RuntimeException(
        "attempted to call debugLog method without a debugFileLocation set"
      )
    ).map(dfl =>
      IO {
        Files.write(dfl, s"$s\n".getBytes(), StandardOpenOption.APPEND)
      }
    )

  def logReceived(s: String): IO[Unit] =
    IO.whenA(debugEnabled)(debugLog(s"received: $s"))

  def respond(s: String): IO[Unit] =
    IO.whenA(debugEnabled)(debugLog(s"response: $s")) >> IO.println(s)
}
